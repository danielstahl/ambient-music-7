package net.soundmining

import net.soundmining.Generative.randomRange
import net.soundmining.synth.MidiUtils.midiToFreq
import net.soundmining.synth.Patch

import scala.util.Random

object Interactive {
  case class PatchArguments(start: Double, key: Int, velocity: Int, device: String, note: Int, pitch: Double, amp: Double, octave: Int)

  abstract case class InteractivePatch() extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val patchArguments = PatchArguments(
        start = start,
        key = key,
        velocity = velocity,
        device = device,
        note = key % 12,
        pitch = midiToFreq(key),
        amp = velocity / 127.0,
        octave = (key / 12) - 1)
      extendedNoteHandle(patchArguments)
    }

    def extendedNoteHandle(patchArguments: PatchArguments): Unit
  }

  trait InteractiveArgument[T] {
    def apply(patchArguments: PatchArguments): T

    def describeCurrent(): String

    def printCurrent(): Unit = println(describeCurrent())
  }

  case class DoubleArgument(var value: Double = 0.0) extends InteractiveArgument[Double] {
    def apply(newValue: Double): Unit = value = newValue

    override def apply(patchArguments: PatchArguments): Double = value

    override def describeCurrent(): String = s"Double $value"
  }

  case class IntArgument(var value: Int = 0) extends InteractiveArgument[Int] {
    def apply(newValue: Int): Unit = value = newValue

    override def apply(patchArguments: PatchArguments): Int = value

    override def describeCurrent(): String = s"Int $value"
  }

  case class RandomRangeArgument(var lower: Double = 0.0, var upper: Double = 0.0)(implicit random: Random) extends InteractiveArgument[Double] {
    def apply(newLower: Double, newUpper: Double): Unit = {
      lower = newLower
      upper = newUpper
    }

    def apply(patchArguments: PatchArguments): Double =
      randomRange(lower, upper)

    override def describeCurrent(): String = s"random range $lower - $upper"
  }

  case class StaticArgument[V](var value: V) extends InteractiveArgument[V] {
    def apply(newValue: V): Unit = value = newValue

    override def apply(patchArguments: PatchArguments): V = value

    override def describeCurrent(): String =
      s"Current value is $value"
  }

  case class ChooseArgument[V](choices: Seq[InteractiveArgument[V]], var current: Int = 0) extends InteractiveArgument[V] {
    def next(): Unit =
      apply(current + 1)

    def prev(): Unit =
      apply(if (current == 0) choices.length else current - 1)

    def apply(newCurrent: Int): Unit =
      current = newCurrent % choices.length

    override def apply(patchArguments: PatchArguments): V =
      choices(current)(patchArguments)

    override def describeCurrent(): String = s"choise ${choices(current).describeCurrent()}"
  }
}
