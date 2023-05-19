package net.soundmining

object Harmonics {

  case class OvertoneAmps(enable: Int => Boolean = _ => true, ampValue: Int => Double = i => 1.0 / (i + 1.0)) {
    val EVERY_SECOND_ENABLE = (i: Int) => i % 2 == 0

    def amps(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => if (enable(i)) ampValue(i) else 0.0
        }
  }

  case class OvertoneRingtimes(ringTime: Int => Double = _ => 1.0) {
    def ringtimes(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => ringTime(i)
        }
  }

  case class OvertonePhases(phase: Int => Double = _ => 0.0) {
    def phases(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => phase(i)
        }
  }
}
