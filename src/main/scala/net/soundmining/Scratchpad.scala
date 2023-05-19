package net.soundmining

import net.soundmining.Generative.{pickItems, randomIntRange, randomRange}
import net.soundmining.Harmonics._
import net.soundmining.Interactive._
import net.soundmining.modular.ModularInstrument.{ControlInstrument, StaticAudioBusInstrument}
import net.soundmining.modular.ModularSynth._
import net.soundmining.modular.{ModularSynth, SynthPlayer}
import net.soundmining.synth.Instrument.{EFFECT, TAIL_ACTION}
import net.soundmining.synth.MidiUtils.midiToFreq
import net.soundmining.synth.SuperColliderClient.{clearSched, deepFree, loadDir}
import net.soundmining.synth.Utils.absoluteTimeToMillis
import net.soundmining.synth._

import scala.util.Random

/*
* Different things
*
* Patch system. Where you can associate and play a "patch" via keyboard.
*
* Make a UI which can control aspects of the sound. Write a UI in Supercollider
* and communicate with it via osc (much like the midi).
*
* The UI could be generic and you could control and assign things from
* the "patch" in scala and set it up via osc.
*
* Also have some useful stuff such as meters and a clock in the there.
* Also be able to send and visualize different notes. With time.
* */
object Scratchpad {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 2, bufferedPlayback = false)
  val patchChooser = PatchChooser()
  var patchPlayback: PatchPlayback = PatchPlayback(patch = InteractiveBankOfResonators, client = client)
  val superColliderReceiver: SuperColliderReceiver = SuperColliderReceiver(patchPlayback)

  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()
    superColliderReceiver.start()

    this.patchChooser.patchChooser = Map(
      36 -> SawPatch,
      37 -> WhiteNoisePluck,
      38 -> PinkNoisePluck,
      39 -> DustPatch,
      40 -> BankPatch)
  }

  val OVERALL_AMP = 4


  def makeAttackCurve(min: Double, max: Double)(implicit random: Random): Seq[Double] = {
    val curve = randomRange(min, max)
    Seq(curve, curve * -1)
  }

  /**
   * Nice fat bass. Warm
   */
  object SawPatch extends Patch {
    def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val pitch = midiToFreq(key)
      val amp = (velocity / 127.0) * OVERALL_AMP
      println(s"start $start key $key freq $pitch velocity $velocity")

      synthPlayer()
        .saw(staticControl(pitch), relativePercControl(0.001, amp, 0.1, Left(Seq(-4, -4))))
        .lowPass(staticControl(pitch * 1))
        .pan(staticControl(0))
        .playWithDuration(start, 3)

      synthPlayer()
        .saw(staticControl(pitch * 1.01), relativePercControl(0.001, amp, 0.1, Left(Seq(-4, -4))))
        .highPass(staticControl(pitch * 5))
        .am(staticControl(pitch * 2))
        .pan(staticControl(0))
        .playWithDuration(start, 3)
    }
  }

  /*
  * Much sharper with white noise. Slight detune of the filtered
  * versions make it more shimmering
  * */
  object WhiteNoisePluck extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val pitch = midiToFreq(key)
      val amp = (velocity / 127.0) * OVERALL_AMP
      val duration = 5.0

      println(s"start $start key $key freq $pitch velocity $velocity")

      synthPlayer()
        .whiteNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .lowPass(staticControl(pitch / 2))
        .monoComb(1.0 / ((pitch / 2.0) * 1.01), duration, staticControl(1))
        .pan(staticControl(0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .whiteNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .highPass(staticControl(pitch))
        .monoComb(1.0 / (pitch * 0.99), duration, staticControl(1))
        .pan(staticControl(-0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .whiteNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .monoComb(1.0 / pitch, duration, staticControl(1))
        .pan(staticControl(0))
        .playWithDuration(start, duration)
    }
  }

  /*
  * Pink noise make it more mellow
  * */
  object PinkNoisePluck extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val pitch = midiToFreq(key)
      val amp = (velocity / 127.0) * OVERALL_AMP
      println(s"start $start key $key freq $pitch velocity $velocity")
      val duration = 5.0

      synthPlayer()
        .pinkNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .lowPass(staticControl(pitch / 2))
        .monoComb(1.0 / ((pitch / 2.0) * 1.01), duration, staticControl(1))
        .pan(staticControl(0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .pinkNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .highPass(staticControl(pitch))
        .monoComb(1.0 / (pitch * 0.99), duration, staticControl(1))
        .pan(staticControl(-0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .pinkNoise(percControl(0, amp, 0.01, Left(Seq(-4, -4))), Some(0.001))
        .monoComb(1.0 / pitch, duration, staticControl(1))
        .pan(staticControl(0))
        .playWithDuration(start, duration)
    }
  }

  object DustPatch extends Patch {

    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val pitch = midiToFreq(key)
      val amp = (velocity / 127.0) * OVERALL_AMP
      println(s"start $start key $key freq $pitch velocity $velocity")
      val duration = 13.0

      synthPlayer()
        .dust(percControl(0, amp, 0.5, Left(Seq(-4, -4))),
          staticControl(500))
        .lowPass(staticControl(pitch * 4))
        .pan(staticControl(0))
        .playWithDuration(start, duration)
    }
  }

  object BankPatch extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val pitch = midiToFreq(key)
      val amp = (velocity / 127.0) * 0.007
      val duration = 8
      synthPlayer()
        .pinkNoise(percControl(0, amp, 0.5, Left(Seq(-4, -4))))
        .bankOfResonators(Seq(pitch * 6, pitch * 7), Seq(1, 1), Seq(5.1, 5.9))
        .pan(staticControl(0))
        .playWithDuration(start, duration)
    }
  }

  case class PatchChooser(chooseDevice: String = "PAD:nanoPAD2", playDevice: String = "KEYBOARD:microKEY2", var patchChooser: Map[Int, Patch] = Map()) extends Patch {
    private var currentPatch: Patch = EmptyPatch

    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      device match {
        case device if device == chooseDevice =>
          patchChooser.get(key) match {
            case Some(patch) => {
              currentPatch = patch
              println(s"patch is $key ${currentPatch.getClass.getSimpleName.replace("$", "")}")
            }
            case None => println(s"$key is not mapped to a patch")
          }
        case device if device == playDevice =>
          currentPatch.noteHandle(start, key, velocity, device)
        case _ =>
      }
    }
  }

  case class SpectArgument(var fundamental: String = "c2", var firstPartial: String = "c3", var scaleSize: Int = 12) extends InteractiveArgument[Seq[Seq[Double]]] {
    def apply(newFundamental: String, newFirstPartial: String, newScaleSize: Int): Unit = {
      fundamental = newFundamental
      firstPartial = newFirstPartial
      scaleSize = newScaleSize
    }

    override def apply(patchArguments: PatchArguments): Seq[Seq[Double]] = {
      val spect = SpectrumScale(fundamental, firstPartial, scaleSize)
      val scale = spect.makeEqualTemperedScale(48)
      spect.makeSpectrums(scale)
    }
    override def describeCurrent(): String = s"Spect $fundamental $firstPartial $scaleSize"
  }

  case class OvertoneAmpsArgument(var sieve: Sieve, var ampFunction: Int => Double) extends InteractiveArgument[OvertoneAmps] {
    def apply(newSieve: Sieve, newAmpFunction: Int => Double): Unit = {
      sieve = newSieve
      ampFunction = newAmpFunction
    }

    def apply(newSieve: Sieve): Unit =
      sieve = newSieve

    def apply(patchArguments: PatchArguments): OvertoneAmps =
      OvertoneAmps(i => sieve.isSieve(i), ampFunction)

    override def describeCurrent(): String = s"over tone amps sieve $sieve and function $ampFunction"
  }

  case class RingtimesArgument(var ringtimesFunc: Int => Double = _ => 1.0) extends InteractiveArgument[OvertoneRingtimes] {
    def apply(newRingtimeFunc: Int => Double): Unit =
      ringtimesFunc = newRingtimeFunc

    def apply(patchArguments: PatchArguments): OvertoneRingtimes =
      OvertoneRingtimes(ringtimesFunc)

    override def describeCurrent(): String = s"ring times function $ringtimesFunc"
  }

  case class ControlInstrumentArgument(var func: PatchArguments => ControlInstrument) extends InteractiveArgument[ControlInstrument] {
    def apply(patchArguments: PatchArguments): ControlInstrument =
      func(patchArguments)

    def apply(newFunc: PatchArguments => ControlInstrument): Unit =
      func = newFunc

    override def describeCurrent(): String = s"Control function $func"
  }

  object InteractiveSawPatch extends InteractivePatch {

    val duration: RandomRangeArgument = RandomRangeArgument(3, 5)
    val pan: RandomRangeArgument = RandomRangeArgument()

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      val amp = patchArguments.amp * OVERALL_AMP

      synthPlayer()
        .saw(staticControl(patchArguments.pitch), relativePercControl(0.001, amp, 0.1, Left(Seq(-4, -4))))
        .lowPass(staticControl(patchArguments.pitch))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments))

      synthPlayer()
        .saw(staticControl(patchArguments.pitch * 1.01), relativePercControl(0.001, amp, 0.1, Left(Seq(-4, -4))))
        .highPass(staticControl(patchArguments.pitch * 5))
        .am(staticControl(patchArguments.pitch * 2))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments))
    }
  }

  def playCleanEffect(startTime: Double, effectAudioBus: StaticAudioBusInstrument, ampValue: Double, duration: Double, output: Int): Unit = {
    val cleanAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.1, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val clean = ModularSynth.stereoVolume(effectAudioBus, cleanAmp(ampValue))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    clean.getOutputBus.staticBus(output)
    val graph = clean.buildGraph(startTime, duration, clean.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def playEffect(startTime: Double, duration: Double, effectOut: Int): StaticAudioBusInstrument = {
    val effectAudioBus = staticAudioBus(2)
    effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

    playCleanEffect(startTime, effectAudioBus, 0.5, duration, 0)

    val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val comb = ModularSynth.stereoComb(effectAudioBus, reverbAmp(0.5), 5.0, 15)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val reverb = ModularSynth.stereoHallReverb(comb, reverbAmp(0.5), rt60 = 6, stereo = 0.6, lowRatio = 0.8, hiRatio = 0.3)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    reverb.getOutputBus.staticBus(effectOut)
    val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

    effectAudioBus
  }


  def playShortEffect1(startTime: Double, duration: Double, effectOut: Int): StaticAudioBusInstrument = {
    val effectAudioBus = staticAudioBus(2)
    effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

    playCleanEffect(startTime, effectAudioBus, 0.5, duration, 0)

    val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.35, damp = 0.8)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)


    reverb.getOutputBus.staticBus(effectOut)
    val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

    effectAudioBus
  }

  def playShortEffect2(startTime: Double, duration: Double, effectOut: Int): StaticAudioBusInstrument = {
    val effectAudioBus = staticAudioBus(2)
    effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

    playCleanEffect(startTime, effectAudioBus, 0.5, duration, 0)

    val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.6, damp = 0.7)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)


    reverb.getOutputBus.staticBus(effectOut)
    val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

    effectAudioBus
  }

  def playShortEffect3(startTime: Double, duration: Double, effectOut: Int): StaticAudioBusInstrument = {
    val effectAudioBus = staticAudioBus(2)
    effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

    playCleanEffect(startTime, effectAudioBus, 0.5, duration, 0)

    val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.8, damp = 0.3)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)


    reverb.getOutputBus.staticBus(effectOut)
    val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

    effectAudioBus
  }

  // scaleSpect("c2", "hess3", 10)
  // fiss diss giss, c diss aisss
  // scaleSpect("e2", "fiss3", 7)
  // fiss giss, c d
  // overtoneAmps(SimpleSieve(2, 1), ampFunc1)
  // overtoneAmps(SimpleSieve(2, 1), ampFunc2)
  // overtoneAmps(SimpleSieve(3, 0), ampFunc1)
  // overtoneAmps(SimpleSieve(3, 0), ampFunc2)

  // c2 hess 10
  // chords cell chords
  // ciss + fiss, giss, a + c ciss diss
  // c + g aiss h + ciss d e
  // diss + f aiss h +  ciss f fiss
  // e + g h c + e fiss g

  // e2 fiss 3 7
  // fiss giss h + g a aiss
  // + fiss giss h d fiss + g a aiss diss f


  object InteractiveBankOfOsc extends InteractivePatch {

    val scaleSpect = ChooseArgument(choices = Seq(
      SpectArgument("c2", "hess3", 10),
      SpectArgument("e2", "fiss3", 7)))
    val pan: RandomRangeArgument = RandomRangeArgument(-0.66, 0.66)
    val duration: RandomRangeArgument = RandomRangeArgument(5, 8)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)
    val overtoneAmps = ChooseArgument(choices = Seq(
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))
    val phasesZero = OvertonePhases()

    var effectChannel: StaticAudioBusInstrument = _

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      if(effectChannel == null) {
        effectChannel = playEffect(patchArguments.start, 60.0 * 6.0, 0)
      }
      val note = (patchArguments.key - 36) % 48
      val amp = patchArguments.amp * 0.81

      synthPlayer()
        .bankOfOsc(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), phasesZero.phases())
        .monoVolume(sineControl(0, amp))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments), outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false)
        //.playWithDuration(patchArguments.start, duration(patchArguments))
    }
  }

  object InteractiveBankOfOscWithRing extends InteractivePatch {

    val scaleSpect = ChooseArgument(choices = Seq(
      SpectArgument("c2", "hess3", 10),
      SpectArgument("e2", "fiss3", 7)))
    val pan: RandomRangeArgument = RandomRangeArgument(-0.66, 0.66)
    val duration: RandomRangeArgument = RandomRangeArgument(5, 8)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)
    val overtoneAmps = ChooseArgument(choices = Seq(
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))
    val phasesZero = OvertonePhases()

    val ringNote: IntArgument = IntArgument()

    var effectChannel: StaticAudioBusInstrument = _

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      if (effectChannel == null) {
        effectChannel = playEffect(patchArguments.start, 60.0 * 6.0, 0)
      }
      val note = (patchArguments.key - 36) % 48
      val amp = patchArguments.amp * 0.81

      val spect = scaleSpect(patchArguments)(note)
      synthPlayer()
        .bankOfOsc(spect, overtoneAmps(patchArguments).amps(), phasesZero.phases())
        .monoVolume(sineControl(0, amp))
        .ring(staticControl(spect(ringNote(patchArguments))))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments), outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false)
    }
  }

  object InteractiveBankOfResonators extends InteractivePatch {

    val scaleSpect = ChooseArgument(choices = Seq(
      SpectArgument("c2", "hess3", 10),
      SpectArgument("e2", "fiss3", 7)))
    val pan: RandomRangeArgument = RandomRangeArgument(-0.66, 0.66)
    val duration: RandomRangeArgument = RandomRangeArgument(5, 8)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)
    val overtoneAmps = ChooseArgument(choices = Seq(
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    val overtoneRingtimes = ChooseArgument(choices = Seq(
      RingtimesArgument(_ => 0.1),
      RingtimesArgument(_ => 0.05),
      RingtimesArgument(_ => 0.2)))

    var effectChannel: StaticAudioBusInstrument = _

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      if (effectChannel == null) {
        effectChannel = playEffect(patchArguments.start, 60.0 * 6.0, 0)
      }
      val note = (patchArguments.key - 36) % 48
      val amp = patchArguments.amp * 0.81

      synthPlayer()
        .pinkNoise(sineControl(0, amp))
        .bankOfResonators(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), overtoneRingtimes(patchArguments).ringtimes())
        .monoVolume(sineControl(0, amp))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments), outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false)
    }
  }

  object InteractiveBankOfResonatorsWithRing extends InteractivePatch {

    val scaleSpect = ChooseArgument(choices = Seq(
      SpectArgument("c2", "hess3", 10),
      SpectArgument("e2", "fiss3", 7)))
    val pan: RandomRangeArgument = RandomRangeArgument(-0.66, 0.66)
    val duration: RandomRangeArgument = RandomRangeArgument(5, 8)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)
    val overtoneAmps = ChooseArgument(choices = Seq(
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    val overtoneRingtimes = ChooseArgument(choices = Seq(
      RingtimesArgument(_ => 0.1),
      RingtimesArgument(_ => 0.05),
      RingtimesArgument(_ => 0.2)))

    val ringNote: IntArgument = IntArgument()

    var effectChannel: StaticAudioBusInstrument = _

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      if (effectChannel == null) {
        effectChannel = playEffect(patchArguments.start, 60.0 * 6.0, 0)
      }
      val note = (patchArguments.key - 36) % 48
      val amp = patchArguments.amp * 0.81

      val spect = scaleSpect(patchArguments)(note)

      synthPlayer()
        .pinkNoise(sineControl(0, amp))
        .bankOfResonators(spect, overtoneAmps(patchArguments).amps(), overtoneRingtimes(patchArguments).ringtimes())
        .monoVolume(sineControl(0, amp))
        .ring(staticControl(spect(ringNote(patchArguments))))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments), outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false)
    }
  }


  object InteractivePulseBankOfResonators extends InteractivePatch {

    val scaleSpect = SpectArgument("c2", "hess3", 10)

    val pan: RandomRangeArgument = RandomRangeArgument(-0.66, 0.66)
    val duration: RandomRangeArgument = RandomRangeArgument(8, 13)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)

    val overtoneAmps = OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2)

    /*
    val overtoneAmps = ChooseArgument(choices = Seq(
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))
*/
    // _ => 0.05
    val overtoneRingtimes = RingtimesArgument(_ => 0.3)
    /*
    val overtoneRingtimes = ChooseArgument(choices = Seq(
      RingtimesArgument(_ => 0.1),
      RingtimesArgument(_ => 0.05),
      RingtimesArgument(_ => 0.2)))
*/

    // 10 13, 21 10, 5 7
    val pulseSpeed1: IntArgument = IntArgument(30)
    val pulseSpeed2: IntArgument = IntArgument(32)
    var effectChannel: StaticAudioBusInstrument = _

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      //if (effectChannel == null) {
      //  effectChannel = playEffect(patchArguments.start, 60.0 * 6.0, 0)
      //}
      val note = (patchArguments.key - 36) % 48
      val amp = patchArguments.amp * 0.2

      val spect = SpectrumScale("c2", "hess3", 10)
      val scale = spect.makeLowerEqualTemperedScale(40)

      println(scale.zipWithIndex)
      synthPlayer()
        .pulse(staticControl(scale(pulseSpeed1(patchArguments))), sineControl(0, amp))
        .pulse(staticControl(scale(pulseSpeed2(patchArguments))), sineControl(0, amp))
        .mix(sineControl(0, amp))
        .bankOfResonators(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), overtoneRingtimes(patchArguments).ringtimes())
        .monoVolume(sineControl(0, amp))
        .pan(staticControl(pan(patchArguments)))
        .playWithDuration(patchArguments.start, duration(patchArguments))
    }
  }


  object InteractivePulseBankOfResonators2 extends InteractivePatch {

    def panLine(distance: Double): (Double, Double) = {
      val randomDistance = distance * randomRange(0.8, 1.2)
      val startingPoint = randomRange(-0.75, 0.75)
      val direction = if(random.nextBoolean()) 1.0 else -1.0
      val desiredEndpoint = (startingPoint + randomDistance) * direction
      val endPoint = math.max(math.min(desiredEndpoint, 1.0), -1.0)

      println(s"Random path with distance $distance ($startingPoint, $endPoint)")
      (startingPoint, endPoint)
    }

    val scaleSpect = SpectArgument("c2", "hess3", 10)

    val oddSieve = SimpleSieve(2, 0)
    val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
    val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)

    val overtoneAmps = ChooseArgument(choices = Seq(
      // Note1 (long)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    val spect = SpectrumScale("c2", "hess3", 10)
    val scale = spect.makeLowerEqualTemperedScale(40)

    var effectChannel1: StaticAudioBusInstrument = _
    var effectChannel2: StaticAudioBusInstrument = _
    var effectChannel3: StaticAudioBusInstrument = _

    var note = ChooseArgument(choices = Seq(StaticArgument(v => note1(v)), StaticArgument(v => note2(v)), StaticArgument(v => note3(v))))

    def extendedNoteHandle(patchArguments: PatchArguments): Unit = {
      if (effectChannel1 == null) {
        effectChannel1 = playShortEffect1(patchArguments.start, 60.0 * 6.0, 0)
        effectChannel2 = playShortEffect2(patchArguments.start, 60.0 * 6.0, 0)
        effectChannel3 = playShortEffect3(patchArguments.start, 60.0 * 6.0, 0)
      }

      note.apply(patchArguments)(patchArguments)
      //note2(patchArguments)
    }

    def note1(patchArguments: PatchArguments): Unit = {
      val volume1 = (p: PatchArguments) => {
        val values = (
          0.0,
          p.amp * randomRange(0.05, 0.25),
          p.amp * randomRange(0.05, 0.25),
          0.0)
        val times = (
          randomRange(0.25, 0.35),
          randomRange(0.35, 0.45),
          randomRange(0.25, 0.35))
        println(s"Time func $values $times")
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulse1 = (patchArguments: PatchArguments) => {
        val values = (
          scale(randomIntRange(13, 17)),
          scale(randomIntRange(18, 22)),
          scale(randomIntRange(23, 27)),
          scale(randomIntRange(17, 21)))

        val times = (randomRange(0.25, 0.35), randomRange(0.35, 0.45), randomRange(0.25, 0.35))
        println(s"Pulse func $values, $times")
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulse1reverse = (patchArguments: PatchArguments) => {

        val values = (
          scale(randomIntRange(17, 21)),
          scale(randomIntRange(23, 27)),
          scale(randomIntRange(18, 22)),
          scale(randomIntRange(13, 17)))

        val times = (randomRange(0.25, 0.35), randomRange(0.35, 0.45), randomRange(0.25, 0.35))

        println(s"Reverse Pulse func $values, $times")
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulse1Choose = (patchArguments: PatchArguments) => {
        pickItems(Seq(pulse1, pulse1reverse), 1).head(patchArguments)
      }

      val overtoneRingtimes1 = OvertoneRingtimes((_: Int) => {
        val ringtime = randomRange(0.05, 0.15)
        ringtime
      })

      val note = (patchArguments.key - 36) % 48

      val duration = randomRange(8, 13)

      val (panStart, panEnd) = panLine(0.7)

      synthPlayer()
        .pulse(pulse1Choose(patchArguments), volume1(patchArguments))
        .bankOfResonators(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), overtoneRingtimes1.ringtimes())
        .monoVolume(volume1(patchArguments))
        .pan(lineControl(panStart, panEnd))
        //.pan(pan(patchArguments))
        // .playWithDuration(patchArguments.start, duration)
        .playWithDuration(patchArguments.start, duration, outputBus = effectChannel1.getOutputBus.busValue.get, realOutput = false)
    }

    def note2(patchArguments: PatchArguments): Unit = {
      val volume1 = (p: PatchArguments) => {
        val volume = p.amp * randomRange(0.05, 0.15)
        println(s"Volume $volume")
        sineControl(0, volume)
      }

      val pulse1 = (patchArguments: PatchArguments) => {
        val start = scale(randomIntRange(18, 22))
        val peak = scale(randomIntRange(23, 27))
        println(s"Pulse start $start peak $peak")
        sineControl(start, peak)
      }

      val overtoneRingtimes1 = OvertoneRingtimes((_: Int) => {
        val ringtime = randomRange(0.1, 0.25)
        ringtime
      })

      val note = (patchArguments.key - 36) % 48

      val duration = randomRange(3, 5)

      val (panStart, panEnd) = panLine(0.5)

      synthPlayer()
        .pulse(pulse1(patchArguments), volume1(patchArguments))
        .bankOfResonators(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), overtoneRingtimes1.ringtimes())
        .monoVolume(volume1(patchArguments))
        //.pan(pan(patchArguments))
        .pan(lineControl(panStart, panEnd))
        //.playWithDuration(patchArguments.start, duration)
        .playWithDuration(patchArguments.start, duration, outputBus = effectChannel2.getOutputBus.busValue.get, realOutput = false)

    }


    def note3(patchArguments: PatchArguments): Unit = {
      val volume1 = (p: PatchArguments) => {
        val volume = p.amp * randomRange(0.05, 0.15)
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        println(s"Volume $volume levels $times")
        twoBlockControl((0, volume, 0), times, (0, 0))
      }

      val pulse1 = (patchArguments: PatchArguments) => {
        // 29 33
        // 34 37
        val start = scale(randomIntRange(34, 37))
        val peak = scale(randomIntRange(29, 33))
        val end = scale(randomIntRange(34, 37))
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        println(s"Pulse ($start $peak $end) times $times")
        twoBlockControl((start, peak, end), times, (0, 0))
      }

      val overtoneRingtimes1 = OvertoneRingtimes((_: Int) => {
        val ringtime = randomRange(0.15, 0.36)
        ringtime
      })

      val note = (patchArguments.key - 36) % 48

      val duration = randomRange(13, 21)

      val (panStart, panEnd) = panLine(1.3)

      synthPlayer()
        .pulse(pulse1(patchArguments), volume1(patchArguments))
        .bankOfResonators(scaleSpect(patchArguments)(note), overtoneAmps(patchArguments).amps(), overtoneRingtimes1.ringtimes())
        .monoVolume(volume1(patchArguments))
        .pan(lineControl(panStart, panEnd))
        //.playWithDuration(patchArguments.start, duration)
        .playWithDuration(patchArguments.start, duration, outputBus = effectChannel3.getOutputBus.busValue.get, realOutput = false)
    }
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.send(clearSched())
    client.send(deepFree(0))
    client.stop
    this.superColliderReceiver.stop()
  }
}
