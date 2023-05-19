package net.soundmining

import net.soundmining.Generative.{pickItems, randomIntRange, randomRange}
import net.soundmining.modular.ModularSynth.{controlMix, lineControl, sineControl, staticAudioBus, threeBlockControl, twoBlockControl}
import net.soundmining.synth.SuperColliderClient.{clearSched, deepFree, groupHead, groupTail, loadDir}
import net.soundmining.modular.{ModularSynth, SynthPlayer}
import net.soundmining.synth.{EmptyPatch, Instrument, PatchPlayback, SuperColliderClient, SuperColliderReceiver}
import Harmonics._
import net.soundmining.Interactive.{ChooseArgument, InteractivePatch, StaticArgument}
import net.soundmining.Scratchpad.InteractivePulseBankOfResonators2.{ampFunc1, ampFunc2}
import net.soundmining.Scratchpad.{OvertoneAmpsArgument, RingtimesArgument}
import net.soundmining.modular.ModularInstrument.StaticAudioBusInstrument
import net.soundmining.synth.Instrument.{EFFECT, ROOM_EFFECT, SOURCE, TAIL_ACTION}
import net.soundmining.synth.Utils.absoluteTimeToMillis

import java.awt.Color
import scala.util.Random

/**
 * Mix noise and pitched tones. Maybe with a MarkovChain between noise and pitch
 * and a second layer of MarkovChain to choose which noise and pitch.
 *
 */
object AmbientMusic7 {
  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 2, bufferedPlayback = false)
  var patchPlayback: PatchPlayback = PatchPlayback(patch = NoiseBankOfResonators, client = client)
  val superColliderReceiver: SuperColliderReceiver = SuperColliderReceiver(patchPlayback)
  val WRITE_TO_SCORE = true

  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()
    superColliderReceiver.start()
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.send(clearSched())
    client.send(deepFree(0))
    client.stop
    this.superColliderReceiver.stop()
  }

  def makeSpect(): SpectrumScale =
    SpectrumScale("c2", "hess3", 10)


  def makeSpectrumScales(): Seq[Seq[Double]] = {
    val spect = makeSpect()
    val scale = spect.makeEqualTemperedScale(48)
    spect.makeSpectrums(scale)
  }

  def playCleanEffect(startTime: Double, effectAudioBus: StaticAudioBusInstrument, ampValue: Double, duration: Double, output: Int): Unit = {
    val cleanAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.1, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

    val clean = ModularSynth.stereoVolume(effectAudioBus, cleanAmp(ampValue))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    clean.getOutputBus.staticBus(output)
    val graph = clean.buildGraph(startTime, duration, clean.graph(Seq()))

    if (WRITE_TO_SCORE) {
      graph.map(SuperColliderClient.newSynthRaw)
        .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
    } else {
      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }
  }

  def panLine(distance: Double): (Double, Double) = {
    val randomDistance = distance * randomRange(0.8, 1.2)
    val startingPoint = randomRange(-0.75, 0.75)
    val direction = if (random.nextBoolean()) 1.0 else -1.0
    val desiredEndpoint = (startingPoint + randomDistance) * direction
    val endPoint = math.max(math.min(desiredEndpoint, 1.0), -1.0)

    (startingPoint, endPoint)
  }

  object Piece {

    def playPiece(start: Double, reset: Boolean = true): Unit = {
      if (reset) client.resetClock()

      if (WRITE_TO_SCORE) {
        synthPlayer.superColliderScore.addMessage(0, groupHead(0, SOURCE.nodeId))
        synthPlayer.superColliderScore.addMessage(0, groupTail(SOURCE.nodeId, EFFECT.nodeId))
        synthPlayer.superColliderScore.addMessage(0, groupTail(EFFECT.nodeId, ROOM_EFFECT.nodeId))
        synthPlayer.superColliderScore.addMessage(0, loadDir(SYNTH_DIR))
      }

      val totalDuration = 60.0 * 5.0

      val pulseEffectChannels = (
        PulseBankOfResonators.playShortEffect1(start, totalDuration, 0, 2),
        PulseBankOfResonators.playShortEffect2(start, totalDuration, 4, 6),
        PulseBankOfResonators.playShortEffect3(start, totalDuration, 8, 10))

      val oscEffectChannels = (
        BankOfOsc.playLongEffect1(start, totalDuration, 12, 14),
        BankOfOsc.playLongEffect2(start, totalDuration, 16, 18),
        BankOfOsc.playLongEffect3(start, totalDuration, 20, 22))


      val noiseEffectChannels = (
        NoiseBankOfResonators.playLongEffect1(start, totalDuration, 24, 26),
        NoiseBankOfResonators.playLongEffect2(start, totalDuration, 28, 30),
        NoiseBankOfResonators.playLongEffect3(start, totalDuration, 32, 34))

      val ui = UiModelBuilder(Seq("Osc One", "Osc Two", "Osc Three", "Noise One", "Noise Two", "Noise Three", "Pulse One", "Pulse Two", "Pulse Three"))

      val pulsePart1 = PulseBankOfResonators
        .pulsePart1(ui, pulseEffectChannels)
        .startTimeHandler(currentTime => currentTime + ((8 + 8 + 13) + randomRange(8, 13)))
        .build()

      val pulsePart2 = PulseBankOfResonators
        .pulsePart2(ui, pulseEffectChannels)
        .startTimeHandler(currentTime => currentTime + ((13 + 13 + 13 + 21) + randomRange(8, 13)))
        .spawnSequencerHandler(1, pulsePart1)
        .build()

      val pulsePart3 = PulseBankOfResonators
        .pulsePart3(ui, pulseEffectChannels)
        .startTimeHandler(currentTime => currentTime + randomRange(5, 8))
        .spawnSequencerHandler(0, pulsePart2)
        .build()


      val oscPart3 = BankOfOsc
        .oscPart3(ui, oscEffectChannels)
        .startTimeHandler(currentTime => currentTime + 13 + randomRange(8, 13))
        .build()

      val oscPart1 = BankOfOsc
        .oscPart1(ui, oscEffectChannels)
        .startTimeHandler(currentTime => currentTime + ((8 + 8 + 13) + randomRange(8, 13)))
        .spawnSequencerHandler(3, oscPart3)
        .build()

      val oscPart2 = BankOfOsc
        .oscPart2(ui, oscEffectChannels)
        .startTimeHandler(currentTime => currentTime + randomRange(3, 5))
        .spawnSequencerHandler(1, oscPart1)
        .spawnSequencerHandler(0, pulsePart3)
        .build()


      val noisePart2 = NoiseBankOfResonators
        .noisePart2(ui, noiseEffectChannels)
        .startTimeHandler(currentTime => currentTime + (13 + 13 + 13 + 21) + randomRange(8, 13))
        .build()

      val noisePart3 = NoiseBankOfResonators
        .noisePart3(ui, noiseEffectChannels)
        .startTimeHandler(currentTime => currentTime + 13 + randomRange(8, 13))
        .spawnSequencerHandler(0, noisePart2)
        .build()

      NoiseBankOfResonators
        .noisePart1(ui, noiseEffectChannels)
        .spawnSequencerHandler(0, oscPart2)
        .spawnSequencerHandler(3, noisePart3)
        .build()
        .generateSequence(start)

      PieceCanvas.displayUiModel(ui.uiModel())
      if (WRITE_TO_SCORE) {
        synthPlayer.superColliderScore.makeScore("ambientMusic7.txt")
      }
    }
  }

  object BankOfOsc extends InteractivePatch {

    val overtoneAmpsArgs = ChooseArgument(choices = Seq(
      // Note1 (long)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    val noteType = ChooseArgument(Seq(
      StaticArgument(1), StaticArgument(2), StaticArgument(3)))


    def playLongEffect1(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 5, stereo = 0.5, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }
      effectAudioBus
    }

    def playLongEffect2(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 3, stereo = 0.2, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    def playLongEffect3(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 8, stereo = 0.9, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    var effectChannel1: StaticAudioBusInstrument = _
    var effectChannel2: StaticAudioBusInstrument = _
    var effectChannel3: StaticAudioBusInstrument = _

    override def extendedNoteHandle(patchArguments: Interactive.PatchArguments): Unit = {

      if (effectChannel1 == null) {
        effectChannel1 = playLongEffect1(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel2 = playLongEffect2(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel3 = playLongEffect3(patchArguments.start, 60.0 * 6.0, 0, 0)
      }

      val note = (patchArguments.key - 36) % 48

      noteType(patchArguments) match {
        case 1 =>
          note1(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel1)
        case 2 =>
          note2(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel2)
        case 3 =>
          note3(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel3)
      }

    }

    // a ciss h jämnt. kort pause mellan varje not. Också låta dom överlappa och komma tätare
    def note1(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val values = (
          0.0,
          amp * randomRange(0.05, 0.25),
          amp * randomRange(0.05, 0.25),
          0.0)
        val times = (
          randomRange(0.25, 0.35),
          randomRange(0.35, 0.45),
          randomRange(0.25, 0.35))
        threeBlockControl(values, times, (0, 0, 0))
      }

      val ampFunc = () => staticAmp * randomRange(0.3, 0.66)

      val ampValue = ampFunc()
      val (panStart, panEnd) = panLine(0.7)

      val duration = randomRange(8, 13)

      val phasesZero = OvertonePhases()

      synthPlayer()
        .bankOfOsc(makeSpectrumScales()(note), overtoneAmps.amps(), phasesZero.phases())
        .monoVolume(volume(ampValue))
        .pan(lineControl(panStart, panEnd))
        //.playWithDuration(start, duration)
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)
      duration
    }


    // fiss giss. long pause fiss giss en oktav högre
    def note2(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        sineControl(0, volume)
      }

      val duration = randomRange(3, 5)

      val (panStart, panEnd) = panLine(0.5)

      val ampFunc = () => staticAmp * randomRange(0.3, 0.66)

      val amp = ampFunc() * staticAmp

      val phasesZero = OvertonePhases()

      synthPlayer()
        .bankOfOsc(makeSpectrumScales()(note), overtoneAmps.amps(), phasesZero.phases())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        //.playWithDuration(start, duration)
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)
      duration

    }

    // g aiss c giss. Overlapping
    def note3(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        twoBlockControl((0, volume, 0), times, (0, 0))
      }

      val duration = randomRange(13, 21)

      val (panStart, panEnd) = panLine(1.3)

      val ampFunc = () => staticAmp * randomRange(0.3, 0.66)

      val amp = ampFunc() * staticAmp

      val phasesZero = OvertonePhases()

      synthPlayer()
        .bankOfOsc(makeSpectrumScales()(note), overtoneAmps.amps(), phasesZero.phases())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        //.playWithDuration(start, duration)
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }

    def playOscPart(start: Double, reset: Boolean = true): Unit = {
      if (reset) client.resetClock()

      val totalDuration = 60.0 * 6.0

      val effectChannels = (
        playLongEffect1(start, totalDuration, 0, 0),
        playLongEffect2(start, totalDuration, 0, 0),
        playLongEffect3(start, totalDuration, 0, 0))

      val ui = UiModelBuilder(Seq("Osc One", "Osc Two", "Osc Three"))

      oscPart3(ui, effectChannels).build().generateSequence(start)
      PieceCanvas.displayUiModel(ui.uiModel())
    }

    def getStaticAmp(variant: Int): Double = if (variant % 2 == 0) 2.0 else 4.0

    val overtoneAmpsVariants = Seq(
      // Note1 (long)
      (SimpleSieve(3, 0), ampFunc2),
      (SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      (SimpleSieve(3, 0), ampFunc1),
      (SimpleSieve(2, 1), ampFunc1))
      .map {
        case (sieve, ampFunction) => OvertoneAmps(i => sieve.isSieve(i), ampFunction)
      }

    def oscPart1(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {

      val (effect1, effect2, effect3) = effectChannels

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime =>
          currentTime + (13 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .build()

      // // g aiss c giss (e ciss h diss)

      // 16,13, 11, 15
      val notes3 = Seq(((7, 10, 12, 8), 1))

      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (5 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((note11, note21, note31, note41), variant) = notes3(index)

          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration11 = note3(currentTime + (0.1 * randomRange(0.1, 1.1)), note11, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", currentTime, 0.5, duration11, note11, color)

          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration21 = note3(secondStart + (0.1 * randomRange(0.1, 1.1)), note21, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", secondStart, 0.5, duration21, note21, color)

          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration31 = note3(thirdStart + (0.1 * randomRange(0.1, 1.1)), note31, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", thirdStart, 0.5, duration31, note31, color)

          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration41 = note3(forthStart + (0.1 * randomRange(0.1, 1.1)), note41, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", forthStart, 0.5, duration41, note41, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
        .build()

      val notes2 = Seq(((6, 8), 2), ((6, 8), 2), ((6, 8), 2), ((6, 8), 2))
      val times2 = Seq(13, 13 * 2, 13, 13)
        .map(_ * randomRange(0.9, 1.1))

      Sequencer(notes2.length)
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstOneNote, secondOneNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration11 = note2(currentTime + (0.1 * randomRange(0.1, 1.1)), firstOneNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", currentTime, 0.5, duration11, firstOneNote, color)

          val secondStart = currentTime + randomRange(3, 5)
          val duration21 = note2(secondStart + (0.1 * randomRange(0.1, 1.1)), secondOneNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", secondStart, 0.5, duration21, secondOneNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

    def oscPart2(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {

      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))
      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + ((13 + 5) * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 0))
      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (5 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      Sequencer(notes1.length)
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

    def oscPart3(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {

      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))
      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Osc Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes1 = Seq(((9, 13, 11), 0), ((9, 13, 11), 0))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Osc One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 1))
      Sequencer(notes3.length)
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Osc Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
    }


  }

  object NoiseBankOfResonators extends InteractivePatch {
    val ampFunc = () => randomRange(0.3, 0.66)

    val overtoneAmps = Seq(
      // Note1 (long)
      (SimpleSieve(3, 0), ampFunc2),
      (SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      (SimpleSieve(3, 0), ampFunc1),
      (SimpleSieve(2, 1), ampFunc1))
      .map {
        case (sieve, func) => OvertoneAmps(i => sieve.isSieve(i), func)
      }

    val overtoneAmpsArgs = ChooseArgument(choices = Seq(
      // Note1 (long)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    val noteType = ChooseArgument(Seq(
      StaticArgument(1), StaticArgument(2), StaticArgument(3)))

    val overtoneAmpsVariants = Seq(
      // Note1 (long)
      (SimpleSieve(3, 0), ampFunc2),
      (SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      (SimpleSieve(3, 0), ampFunc1),
      (SimpleSieve(2, 1), ampFunc1))
      .map {
        case (sieve, ampFunction) => OvertoneAmps(i => sieve.isSieve(i), ampFunction)
      }

    def playNoisePart(start: Double, reset: Boolean = true): Unit = {
      if (reset) client.resetClock()

      val totalDuration = 60.0 * 6.0

      val effectChannels = (
        playLongEffect1(start, totalDuration, 0, 0),
        playLongEffect2(start, totalDuration, 0, 0),
        playLongEffect3(start, totalDuration, 0, 0))

      val ui = UiModelBuilder(Seq("Noise One", "Noise Two", "Noise Three"))

      noisePart3(ui, effectChannels).build().generateSequence(start)
      PieceCanvas.displayUiModel(ui.uiModel())
    }

    def getStaticAmp(variant: Int): Double = if (variant % 2 == 0) 4.0 else 8.0

    def noisePart3(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {
      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))

      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes1 = Seq(((9, 13, 11), 0), ((9, 13, 11), 0))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 1))
      Sequencer(notes3.length)
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
    }

    def noisePart2(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {
      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))
      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + ((13 + 5) * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 0))
      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (5 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      Sequencer(notes1.length)
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

    def noisePart1(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {
      val (effect1, effect2, effect3) = effectChannels

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime =>
          currentTime + (13 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Noise One", thirdStart, 0.5, duration3, thirdNote, color)
        }).build()

      // // g aiss c giss (e ciss h diss)

      // 16,13, 11, 15
      val notes3 = Seq(((7, 10, 12, 8), 1))

      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (5 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((note11, note21, note31, note41), variant) = notes3(index)

          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration11 = note3(currentTime + (0.1 * randomRange(0.1, 1.1)), note11, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", currentTime, 0.5, duration11, note11, color)

          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration21 = note3(secondStart + (0.1 * randomRange(0.1, 1.1)), note21, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", secondStart, 0.5, duration21, note21, color)

          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration31 = note3(thirdStart + (0.1 * randomRange(0.1, 1.1)), note31, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", thirdStart, 0.5, duration31, note31, color)

          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration41 = note3(forthStart + (0.1 * randomRange(0.1, 1.1)), note41, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Noise Three", forthStart, 0.5, duration41, note41, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
        .build()

      val notes2 = Seq(((6, 8), 2), ((6, 8), 2), ((6, 8), 2), ((6, 8), 2))
      val times2 = Seq(13, 13 * 2, 13, 13)
        .map(_ * randomRange(0.9, 1.1))

      Sequencer(notes2.length)
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstOneNote, secondOneNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = getStaticAmp(variant)
          val duration11 = note2(currentTime + (0.1 * randomRange(0.1, 1.1)), firstOneNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", currentTime, 0.5, duration11, firstOneNote, color)

          val secondStart = currentTime + randomRange(3, 5)
          val duration21 = note2(secondStart + (0.1 * randomRange(0.1, 1.1)), secondOneNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Noise Two", secondStart, 0.5, duration21, secondOneNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

    def playLongEffect1(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 5, stereo = 0.5, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    def playLongEffect2(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 3, stereo = 0.2, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    def playLongEffect3(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, 0.6, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(0.4), rt60 = 8, stereo = 0.9, lowRatio = 0.5, hiRatio = 0.5)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    var effectChannel1: StaticAudioBusInstrument = _
    var effectChannel2: StaticAudioBusInstrument = _
    var effectChannel3: StaticAudioBusInstrument = _

    override def extendedNoteHandle(patchArguments: Interactive.PatchArguments): Unit = {

      if (effectChannel1 == null) {
        effectChannel1 = playLongEffect1(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel2 = playLongEffect2(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel3 = playLongEffect3(patchArguments.start, 60.0 * 6.0, 0, 0)
      }

      val note = (patchArguments.key - 36) % 48
      noteType(patchArguments) match {
        case 1 =>
          note1(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel1)
        case 2 =>
          note2(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel2)
        case 3 =>
          note3(patchArguments.start, note, 2.0, overtoneAmpsArgs(patchArguments), effectChannel3)
      }
    }

    // a ciss h (e c d)
    // fiss giss (f diss)
    // g aiss c giss (e ciss h diss)


    // a ciss h jämnt. kort pause mellan varje not. Också låta dom överlappa och komma tätare
    def note1(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val values = (
          0.0,
          amp * randomRange(0.05, 0.25),
          amp * randomRange(0.05, 0.25),
          0.0)
        val times = (
          randomRange(0.25, 0.35),
          randomRange(0.35, 0.45),
          randomRange(0.25, 0.35))
        threeBlockControl(values, times, (0, 0, 0))
      }

      val ampFunc = () => staticAmp * randomRange(0.3, 0.66)

      val overtoneRingtimes = OvertoneRingtimes(_ => randomRange(0.05, 0.15))

      val ampValue = ampFunc()
      val (panStart, panEnd) = panLine(0.7)

      val duration = randomRange(8, 13)

      synthPlayer()
        .pinkNoise(volume(ampValue))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(ampValue))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }

    // fiss giss. long pause fiss giss en oktav högre
    def note2(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        sineControl(0, volume)
      }

      val overtoneRingtimes = OvertoneRingtimes(_ =>
        randomRange(0.1, 0.25))

      val duration = randomRange(3, 5)

      val (panStart, panEnd) = panLine(0.5)

      val amp = ampFunc() * staticAmp

      synthPlayer()
        .pinkNoise(volume(amp))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration

    }

    // g aiss c giss. Overlapping
    def note3(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        twoBlockControl((0, volume, 0), times, (0, 0))
      }

      val overtoneRingtimes = OvertoneRingtimes(_ =>
        randomRange(0.15, 0.36))

      val duration = randomRange(13, 21)

      val (panStart, panEnd) = panLine(1.3)

      val amp = ampFunc() * staticAmp

      synthPlayer()
        .pinkNoise(volume(amp))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }
  }

  object PulseBankOfResonators extends InteractivePatch {
    val pulseScale = makeSpect().makeLowerEqualTemperedScale(40)

    val harmonicsAmpFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
    val harmonicsAmpFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)

    val ampFunc = () => randomRange(0.3, 0.66)
    val overtoneAmps = Seq(
      // Note1 (long)
      (SimpleSieve(3, 0), harmonicsAmpFunc2),
      (SimpleSieve(2, 1), harmonicsAmpFunc2),
      // Note2 (short)
      (SimpleSieve(3, 0), harmonicsAmpFunc1),
      (SimpleSieve(2, 1), harmonicsAmpFunc1))
      .map {
        case (sieve, func) => OvertoneAmps(i => sieve.isSieve(i), func)
      }

    def playShortEffect1(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

      playCleanEffect(startTime, effectAudioBus, 0.5, duration, cleanOut)

      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.35, damp = 0.8)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    def playShortEffect2(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

      playCleanEffect(startTime, effectAudioBus, 0.5, duration, cleanOut)

      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.6, damp = 0.7)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    def playShortEffect3(startTime: Double, duration: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)

      playCleanEffect(startTime, effectAudioBus, 0.5, duration, cleanOut)

      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrolv1(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val reverb = ModularSynth.stereoFreeReverb(effectAudioBus, reverbAmp(0.5), mix = 0.7, room = 0.8, damp = 0.3)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      if (WRITE_TO_SCORE) {
        graph.map(SuperColliderClient.newSynthRaw)
          .foreach(message => synthPlayer.superColliderScore.addMessage(startTime, message))
      } else {
        client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      }

      effectAudioBus
    }

    // var note = ChooseArgument(choices = Seq(StaticArgument(v => note1(v)), StaticArgument(v => note2(v)), StaticArgument(v => note3(v))))
    val noteType = ChooseArgument(Seq(
      StaticArgument(1), StaticArgument(2), StaticArgument(3)))

    var effectChannel1: StaticAudioBusInstrument = _
    var effectChannel2: StaticAudioBusInstrument = _
    var effectChannel3: StaticAudioBusInstrument = _

    val overtoneAmpsArgs = ChooseArgument(choices = Seq(
      // Note1 (long)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc2),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      OvertoneAmpsArgument(SimpleSieve(3, 0), ampFunc1),
      OvertoneAmpsArgument(SimpleSieve(2, 1), ampFunc1)))

    override def extendedNoteHandle(patchArguments: Interactive.PatchArguments): Unit = {
      if (effectChannel1 == null) {
        effectChannel1 = playShortEffect1(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel2 = playShortEffect2(patchArguments.start, 60.0 * 6.0, 0, 0)
        effectChannel3 = playShortEffect3(patchArguments.start, 60.0 * 6.0, 0, 0)
      }
      val note = (patchArguments.key - 36) % 48
      noteType(patchArguments) match {
        case 1 =>
          note1(patchArguments.start, note, 1.0, overtoneAmpsArgs(patchArguments), effectChannel1)
        case 2 =>
          note2(patchArguments.start, note, 1.0, overtoneAmpsArgs(patchArguments), effectChannel2)
        case 3 =>
          note3(patchArguments.start, note, 1.0, overtoneAmpsArgs(patchArguments), effectChannel3)
      }

    }

    // a ciss h jämnt. kort pause mellan varje not. Också låta dom överlappa och komma tätare
    def note1(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val values = (
          0.0,
          amp * randomRange(0.05, 0.25),
          amp * randomRange(0.05, 0.25),
          0.0)
        val times = (
          randomRange(0.25, 0.35),
          randomRange(0.35, 0.45),
          randomRange(0.25, 0.35))
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulse = () => {
        val values = (
          pulseScale(randomIntRange(13, 17)),
          pulseScale(randomIntRange(18, 22)),
          pulseScale(randomIntRange(23, 27)),
          pulseScale(randomIntRange(17, 21)))

        val times = (randomRange(0.25, 0.35), randomRange(0.35, 0.45), randomRange(0.25, 0.35))
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulseReverse = () => {

        val values = (
          pulseScale(randomIntRange(17, 21)),
          pulseScale(randomIntRange(23, 27)),
          pulseScale(randomIntRange(18, 22)),
          pulseScale(randomIntRange(13, 17)))

        val times = (randomRange(0.25, 0.35), randomRange(0.35, 0.45), randomRange(0.25, 0.35))
        threeBlockControl(values, times, (0, 0, 0))
      }

      val pulseChoose = () => {
        pickItems(Seq(pulse, pulseReverse), 1).head()
      }

      val overtoneRingtimes = OvertoneRingtimes(_ => randomRange(0.05, 0.15))

      val duration = randomRange(8, 13)

      val (panStart, panEnd) = panLine(0.7)

      val amp = ampFunc() * staticAmp

      synthPlayer()
        .pulse(pulseChoose(), volume(amp))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }

    // fiss giss. long pause fiss giss en oktav högre
    def note2(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        sineControl(0, volume)
      }

      val pulse = () => {
        val start = pulseScale(randomIntRange(18, 22))
        val peak = pulseScale(randomIntRange(23, 27))
        sineControl(start, peak)
      }

      val overtoneRingtimes = OvertoneRingtimes(_ =>
        randomRange(0.1, 0.25))

      val duration = randomRange(3, 5)

      val (panStart, panEnd) = panLine(0.5)

      val amp = ampFunc() * staticAmp

      synthPlayer()
        .pulse(pulse(), volume(amp))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }

    // g aiss c giss. Overlapping
    def note3(start: Double, note: Int, staticAmp: Double, overtoneAmps: OvertoneAmps, effectChannel: StaticAudioBusInstrument): Double = {
      val volume = (amp: Double) => {
        val volume = amp * randomRange(0.05, 0.15)
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        twoBlockControl((0, volume, 0), times, (0, 0))
      }

      val pulse = () => {
        val start = pulseScale(randomIntRange(34, 37))
        val peak = pulseScale(randomIntRange(29, 33))
        val end = pulseScale(randomIntRange(34, 37))
        val times = (randomRange(0.33, 0.66), randomRange(0.33, 0.66))
        twoBlockControl((start, peak, end), times, (0, 0))
      }

      val overtoneRingtimes = OvertoneRingtimes(_ =>
        randomRange(0.15, 0.36))

      val duration = randomRange(13, 21)

      val (panStart, panEnd) = panLine(1.3)

      val amp = ampFunc() * staticAmp

      synthPlayer()
        .pulse(pulse(), volume(amp))
        .bankOfResonators(makeSpectrumScales()(note), overtoneAmps.amps(), overtoneRingtimes.ringtimes())
        .monoVolume(volume(amp))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration, outputBus = effectChannel.getOutputBus.busValue.get, realOutput = false, shouldWriteToScore = WRITE_TO_SCORE)

      duration
    }

    val overtoneAmpsVariants = Seq(
      // Note1 (long)
      (SimpleSieve(3, 0), ampFunc2),
      (SimpleSieve(2, 1), ampFunc2),
      // Note2 (short)
      (SimpleSieve(3, 0), ampFunc1),
      (SimpleSieve(2, 1), ampFunc1))
      .map {
        case (sieve, ampFunction) => OvertoneAmps(i => sieve.isSieve(i), ampFunction)
      }

    def playPulsePart(start: Double, reset: Boolean = true): Unit = {
      if (reset) client.resetClock()

      val totalDuration = 60.0 * 6.0

      val ui = UiModelBuilder(Seq("Pulse One", "Pulse Two", "Pulse Three"))

      val effectChannels = (
        playShortEffect1(start, totalDuration, 0, 0),
        playShortEffect2(start, totalDuration, 0, 0),
        playShortEffect3(start, totalDuration, 0, 0))

      pulsePart2(ui, effectChannels).build().generateSequence(start)
      PieceCanvas.displayUiModel(ui.uiModel())
    }

    def pulsePart3(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {
      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))

      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes1 = Seq(((9, 13, 11), 0), ((9, 13, 11), 0))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 1))
      Sequencer(notes3.length)
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
    }

    def pulsePart2(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {
      val (effect1, effect2, effect3) = effectChannels

      val notes2 = Seq(((6, 8), 3), ((6, 8), 3))
      val times2 = Seq(13 * 2, 13)
        .map(_ * randomRange(0.9, 1.1))
      val notes2Seq = Sequencer(notes2.length)
        .startTimeHandler(currentTime => currentTime + ((13 + 5) * randomRange(0.9, 1.0)))
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 0))
      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (5 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes2Seq)
        .build()

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      Sequencer(notes1.length)
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

    def pulsePart1(ui: UiModelBuilder, effectChannels: (StaticAudioBusInstrument, StaticAudioBusInstrument, StaticAudioBusInstrument)): Sequencer.Builder = {

      val (effect1, effect2, effect3) = effectChannels

      val notes1 = Seq(((9, 13, 11), 1), ((9, 13, 11), 1))
      val notes1Seq = Sequencer(notes1.length)
        .startTimeHandler(currentTime =>
          currentTime + (13 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (8 + 8 + 13) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote), variant) = notes1(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note1(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (8 * randomRange(0.9, 1.1))
          val duration2 = note1(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (8 * randomRange(0.9, 1.1))
          val duration3 = note1(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect1)
          ui.addUi("Pulse One", thirdStart, 0.5, duration3, thirdNote, color)
        })
        .build()

      val notes3 = Seq(((7, 10, 12, 8), 1))
      val notes3Seq = Sequencer(notes3.length)
        .startTimeHandler(currentTime => currentTime + (8 * randomRange(0.9, 1.1)))
        .nextTimeHandler(_ => (13 + 13 + 13 + 21) * randomRange(0.9, 1.1))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote, thirdNote, forthNote), variant) = notes3(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note3(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + (13 * randomRange(0.9, 1.1))
          val duration2 = note3(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", secondStart, 0.5, duration2, secondNote, color)
          val thirdStart = secondStart + (13 * randomRange(0.9, 1.1))
          val duration3 = note3(thirdStart, thirdNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", thirdStart, 0.5, duration3, thirdNote, color)
          val forthStart = thirdStart + (13 * randomRange(0.9, 1.1))
          val duration4 = note3(forthStart, forthNote, staticAmp, overtoneAmpsVariants(variant), effect3)
          ui.addUi("Pulse Three", forthStart, 0.5, duration4, forthNote, color)
        })
        .spawnSequencerHandler(0, notes1Seq)
        .build()

      val notes2 = Seq(((6, 8), 2), ((6, 8), 2), ((6, 8), 2), ((6, 8), 2))
      val times2 = Seq(13, 13 * 2, 13, 13)
        .map(_ * randomRange(0.9, 1.0))

      Sequencer(notes2.length)
        .nextTimeHandler(i => times2(i))
        .stepHandler((index, currentTime) => {
          val ((firstNote, secondNote), variant) = notes2(index)
          val color = if (variant % 2 == 0) Color.darkGray else Color.lightGray
          val staticAmp = if (variant % 2 == 0) 1.0 else 2.0
          val duration1 = note2(currentTime, firstNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", currentTime, 0.5, duration1, firstNote, color)
          val secondStart = currentTime + randomRange(3, 5)
          val duration2 = note2(secondStart, secondNote, staticAmp, overtoneAmpsVariants(variant), effect2)
          ui.addUi("Pulse Two", secondStart, 0.5, duration2, secondNote, color)
        })
        .spawnSequencerHandler(0, notes3Seq)
    }

  }
}
