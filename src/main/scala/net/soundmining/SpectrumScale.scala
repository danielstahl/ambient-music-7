package net.soundmining

case class SpectrumScale(fundamentalNote: String, firstPartialNote: String, scaleSize: Int) {
  val fundamental = Note.noteToHertz(fundamentalNote)
  val firstPartial = Note.noteToHertz(firstPartialNote)
  val fact = Spectrum.makeFact(fundamental, firstPartial)

  val baseSpectrum = Spectrum.makeSpectrum2(fundamental, Spectrum.makeFact(fundamental, firstPartial), 15)

  def makeEqualTemperedScale(size: Int): Seq[Double] = {
    val fundamentalCents = Note.hertzToCents(fundamental)
    val octaveCents = Note.hertzToCents(firstPartial) - fundamentalCents
    val noteCents = octaveCents / scaleSize
    Seq.fill(size)(noteCents)
      .zipWithIndex
      .map {
        case (noteCent, i) => fundamentalCents + (noteCent * i)
      }
      .map(noteCents => Note.centsToHertz(noteCents))
  }

  def makeLowerEqualTemperedScale(size: Int): Seq[Double] = {
    val fundamentalCents = Note.hertzToCents(fundamental)
    val octaveCents = Note.hertzToCents(firstPartial) - fundamentalCents
    val noteCents = octaveCents / scaleSize
    Seq.fill(size)(noteCents)
      .zipWithIndex
      .map {
        case (noteCent, i) => fundamentalCents - (noteCent * i)
      }
      .map(noteCents => Note.centsToHertz(noteCents))
  }

  def makeSpectrums(freqs: Seq[Double]): Seq[Seq[Double]] =
    freqs.map(freq => Spectrum.makeSpectrum2(freq, fact, 15))
}
