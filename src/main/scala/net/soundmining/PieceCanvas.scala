package net.soundmining

import java.awt.{Color, Dimension, Graphics}
import javax.swing._
import scala.collection.mutable

case class PieceCanvas(uiModel: UiModel) extends JPanel {

  import PieceCanvas._

  override def getPreferredSize = {
    val width = (100 + (uiModel.getDuration * PieceCanvas.NOTE_SCALE_FACTOR)).toInt
    val height = (200 * uiModel.tracks.size) + 80
    new Dimension(width, height)
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val rulers = (uiModel.getDuration / 10.0).toInt
    (0 until rulers).map(_ * 10).foreach {
      rule =>
        g.drawString(
          s"$rule",
          100 + (rule * NOTE_SCALE_FACTOR),
          20)

        g.drawLine(
          100 + (rule * NOTE_SCALE_FACTOR),
          30,
          100 + (rule * NOTE_SCALE_FACTOR),
          35
        )
    }

    uiModel.tracks.zipWithIndex.foreach {
      case ((trackName, notes), trackIndex) =>
        g.drawString(trackName, 20, (trackIndex * TRACK_HEIGHT) + HEIGHT_INDENT)
        notes.foreach {
          case UiNote(start: Double, peak: Double, duration: Double, note: Int, color: Color) =>
            val originalColor = g.getColor
            g.setColor(color)
            g.drawLine(
              (200 + (start * NOTE_SCALE_FACTOR)).toInt,
              (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT,
              (200 + ((start + (duration * peak)) * NOTE_SCALE_FACTOR)).toInt,
              (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT - 5)
            g.drawLine(
              (200 + ((start + (duration * peak)) * NOTE_SCALE_FACTOR)).toInt,
              (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT - 5,
              (200 + ((start + duration) * NOTE_SCALE_FACTOR)).toInt,
              (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT)
            g.setColor(originalColor)
        }
    }
  }
}

case class UiNote(start: Double, peak: Double, duration: Double, note: Int, color: Color = Color.BLACK)

case class UiModel(tracks: Seq[(String, Seq[UiNote])]) {
  def getDuration: Double = {
    val noteDurations: Seq[Double] = tracks.map {
      case (_, notes) => notes.map(note => note.start + note.duration).maxOption.getOrElse(0)
    }
    noteDurations.max
  }
}

case class UiModelBuilder(trackNames: Seq[String]) {

  val uiBuilder: Seq[(String, mutable.Buffer[UiNote])] =
    trackNames.map(trackName => (trackName, mutable.Buffer()))

  def addUi(track: String, start: Double, peak: Double, duration: Double, note: Int, color: Color = Color.black): Unit =
    uiBuilder.find(uib => uib._1 == track).map(_._2.append(UiNote(start, peak, duration, note, color)))

  def uiModel(): UiModel =
    UiModel(uiBuilder.map {
      case (name, buffer) => (name, buffer.toSeq)
    })
}

object PieceCanvas {
  val TRACK_HEIGHT = 200
  val NOTE_SCALE_FACTOR = 20
  val HEIGHT_INDENT = 180

  def displayUiModel(uiModel: UiModel): Unit = {
    val duration = uiModel.getDuration
    val width = (100 + (duration * PieceCanvas.NOTE_SCALE_FACTOR)).toInt
    val height = (200 * uiModel.tracks.size) + 80
    println(s"duration $duration width $width")
    println(s"tracks ${uiModel.tracks.size} height $height")
    SwingUtilities.invokeLater(
      () => {
        val frame = new JFrame()
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        val canvas = PieceCanvas(uiModel)
        canvas.setSize(width, height)
        val panel = new JScrollPane(canvas)
        panel.setAutoscrolls(true)

        frame.getContentPane.add(panel)
        frame.setSize(math.min(4000, width), height)
        frame.pack()
        frame.setVisible(true)
      }
    )

  }
}

