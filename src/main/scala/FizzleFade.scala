import java.awt.Color.RED
import java.awt.Graphics
import java.lang.Thread.sleep
import java.util
import javax.swing.SwingUtilities.invokeAndWait
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import javax.swing.{JFrame, JPanel, SwingWorker}

import FizzleFade.Point

final class FizzleFade(title: String, width: Int, height: Int, points: (Int, Int) => Stream[Point])
  extends SwingWorker[Unit, Point] {

  @volatile private var frame: Option[JFrame] = None
  @volatile private var graphics: Option[Graphics] = None

  override def doInBackground(): Unit = {
    createFrameInBackground()
    publishRandomUpdates()
    sleep(500)
  }

  private def createFrameInBackground(): Unit = {
    invokeAndWait(() => {

      val frame = new JFrame(title)
      frame.setDefaultCloseOperation(DISPOSE_ON_CLOSE)
      frame.setSize(width, height)
      frame.setResizable(false)
      frame.setVisible(true)

      val panel = new JPanel()
      panel.setSize(width, height)
      frame.add(panel)

      val graphics = panel.getGraphics
      graphics.setColor(RED)

      this.frame = Some(frame)
      this.graphics = Some(graphics)
    })
  }

  private def publishRandomUpdates(): Unit = {
    points(width, height).grouped(50).foreach(group => {
      sleep(1)
      group.foreach { case point@(x, y) =>
        assert(x < width && y < height, point)
        publish(point)
      }
    })
  }

  override def process(points: util.List[Point]): Unit = {
    super.process(points)
    points.forEach { case (x, y) => graphics.get.drawRect(x, y, 1, 1) }
  }

  override def done(): Unit = {
    super.done()
    graphics.foreach(_.dispose())
    frame.foreach(_.dispose())
  }
}

object FizzleFade {

  type Point = (Int, Int)

  def main(args: Array[String]): Unit = {

    val workers = List(
      new FizzleFade("Linear Feedback Shift Register", 320, 200, randomPointsByLinearFeedbackShiftRegister),
      new FizzleFade("Feistel Network", 320, 200, randomPointsByFeistelNetwork),
      new FizzleFade("Generalized Feistel Network", 480, 320, randomPointsByGeneralizedFeistelNetwork)
    )

    workers.foreach({ worker =>
      worker.execute()
      worker.get()
    })
  }

  /*
   * http://fabiensanglard.net/fizzlefade/index.php
   * https://en.wikipedia.org/wiki/Linear-feedback_shift_register
   *
   * Designed for 320x200
   */
  def randomPointsByLinearFeedbackShiftRegister(width: Int, height: Int): Stream[Point] = {
    Stream
      .iterate(1)(i => (i >>> 1) ^ (-(i & 1) & 0x00012000))
      .zipWithIndex
      .takeWhile { case (value, index) => value != 1 || index == 0 }
      .map(_._1)
      .map(i => ((i & 0x1FF00) >>> 8, (i & 0xFF) - 1))
      .filter { case (x, y) => x < width && y < height }
  }

  /*
   * http://antirez.com/news/113
   * https://en.wikipedia.org/wiki/Feistel_cipher
   *
   * Designed for 320x200
   */
  def randomPointsByFeistelNetwork(width: Int, height: Int): Stream[Point] = {
    Stream
      .range(0, 65536)
      .map(i => {
        val (ln, rn) = Stream.iterate((i & 0xFF, i >>> 8), 8) { case (l, r) =>
          (r, l ^ ((((r * 11) + (r >> 5) + 7 * 127) ^ r) & 0xFF))
        }.last
        ((rn << 8) | ln) & 0xFFFF
      })
      .map(i => (i % width, i / width))
      .filter { case (x, y) => x < width && y < height }
  }

  /*
   * https://news.ycombinator.com/item?id=15125836
   *
   * Designed for any resolution
   */
  def randomPointsByGeneralizedFeistelNetwork(width: Int, height: Int): Stream[Point] = {
    Stream
      .range(0, width * height)
      .map(i => {
        Stream.iterate(i, 8) { i =>
          val l = i / width
          val r = i % width
          val F = (r * 356357) % height
          height * r + (l + F) % height
        }.last
      })
      .map(i => (i % width, i / width))
  }
}
