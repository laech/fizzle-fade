import java.awt.Color.RED
import java.awt.{Graphics, Point}
import java.lang.Thread.sleep
import java.util
import javax.swing.SwingUtilities.invokeAndWait
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import javax.swing.{JFrame, SwingWorker}

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

      val graphics = frame.getGraphics
      graphics.setColor(RED)

      this.frame = Some(frame)
      this.graphics = Some(graphics)
    })
  }

  private def publishRandomUpdates(): Unit = {
    points(width, height).grouped(50).foreach(group => {
      sleep(1)
      group.foreach(p => {
        assert(p.x < width && p.y < height, p.toString)
        publish(p)
      })
    })
  }

  override def process(points: util.List[Point]): Unit = {
    super.process(points)
    points.forEach(p => graphics.get.drawRect(p.x, p.y, 1, 1))
  }

  override def done(): Unit = {
    super.done()
    graphics.foreach(_.dispose())
    frame.foreach(_.dispose())
  }
}

object FizzleFade {

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
   */
  def randomPointsByLinearFeedbackShiftRegister(width: Int, height: Int): Stream[Point] = Stream
    .iterate(1)(i => if ((i & 1) == 0) i >>> 1 else (i >>> 1) ^ 0x00012000)
    .drop(1)
    .takeWhile(_ != 1)
    .map(i => new Point((i & 0x1FF00) >>> 8, i & 0xFF))
    .filter(p => p.x < width && p.y < height)

  /*
   * http://antirez.com/news/113
   * https://en.wikipedia.org/wiki/Feistel_cipher
   */
  def randomPointsByFeistelNetwork(width: Int, height: Int): Stream[Point] = Stream
    .range(0, 65536)
    .map(i => {
      var l = i & 0xFF
      var r = i >>> 8
      for (_ <- 0 until 8) {
        val nl = r
        val F = (((r * 11) + (r >> 5) + 7 * 127) ^ r) & 0xFF
        r = l ^ F
        l = nl
      }
      ((r << 8) | l) & 0xFFFF
    })
    .map(i => new Point(i % width, i / width))
    .filter(p => p.x < width && p.y < height)

  /*
   * https://news.ycombinator.com/item?id=15125836
   */
  def randomPointsByGeneralizedFeistelNetwork(width: Int, height: Int): Stream[Point] = Stream
    .range(0, width * height)
    .map(i => {
      var value = i
      for (j <- 0 until 8) {
        var l = value / width
        var r = value % width
        val nl = r
        val F = (r * 356357 + j * 1234567) % height
        r = (l + F) % height
        l = nl
        value = height * l + r
      }
      value
    })
    .map(i => new Point(i % width, i / width))
}
