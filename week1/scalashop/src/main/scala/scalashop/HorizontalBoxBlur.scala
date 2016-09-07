package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method

    val pieces = for {
      y <- from to end-1
      x <- 0 to src.width-1

    } yield (x, y)

    pieces foreach { case (x, y) =>
      val desPix = boxBlurKernel(src, x, y, radius)
      dst.update(x, y, desPix)
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
    if (src.width == 0 || src.height == 0) return

    val stripHeight = if (numTasks <= src.height) src.height/numTasks else 1
    val newNumTasks = if (numTasks <= src.height) numTasks else src.height

    val yindices = 0 to src.height by stripHeight
    var parts = (yindices zip yindices.tail)
    if (src.height % newNumTasks != 0) parts = (parts.last._2, src.height-1) +: parts

//    println (parts)

    val partsm = parts map { case (from, to) =>
      task {
        blur(src, dst, from, to, radius)
      }
    }

    partsm foreach {a=>a.join()}

  }


}
