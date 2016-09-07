package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

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
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    val pieces = for {
      x <- from to end-1
      y <- 0 to src.height-1

    } yield (x, y)

    pieces foreach { case (x, y) =>
      val desPix = boxBlurKernel(src, x, y, radius)
      dst.update(x, y, desPix)
    }

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    if (src.width == 0 || src.height == 0) return

    // TODO implement using the `task` construct and the `blur` method
    val stripWidth = if (numTasks <= src.width) src.width/numTasks else 1
    val newNumTasks = if (numTasks <= src.width) numTasks else src.width

    val xindices = 0 to src.width-1 by stripWidth
    var parts = (xindices zip xindices.tail)
    if (src.width-1 % newNumTasks != 0) parts = (parts.last._2, src.width) +: parts

//    println (parts)

    val partsm = parts map { case (from, to) =>
      task {
        blur(src, dst, from, to, radius)
      }
    }

    partsm foreach {a=>a.join()}

  }

}
