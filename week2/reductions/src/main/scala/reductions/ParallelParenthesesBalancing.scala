package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    balance(chars, 0)
  }

  private def balance(chars: Array[Char], count:Int): Boolean = {
    if (chars.isEmpty && count == 0) return true
    if (chars.isEmpty && count != 0) return false
    if (count < 0) return false
    chars.head match {
      case '(' =>  balance(chars.tail, count+1)
      case ')' =>  balance(chars.tail, count-1)
      case _ => balance(chars.tail, count)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, openCount: Int, closeCount: Int) : Int = ???
//    {
//      if (chars.isEmpty && idx == until -1) return true
//      if (chars.isEmpty && count != 0) return false
//      if (count < 0) return false
//      chars.head match {
//        case '(' =>  balance(chars.tail, count+1)
//        case ')' =>  balance(chars.tail, count-1)
//        case _ => balance(chars.tail, count)
//      }
//    }

    def reduce(from: Int, until: Int) /*: Int*/ = ???
//    {
//      if (until - from < 1) return 0
//
//      if (until - from == 1) {
//        chars(from) match {
//          case '(' => 1
//          case ')' => -1
//        }
//      }
//
//      val split = (until - from) / 2
//      val (a,b) = parallel(reduce(from, from+split), reduce(from+split, until))
//
//    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
