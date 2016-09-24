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
//    if (chars.isEmpty && count == 0) return true
//    if (chars.isEmpty && count != 0) return false
//    if (count < 0) return false
//    chars.head match {
//      case '(' =>  balance(chars.tail, count+1)
//      case ')' =>  balance(chars.tail, count-1)
//      case _ => balance(chars.tail, count)
//    }

    var idx:Int = 0
    var count:Int = 0
    while (idx < chars.size) {
      chars(idx) match {
        case '(' =>  count += 1
        case ')' =>  if (count < 1) return false else count -= 1
        case _ =>
      }
      idx += 1
    }

    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int) : (Int, Int) =
    {
//      def matcher(chars:List[Char], open: Int, close: Int) : (Int, Int) = {
//        if (chars.isEmpty) return (open,close)
//        chars.head match {
//          case '(' => matcher(chars.tail, open + 1, close)
//          case ')' => if (open > 0) matcher(chars.tail, open - 1, close) else matcher(chars.tail, open, close + 1)
//          case _ => matcher(chars.tail, open,close)
//        }
//      }
//      def charList = chars.toList.slice(idx, until)
//      matcher(charList, 0, 0)

      var ind:Int = idx
      var openCount = 0
      var closeCount = 0
      while(ind < until) {
        chars(ind) match {
          case '(' => openCount += 1
          case ')' => if (openCount > 0) openCount -= 1 else closeCount += 1
          case _ =>
        }
        ind += 1
      }
      (openCount, closeCount)
    }

    def reduce(from: Int, until: Int) : (Int, Int) =
    {
      if (until - from < threshold) return traverse(from, until)
      else {
        val split = (until + from) / 2
        val (a,b) = parallel(reduce(from, split), reduce(split, until))
        val matched = scala.math.min(a._1, b._2)
        (a._1 + b._1 - matched, a._2 + b._2 - matched)
      }
    }
    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
