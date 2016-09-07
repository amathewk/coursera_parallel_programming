package scalashop

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlurSuite2 extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {

    val src = new Img(13, 5)

    VerticalBoxBlur.parBlur(src, src, 3, 2)




//    for (x <- 0 until 5; y <- 0 until 5)
//      src(x, y) = rgba(x, y, x + y, math.abs(x - y))
//
//    for (x <- 0 until 5; y <- 0 until 5)
//      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
//        "boxBlurKernel(_,_,0) should be identity.")
  }



}
