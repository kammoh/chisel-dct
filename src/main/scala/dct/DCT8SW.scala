
package dct

import scala.math._
import scala.util.Random


class DCT8SW (val outBits: Int, val f1: Int, val r1: Int) extends DCT8 {
  private val data = Array.ofDim[Int](64)


  val MAX_VAL: Int = (1 << (outBits - 1)) - 1
  val MIN_VAL: Int = -(MAX_VAL + 1)

  private def computeForward(input: Array[Int], output: Array[Int], shift: Int): Unit = {
    val round: Int = (1 << shift) >> 1
    for (i <- 0 until 8) {

      def x(idx: Int) = input(i * 8 + idx)

      def a(idx: Int) = {
        val k = idx % 2 + (idx / 4) * 2
        x(k) + (if ((idx / 2) % 2 == 0) x(7 - k) else -x(7 - k))
      }

      def b(idx: Int) = a(idx % 2) + (if (idx > 1) -1 else 1) * a(5 - idx % 2)

      for (k <- 0 to 7) {
        val s = w.slice(k * 8, k * 8 + 4).zip(Seq(0, 1, 4, 5).map { d =>
          if (k % 2 == 0)
            b(if (k % 4 == 0) d else d + 2)
          else
            a(d + 2)
        }).filter(_._1 != 0).map { case (w: Int, in: Int) => w * in }.foldLeft(round)(_ + _) >> shift
        output(i + k * 8) = s
      }
    }
  }

  private def computeInverse(input: Array[Int], output: Array[Int], shift: Int): Unit = {
    val round: Int = (1 << shift) >> 1
    //println(s" SW computeInverse with shift = $shift round = $round")
    for (i <- 0 until 8) {


      def x(idx: Int) = input(i + idx * 8)

      def a(idx: Int) = idx match {
        case 0 => (w(8) * x(1)) + (w(24) * x(3)) + (w(40) * x(5)) + (w(56) * x(7))
        case 1 => (w(9) * x(1)) + (w(25) * x(3)) + (w(41) * x(5)) + (w(57) * x(7))
        case 2 => (w(10) * x(1)) + (w(26) * x(3)) + (w(42) * x(5)) + (w(58) * x(7))
        case 3 => (w(11) * x(1)) + (w(27) * x(3)) + (w(43) * x(5)) + (w(59) * x(7))
        case 4 => (w(16) * x(2)) + (w(48) * x(6))
        case 5 => (w(17) * x(2)) + (w(49) * x(6))
        case 6 => (w(0) * x(0)) + (w(32) * x(4))
        case 7 => (w(1) * x(0)) + (w(33) * x(4))
      }

      def b(idx: Int) = idx match {
        case 0 => a(6) + a(4)
        case 1 => a(7) + a(5)
        case 2 => a(6) - a(4)
        case 3 => a(7) - a(5)
      }


      def c(idx: Int) = ((idx match {
        case 0 => b(0) + a(0)
        case 1 => b(1) + a(1)
        case 2 => b(3) + a(2)
        case 3 => b(2) + a(3)
        case 4 => b(2) - a(3)
        case 5 => b(3) - a(2)
        case 6 => b(1) - a(1)
        case 7 => b(0) - a(0)
      }) + round) >> shift

//      def sat(c: Int) = max(min(c, MAX_VAL), MIN_VAL)

      for (j <- 0 to 7) {
        output(i * 8 + j) = c(j) //sat(c(j))
//        assert(s <= MAX_VAL && s >= MIN_VAL, s"inverse $s out of range!")
      }
    }

  }


  def forward(src: Array[Int]): Array[Int] = {
    val dst = Array.ofDim[Int](64)
    computeForward(src, data, f1)
    computeForward(data, dst, f1+1)
    dst
  }

  def inverse(src: Array[Int]): Array[Int] = {
    val dst = Array.ofDim[Int](64)
    computeInverse(src, data, r1)
    computeInverse(data, dst, r1+1)
    dst
  }
}

object Dct8SwTest {

  def main(args: Array[String]) {

    val psnrStats = new PsnrStats

    val dct = new DCT8SW(8, 8, 6)

    for (test <- 0 to 10000) {


      val data = Array.fill(64) {Random.nextInt(64) - 32
      }

  //          for(i<-0 to 63 by 9) data(i) = 123



      val res1 = dct.forward(data)
      val res2 = dct.inverse(res1)

      val psnr = res2 PSNR data
//      println(psnr)
      assert(psnr forall (psnrStats.add(_) > 40.0))
    }

    val maxVal = 256
    val F = "%.3f"

    def rmse(psnr: Double) = pow(10.0, log10(maxVal) - (psnr /20))

    val revRmse = rmse(psnrStats.average)
    println(s"REVERSE: avgPSNR=$F minPSNR=$F avg RMSE=$F" format (psnrStats.average, psnrStats.min, revRmse))

  }


}