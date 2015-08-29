package dct

/**
 * Created by kamyar on 8/25/15.
 */

import Chisel._
import ConstMult._

object DCTInv extends DCT8 {
  def apply(n: Int, inWidth:Int, preShift1: Int, preShift2: Int, shift:Int)(input: Vec[SInt]): Vec[SInt] = {
    val round = SInt(1 << (shift - preShift1 - preShift2 -1))
    println(s"DCTInv with shift = $shift round =${round.litValue()}")

    def in(i: Int)(j: Int): SInt = {
      val out = Wire(SInt(width = inWidth + 1))
      out := input(i + 8 * j)
      out
    }

    val a = Vec(Array.tabulate(n) { i => Vec(Array.tabulate(n)(j => {
      //j match {
      //0>    2              (w(8)  *& in(i)(1))  20             (w(24) *& in(i)(3))  34             (w(40) *& in(i)(5))  52             (w(56) *& in(i)(7))
      //1>    3              (w(9)  *& in(i)(1))  21             (w(25) *& in(i)(3))  35             (w(41) *& in(i)(5))  53             (w(57) *& in(i)(7))
      //2>    4              (w(10) *& in(i)(1))  22             (w(26) *& in(i)(3))  36             (w(42) *& in(i)(5))  54             (w(58) *& in(i)(7))
      //3>    5              (w(11) *& in(i)(1))  23             (w(27) *& in(i)(3))  37             (w(43) *& in(i)(5))  55             (w(59) *& in(i)(7))
      //4>    6                 12             (w(16) *& in(i)(2))  28                38                44             (w(48) *& in(i)(6))  60
      //5>    7                 13             (w(17) *& in(i)(2))  29                39                45             (w(49) *& in(i)(6))  61
      //6> (w(0)  *& in(i)(0))  14                18                30             (w(32) *& in(i)(4))  46                50                62
      //7> (w(1)  *& in(i)(0))  15                19                31             (w(33) *& in(i)(4))  47                51                63
      //}6

      (0 until n).map(c => {
        def cc = {
          val cc4 = c % 4
          if (cc4 % 2 == 0) 6 - cc4 else 0
        }
        n * c + (j + n - cc) % n
      }).map(w(_)).zipWithIndex.filter(_._1 != 0).map {
        case (const, idx) => trim((const *& in(i)(idx)) >> preShift1, inWidth + log2Floor(math.abs(const)) + 2 - preShift1)
      }.reduce(_ +& _)
    }))
    })

    val b = Vec(Array.tabulate(n) { i => Vec(Array.tabulate(4) {
      case 0 => a(i)(6) +& a(i)(4)
      case 1 => a(i)(7) +& a(i)(5)
      case 2 => a(i)(6) -& a(i)(4)
      case 3 => a(i)(7) -& a(i)(5)
    })
    })

    val c = Vec(Array.tabulate(n) { i => Vec(Array.tabulate(n) {
      case 0 => b(i)(0) +& a(i)(0)
      case 1 => b(i)(1) +& a(i)(1)
      case 2 => b(i)(3) +& a(i)(2)
      case 3 => b(i)(2) +& a(i)(3)
      case 4 => b(i)(2) -& a(i)(3)
      case 5 => b(i)(3) -& a(i)(2)
      case 6 => b(i)(1) -& a(i)(1)
      case 7 => b(i)(0) -& a(i)(0)
    } .map(s => trim(s>> preShift2, 16 - preShift1 - preShift2) +& round >> (shift - preShift1-preShift2)))
    })

    Vec(c.toArray.flatten)
  }

}

class DCTInv extends DCTModuleImpl {
  def singleStage = DCTInv(n = n, inWidth = inWidth, preShift1, preShift2, shift = invShift)

  def firstIter = (in: Vec[SInt]) => in

  def secondIter = (in: Vec[SInt]) => Vec(in map (word => {
    val msb = word.getWidth() - 1
    if (doSaturate) {
      Mux(word(msb), Mux( !word(msb-1,8).andR, SInt(-128), Cat(word(msb), word(7,1)).toSInt ),
        Mux(word(msb,8).orR(), SInt(127), word(8,1).toSInt))
    }
    else
      trim(word >> 1, outWidth)
  }))
}
