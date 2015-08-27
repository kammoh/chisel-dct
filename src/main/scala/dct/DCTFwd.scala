package dct

import Chisel._
import ConstMult._


object DCTFwd extends DCT8 {
  val shift = 8

  def apply(n: Int, dWidth: Int, preShift1: Int, preShift2: Int)(input: Vec[SInt]): Vec[SInt] = {
    //    println(s"shift = $shift")
    val round = SInt(1 << (shift - preShift1) - 1) // TODO CHECK

    def inTrans(i: Int)(j: Int): SInt = {
      val out = Wire(SInt(width = dWidth + 1))
      out := input(i * 8 + j)
      out
    }

    val a = Vec(Array.tabulate(n) { i => Vec(Array.tabulate(n)(k => {
      val kk = k % 2 + (k / 4) * 2
      addsub += 1
      if ((k / 2) % 2 == 0) inTrans(i)(kk) +& inTrans(i)(7 - kk) else inTrans(i)(kk) -& inTrans(i)(7 - kk)
    }))
    })

    val b = Vec(Array.tabulate(n) { i => Vec(Array.tabulate(n)(k => {
      addsub += 1
      if (k > 1) a(i)(k % 2) -& a(i)(5 - k % 2) else a(i)(k % 2) +& a(i)(5 - k % 2)
    }))
    })

    val b_width = b.toArray.flatten.map(_.getWidth()).max

    Vec(Array.tabulate(n) { i => Vec(Array.tabulate(n) { j =>
      (w.slice(i * n, i * n + 4).filter(_ != 0), Seq(0, 1, 4, 5).map { d =>
        if (i % 2 == 0)
          b(j)(if (i % 4 == 0) d else d + 2)
        else
          a(j)(d + 2)
      }).zipped.map((w, s) => trim((w *& s) >> preShift1, s.getWidth() + log2Floor(math.abs(w)) + 1 - preShift1)).
        foldLeft(round)((sum, next) => {
        addsub += 1
        trim((next >> preShift2) +& sum, b_width + dWidth - 1 - preShift2 - preShift1)
      }) >> (shift - preShift1 - preShift2)

    })
    }.flatten)
  }
}

class DCTFwd extends DCTModuleImpl {
  val shift: Int = DCTFwd.shift

  def singleStage = DCTFwd(n, dWidth, preShift1, preShift2)

  def firstIter = (in: Vec[SInt]) => in

  def secondIter = (in: Vec[SInt]) => Vec(in.map(word => word >> 1))

}