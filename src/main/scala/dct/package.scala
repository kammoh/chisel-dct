import Chisel._

import scala.math._

package object dct {

  object Implementation extends Enumeration {
    type Implementation = Value
    val Combinational, Pipelined, Iterative = Value
  }


  def trim(node: SInt, bits: Int): SInt = {
    val out = Wire(SInt(width = bits))

    val msb = min(node.getWidth(), bits) - 1
//    println(s"width = ${node.getWidth()} trimmed to ${msb+1}")
    out := Cat(node(msb), node(msb - 1, 0)).toSInt()
    out
  }


  def MAX_VAL(outBits: Int) = SInt((1 << (outBits - 1)) - 1, width = outBits)

  def MIN_VAL(outBits: Int) = -MAX_VAL(outBits) - SInt(1, width = outBits)

  def saturate(i: SInt, bits: Int) = {
    val max = MAX_VAL(bits)
    val min = MIN_VAL(bits)
    //    println(s"saturating to [${min.litValue()}, ${max.litValue()}]")
    Mux(i > max, max, Mux(i < min, min, i))
  }

  def makeValid[T <: Data](valid: Bool, data: T) = {
    val out = Valid(data)
    out.valid <> valid
    out.bits <> data
    out.setIsTypeNode
    out
  }


  class PsnrStats {
    var sum = 0.0
    var count = 0L
    var min = Double.MaxValue

    def add(p: Double) = {
      sum += p
      count += 1L
      if (p < min) min = p
      p
    }

    def average = if (count > 0) sum / count else 100.0
  }

  def mse2Psnr(m: Double, bits:Int, n: Int) = 10.0 * log10((1 << (2*bits)).toDouble * n.toDouble * n.toDouble / m)

  implicit class MyArray(array: Array[Int]) {
    def pretty(columns: Int = 8, width: Int = 4): String = {
      val line = "-" * (columns * (width + 1)) + "\n"
      line +
        (array.map(s"%${width}d" format _).sliding(columns, columns).toList map (_ mkString " ") mkString "\n") + "\n" + line
    }

    def mse(rhs: Array[Int]): Double = {
      (this.array, rhs).zipped.map(_ - _) map(x => x * x) sum
    }

    def PSNR(rhs: Array[Int], bits: Int, n: Int = 8): Option[Double] = {
      val m = max(0.5, mse(rhs))
//      println(s"*** mse = $m, bits=$bits, n=$n")
      if (m == 0.0) None else Some(mse2Psnr(m,bits,n))
    }

    def PSNR(rhs: Array[Int]): Option[Double] = PSNR(rhs: Array[Int], 8)

  }

  /**
   *
   * Totally awesome new console progress tool from mixedbits-webframework
   * Copyright dan.shryock Nov 19, 2010
   *
   * modified by Kamyar to work in IntelliJ console
   *
   */

  object ConsoleProgress{

    def apply(label:String,width:Int,labelColor:Option[String] = None, progressColor:Option[String] = None, percentColor:Option[String] = None):Double => Unit = {
      var previousLineWidth = 0
      var complete = false

      {progress:Double =>
        if(!complete){
          //clear the old line
          print("\r" + Console.RESET + "\b" * previousLineWidth)

          //print the new line
          val barWidth = (width * progress).toInt
          val barRemainder = width - barWidth

          val labelOutput = label+": "
          val progressBar = "["+("=" * barWidth)+(" " * barRemainder)+"] "
          val percent = (math.round(progress * 1000.0)/10.0)+"%"

          labelColor foreach print
          print(labelOutput)
          if(labelColor.isDefined) print(Console.RESET)
          progressColor foreach print
          print(progressBar)
          if(progressColor.isDefined) print(Console.RESET)

          percentColor foreach print
          print(percent)
          if(percentColor.isDefined) print(Console.RESET)

          previousLineWidth = labelOutput.size + progressBar.size + percent.size

          //deal with 100% progress
          if(progress >= 1.0){
            complete = true
            println()
          }
        }
      }
    }
  }

}