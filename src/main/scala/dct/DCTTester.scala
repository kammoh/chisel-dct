package dct

import Chisel._
import ConstMult._

import scala.math._
import scala.util.Random

/**
 * Created by kamyar on 8/25/15.
 */


class DctTester[T <: DCTModule](dut: T, mode: String, val numTests:Int = 1, isTrace: Boolean = false, val isDebug: Boolean = true) extends
Tester(dut, isTrace = isTrace) {
  def params = dut.params
  val n = dut.n
  val impl = dut.impl
  val inWidth = dut.inWidth

  if(isDebug) println ("Running in DEBUG mode")

  def debug(s: Any = ""): Unit = if(isDebug) println(s)

  val maxVal = 1 << inWidth

  val dct = new DCT8SW(dut.inWidth, dut.fwdShift, dut.invShift)


  println(s"totall add/sub/negs = ${DCTFwd.addsub + ConstMult.stats.addsub + ConstMult.stats.neg}")
  println(s"Running $numTests tests on DCT$mode $impl implementation with uniform random 8x8 8-bit inputs...")

  val undoPsnrStats = new PsnrStats
  val swPsnrStats = new PsnrStats

  val F = "%.3f"

  val progressBar = ConsoleProgress("Progress", 40, progressColor = Some(Console.YELLOW))

  val startTime = Driver.elapsedTime

  var testsDone = 0
  for (test <- 0 until numTests; if ok) {
    if(!isDebug){
      progressBar(test.toDouble/(numTests - 1).toDouble)
    }
    val inputs = Array.fill(n * n) {
      Random.nextInt(maxVal) - (maxVal >> 1)
    }
    debug("inputs:\n" + inputs.pretty())

    expect(dut.io.in.ready, 1)
    poke(dut.io.in.valid, 1)
    for (i <- 0 until n * n) {
      poke(dut.io.in.bits(i), inputs(i))
    }
    step(1)

    poke(dut.io.out.ready, 1)

    if(impl == Implementation.Iterative) {
      expect(dut.io.in.ready, 0)
      poke(dut.io.in.valid, 0)
    }

    if(impl == Implementation.Pipelined) {
      step(1)
    }

    // wait for out.valid
    var waitedForValid = 0
    while(peek(dut.io.out.valid) != BigInt(1)){
      step(1)
      waitedForValid +=1
      if(waitedForValid > 20) {
        throwException(s"waited for $waitedForValid cycles and no valid")
      }
    }

    expect(dut.io.out.valid, 1)

    val outputs = Array.fill(n * n)(0)
    for (i <- 0 until n * n) {
      outputs(i) = peek(dut.io.out.bits(i)).toInt
    }
    debug("actual output:\n" + outputs.pretty())

    val expected = (mode match {
      case "Fwd" => dct.forward _
      case "Inv" => dct.inverse _
      case "B2B" => dct.forward _ andThen dct.inverse
    })(inputs)
    debug("expected:\n" + expected.pretty())

    expect(expected PSNR(outputs, 8) map (x => {debug(s"expected vs. outputs PSNR = $F" format x);x})
      forall(swPsnrStats.add(_) > 35.0), "software check")

    val swUndo = (mode match {
      case "Fwd" => dct.inverse _
      case "Inv" => dct.forward _
      case "B2B" => identity[Array[Int]] _
    })(outputs)
    debug("swUndo:\n" + swUndo.pretty())

    def printPass(x:Option[Double]): Option[Double] = x

    expect(swUndo PSNR inputs map undoPsnrStats.add forall(x => {debug("swUndo psnr="+x); x > 35.0}), "backward software check")
    testsDone +=1
  }

  println(s"${Console.MAGENTA} Completed $testsDone tests in ${(Driver.elapsedTime - startTime)/1e3} seconds${Console.RESET}")

  def rmse(psnr: Double) = pow(10.0, log10(maxVal) - (psnr /20))

  val swrmse = rmse(swPsnrStats.average)
  println(s"SW: avgPSNR=$F minPSNR=$F RMSE=$F" format (swPsnrStats.average, swPsnrStats.min, swrmse))

  val revRmse = rmse(undoPsnrStats.average)
  println(s"REVERSE: avgPSNR=$F minPSNR=$F avg RMSE=$F" format (undoPsnrStats.average, undoPsnrStats.min, revRmse))


}
