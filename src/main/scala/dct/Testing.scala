package dct

/**
 * Created by kamyar on 8/25/15.
 */

import collection.mutable

import Chisel._

import scala.reflect.runtime.universe._


class ArgsParam(args: Array[String]) {
  val local = mutable.Set.empty[String]

  def apply[T](name: String)(implicit tag: TypeTag[T]): Option[T] = {
    val i = args.lastIndexOf("--" + name)
    if (i != -1) {
      val str = if (i == args.length - 1) "" else if (args(i + 1).startsWith("--")) "" else args(i + 1)


      val out = if (typeTag[T] == typeTag[String] || typeTag[T] == typeTag[java.lang.String]) str
      else typeTag[T] match {
        case TypeTag.Int => str.toInt
        case TypeTag.Double => str.toDouble
        case TypeTag.Boolean => if (str.isEmpty) true else str.toBoolean

        case TypeTag.Object => str
        case TypeTag.Nothing => str
      }
      local += args(i)
      Some(out.asInstanceOf[T])

    } else {
      //      println(s"$name not found in ${args.toList}")
      None
    }
  }

  def apply[T](name: String, default: T)(implicit tag: TypeTag[T]): T = apply[T](name) getOrElse default

  def nonProcessed = {
    val out = mutable.ArrayBuffer.empty[String]
    var prevLocal = false
    for (arg <- args) {
      if (arg.startsWith("--")) {
        if (local.contains(arg))
          prevLocal = true
        else
          prevLocal = false
      }
      else if (prevLocal)
        prevLocal = false
      else out += arg
    }
    out.toArray
  }
}

object TestGenerator extends App {
  val params = new ArgsParam(args)

  val backend = params("backend", "c")
  val mode = if (params("inv", false)) "Inv" else if (params("fwd", false)) "Fwd" else params("mode") getOrElse "B2B"
  val debug = params("Debug", false)
  val numTests = params("numTests", 10)

  if (true) {

    val CONFIG = "DCTConfig"
    val PROJECT = "dct"

    val chiselArgs = Array("--W0W", "--minimumCompatibility", "3.0.0",
      "--backend", backend, "--targetDir", backend,
      "--compile",
      "--genHarness", "--test", "--configInstance", s"$PROJECT.$CONFIG") ++
      (if (!debug) Array("--noIoDebug") else Array("--vcd", "--debug", "--ioDebug"))

    val gen = () => Class.forName(PROJECT + "." + "DCT" + mode).newInstance().asInstanceOf[DCTModule]

    chiselMain.run(params.nonProcessed ++ chiselArgs, gen, (dut: DCTModule) =>
      new DctTester(dut, mode, numTests = numTests, isDebug = debug, isTrace = debug))
  }
}