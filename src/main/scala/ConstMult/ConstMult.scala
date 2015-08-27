/**
 * Created by kamyar on 8/22/15.
 */

package ConstMult

import java.io.{BufferedReader, ByteArrayInputStream, InputStreamReader}

import Chisel._
import com.sun.jna.{Native, NativeLibrary}

import scala.collection.mutable
import scala.util.Random


object ACM {
  NativeLibrary.addSearchPath("acm", "src/main/cpp")
  Native.register("acm")

  val MaxOutSize = 2000

  @native def jnaCall(num: Int, consts: Array[Int], buf_size: Int, out_buffer: Array[Byte]): Int

  def apply(const: Array[Int]): Array[Byte] = {
    val buffer = Array.fill[Byte](MaxOutSize)(0)
    val ret = jnaCall(const.length, const, buffer.length, buffer)
    buffer.slice(0, ret)
  }
}

object ConstMult {
  var OptimizeMCM = true

  object stats {
    var addsub = 0
    var shift = 0
    var neg = 0
  }

  type Junction = (Int, SInt, SInt)
  val pairs = mutable.Set.empty[Junction]

  def apply(const: Int, node: SInt): SInt = {
    val out = Wire(SInt(width = node.getWidth() + log2Floor(math.abs(const)) + 1))
    //    println(const , node._id)
    pairs += ((const, node, out))
    out
  }

  object Ops extends Enumeration(1) {
    val ADD, SUB,  SHL,  SHR,  CMUL,  NEG,  ADDSHL,  SUBSHL,  SHLSUB = Value
  }

  def eval(): Unit = {
    if (!OptimizeMCM)
      pairs.foreach { case (const: Int, node: SInt, out: SInt) => out := SInt(const) * node }
    else {

      pairs.groupBy { case s: Junction => s._2._id }.values.foreach { case set: mutable.HashSet[Junction] => {
        val multNodesMap = mutable.Map[Int, SInt](0 -> set.head._2) // add input first
        val reader = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(ACM(set.map(_._1).toArray))))

        val commentPat = """^// (.*)""".r
        val outputsPat = """^outputs: (.*)""".r
        val costPat = """^cost: (.*)""".r
        val opPat = """^op: (.*)""".r
        val emptyPat = """\s+""".r
        var outMap = Map[Int, Int]()
        reader.lines().toArray.foreach({ case line: String => line match {
          case commentPat(c) => // println("## " + c)
          case costPat(c) => {
            val l = c.split("\\s+")
            stats.addsub += l(0).toInt
            stats.shift += l(1).toInt
            stats.neg += l(2).toInt
          }
          case outputsPat(c) => {
            outMap = c.split("\\s+").filter(_.nonEmpty).map(tup => {
              val t = tup.split(",")
              (t(1).toInt, t(0).toInt)
            }).toMap
          }

          case opPat(c) => {
            val op = c.split("\\s+").filter(_.nonEmpty).map(_.toInt)
            val dest :: op_type :: r1 :: r2 :: sh :: cc :: Nil = op.toList
            val node = Ops(op_type) match {
                case Ops.ADD => multNodesMap(r1) +& multNodesMap(r2)
                case Ops.SUB => multNodesMap(r1) -& multNodesMap(r2)
                case Ops.SHL => multNodesMap(r1) << sh
                case Ops.SHR => multNodesMap(r1) >> sh
                case Ops.NEG => -multNodesMap(r1)
              }
            multNodesMap += dest -> node
          }
          case emptyPat() =>
          case l => println("unknown line >>> " + l)
        }
        })

        set.foreach { case (w: Int, in: SInt, out: SInt) => out := multNodesMap(outMap(w)) }

      }
      }
    }
  }

  def transform(m: Module): Unit = {
    def info(t: String, m: String): Unit = {
      def tag(name: String, color: String): String =
        s"[${color}${name}${Console.RESET}]"
      println(tag(t, Console.CYAN) + " [%2.3f] ".format(Driver.elapsedTime / 1e3) + m)
    }

    stats.addsub = 0
    stats.shift = 0
    stats.neg = 0

    if(OptimizeMCM) {
      info("INFO", s"optimizing MCMs for module ${m.moduleName}")
      eval()
      info("DONE", s"converted ${pairs.size} MCM(s) into ${stats.addsub} add/sub(s), ${stats.shift} shift(s)" +
        s" and ${stats.neg} negation(s)")
    } else {
      eval()
      info("WARN", s"not optimizing MCMs for module ${m.moduleName}")
    }
    pairs.clear()
  }

  def addTransform() = Module.backend.transforms += transform

  addTransform()
}

class ConstMult(val const: Int, val width: Int = 8) extends Module {
  val io = new Bundle {
    val in = SInt(INPUT, width)
    val out = SInt(OUTPUT, width + log2Floor(math.abs(const)) + 1)
  }

  io.out := ConstMult(const, io.in)

}

class ConstMultTest(dut: ConstMult) extends Tester(dut, isTrace = false) {
  println(s"const = ${dut.const} inbits=${dut.width} outbits=${dut.io.out.getWidth()}")
  reset(1)
  for (test <- 0 to 10000; if ok) {
    val in = rnd.nextInt(1 << dut.width) - (1 << (dut.width - 1))

    println("input was " + in)
    poke(dut.io.in, in)
    step(1)
    val out = peek(dut.io.out).toInt
    val c = dut.const
    val expected = dut.const * in
    println(s"output was $out and expected $expected")
    expect(out == expected, "FAILED!")
  }
}

object ConstMultTest {
  def main(args: Array[String]) {
    val backend = if (args.isEmpty) "c" else args(0)
    val chiselArgs = Array("--W0W", "--minimumCompatibility", "3.0.0",
      "--backend", backend, "--targetDir", backend,
      "--compile",
      "--genHarness", "--test") ++
      (if(true) Array("--noIoDebug") else Array("--vcd", "--debug", "--ioDebug") )
    val max = 1 << 62

    for (metaTest <- 0 to 1000) {
      println(s"metaTest: $metaTest")

      chiselMain.run(args.drop(1) ++ chiselArgs
        , () => {
          ConstMult.addTransform()
          new ConstMult(Random.nextInt(max) - max / 2)}, (dut:ConstMult) => new ConstMultTest(dut) )
    }
  }
}