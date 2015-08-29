package dct

import Chisel._
import dct.Implementation._


trait TopLevelParameters extends UsesParameters {
  val n = params(N)
  val impl = params(Impl)
  val inWidth = params(InWidth)
  val outWidth = params(OutWidth)
}

trait TopLevelModuleParameters extends Module with TopLevelParameters{
  def modParams(dCTModule: TopLevelModuleParameters) =  (
    if(Driver.parStack.isEmpty) Parameters.empty
    else Driver.parStack.top).
    alter((pname,site,here,up) => pname match {
    case ModName => this.getClass.getSimpleName
  })
  implicit override lazy val params = modParams(this)
}

class DCT8SIntIO(n:Int, inWidth: Int, outWidth: Int) extends Bundle {
  val in = Decoupled(Vec.fill(n*n)(SInt(INPUT, width = inWidth))).flip()
  val out = Decoupled(Vec.fill(n*n)(SInt(INPUT, width = outWidth)))
}

trait DCTModule extends Module with TopLevelModuleParameters {
  ConstMult.ConstMult.OptimizeMCM = params(OptimizeMCM) // TODO
  val fwdShift = params(FwdShift)
  val invShift = params(InvShift)

  val io = new DCT8SIntIO(n,inWidth, outWidth)
}

trait DCTModuleImpl extends DCTModule {
  def singleStage: Vec[SInt] => Vec[SInt]
  def firstIter: Vec[SInt] => Vec[SInt]
  def secondIter: Vec[SInt] => Vec[SInt]

  val doSaturate = params(Saturate)
  val preShift1 = params(PreShift1)
  val preShift2 = params(PreShift2)

  println(s"Implementing ${this.getClass.getSimpleName} as $impl inWidth=$inWidth outWidth=$outWidth")
  if(impl == Iterative) {
    val done = Reg(init = Bool(false), next = Reg(init = Bool(false), next = io.in.fire()) && !io.out.fire())
    val ready = Reg(init = Bool(true), next = !io.in.fire() && io.in.ready)

    io.in.ready := ready || io.out.fire()
    io.out.valid := done

    val fire = io.in.fire()

    val reg = Reg(Vec.fill(n*n)(SInt(width = outWidth + 1)))

    val out1 = singleStage( Mux(fire, io.in.bits, reg))

    when(fire){
      reg :=  firstIter(out1)
    }
    when(!done && !ready) {
      reg := secondIter(out1)
    }

    io.out.bits := reg
  } else {
    io.in.ready := Bool(true)
    val p = Pipe (io.in.valid, firstIter(singleStage(io.in.bits)), if (impl == Pipelined) 1 else 0)
    io.out <> makeValid(p.valid, secondIter(singleStage(p.bits)))
  }
}

class DCTB2B extends DCTModule {
  val fwd = Module(new DCTFwd).io
  val inv = Module(new DCTInv).io

  fwd.in <> io.in
  inv.in <> fwd.out
  io.out <> inv.out
}
