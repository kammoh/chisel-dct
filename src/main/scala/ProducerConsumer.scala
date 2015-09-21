/**
 * Created by kamyar on 8/26/15.
 */

import Chisel._

class ProducerConsumerIO extends DecoupledIO(UInt(OUTPUT, width = 8))

class Producer0 extends Module {
  val io = new Bundle {
    val out = new ProducerConsumerIO
  }
  val produce = Counter(io.out.ready, 100)._1 << UInt(1)
  val valid = Reg(Bool())
  valid := !valid // toggle

  io.out.bits := produce
  io.out.valid := valid
  when(io.out.fire()) {
    printf("*** producer0 out is %d\n", io.out.bits)
  }
}

class Producer1 extends Module {
  val io = new Bundle {
    val out = new ProducerConsumerIO
  }
  val produce = (Counter(Bool(true), 100)._1 << UInt(1)) + UInt(1)
  io.out.bits := produce
  io.out.valid := Bool(true) // produce(2) // alternating valid every

  when(io.out.fire()) {
    printf("*** producer1 out is %d\n", io.out.bits)
  }
}

class Consumer extends Module {
  val io = new Bundle {
    val in = (new ProducerConsumerIO).flip()
    val out = new Bundle{
      val received_val = UInt(OUTPUT, 8)
    }
  }

  val b = 4

  val counter = new Counter(1<<b)
  val rand = LFSR16(io.in.fire())(b - 1,0)

  when(!io.in.ready) {
    printf("consumer not ready! counter=%d rand = %d\n", counter.value, rand)
    counter.inc()
  }


  when(io.in.fire()){
    counter.value := UInt(0)
  }


  io.out.received_val := RegEnable(io.in.bits, io.in.fire())
  io.in.ready := (counter.value === rand)
}

class ProducerConsumer(i: Int = 1) extends Module {
  val io = new Bundle {
    val chosen = UInt(OUTPUT, 2) // 1 is enough
    val consumed_val = UInt(OUTPUT, 8)
    val in_0_valid = Bool(OUTPUT)
    val in_0_ready = Bool(OUTPUT)
    val in_1_valid = Bool(OUTPUT)
    val in_1_ready = Bool(OUTPUT)
    val out_valid = Bool(OUTPUT)
    val out_ready = Bool(OUTPUT)
  }

  val producer = Vec( Module(new Producer0).io, Module(new Producer1).io)
  val consumer = Module(new Consumer).io


  val arbiter = Module(new LockingArbiter(consumer.in.bits, 2, 1, None)).io

  for(i <- 0 until 2) {
    when(arbiter.in(i).fire()) {
      printf("arbiter consumer got %d from %d chosen = %x\n", consumer.in.bits, UInt(i), arbiter.chosen )
      assert(arbiter.chosen === UInt(i), "chosen != %d" format i)
      assert(consumer.in.bits(0) === UInt(i), "chosen != %d" format i)
    }
  }
  io.chosen := arbiter.chosen
  io.in_0_valid := arbiter.in(0).valid
  io.in_1_valid := arbiter.in(1).valid
  io.in_0_ready := arbiter.in(0).ready
  io.in_1_ready := arbiter.in(1).ready
  io.out_valid := arbiter.out.valid
  io.out_ready := arbiter.out.ready

  (producer.map(_.out), arbiter.in).zipped foreach ( _ <> _ )
  arbiter.out <> consumer.in
  io.consumed_val := consumer.out.received_val
}

class ProducerConsumerTest(dut: ProducerConsumer) extends Tester(dut, isTrace = true) {

  reset()

  val t0 = t

  for(i <- 0 to 1000) {

    step(1)
    println(s"clock ${t-t0}: in(0).valid = ${peek(dut.io.in_0_valid)}," +
      s" in(1).valid = ${peek(dut.io.in_1_valid)}  -> out === in(${peek(dut.io.chosen)})" +
      s" out.valid = ${peek(dut.io.out_valid)}  out.ready = ${peek(dut.io.out_ready)} " +
      s"  consumed ${peek(dut.io.consumed_val)}")
//    expect(dut.io.out_valid, 0)

  }

}

object ProducerConsumerTest{

  def main(args: Array[String]) {
    chiselMain.run(args ++ Seq("--vcd", "--targetDir", "test", "--debug", "--ioDebug", "--genHarness", "--compile",
      "--test"), ()=> new ProducerConsumer,
      (dut:ProducerConsumer) => new ProducerConsumerTest(dut))
  }
}



