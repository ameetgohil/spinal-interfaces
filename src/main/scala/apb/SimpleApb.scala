package apb


import scala.util.Random

case class ApbIntf() extends Bundle with IMasterSlave {
  val psel = Bool
  val penable = Bool
  val paddr = Bits(32 bit)
  val pwrite = Bool
  val pwdata = Bits(32 bit)
  val prdata = Bits(32 bit)
  val pready = Bool
  val pslverr = Bool

  override def asMaster() : Unit = {
    out(psel, penable, paddr, pwrite, pwdata)
    in(prdata, pready, pslverr)
  }
}

object ApbState extends SpinalEnum {
  val IDLE, WRITE, READ = newElement()
}

class ApbFsm(apbIntf: ApbIntf) extends Area {
  import ApbState._
  val stateNext = ApbState()
  val state = RegNext(stateNext) init(IDLE)

  when(apbIntf.psel && apbIntf.penable && apbIntf.pwrite) {
  }
}

class Apb(val apbIntf: ApbIntf) extends Area with ApbAcess {

}

class ApbOp(apbAccess: ApbAccess, val addr: Bits) extends Area {
  def doWrite(): Unit = {}
  def doRead(): Unit = {}

  val opHit = apbAccess === addr
}

trait ApbAccess {

  def idcode(value: Bits)(addrId: Bits) = new ApbIdcode(value)(this, addrId)

  def read[T <: Data](data: T)(addr: Bits) = new AppOpRead[T](data)(this, addr)

  def write[T <: Data](data: T)(addr: Bits) = new ApbOpWrite[T](data)(this, addr)
}


class ApbRead[T <: Data](data: T)(apbAccess: ApbAccess, addr: Bits) extends ApbOp(apbAccess, addr) {

  override def doRead(): Unit = {
  }

}

class ApbWrite[T <: Data](data: T)(apbAccess: ApbAccess, addr: Bits) extends 
