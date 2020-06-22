package ieee1500

import spinal.core._
import spinal.lib._

import scala.util.Random

case class Ieee1500Intf() extends Bundle with IMasterSlave {
  val shift = Bool
  val capture = Bool
  val update = Bool
  val selectWIR = Bool
  val wsi = Bool
  val wso = Bool

  override def asMaster() : Unit = {
    out(shift, capture, update, selectWIR, wsi)
    in(wso)
  }

}

object Ieee1500State extends SpinalEnum {
  val RESET, IDLE, IR_CAPTURE, IR_SHIFT, IR_UPDATE, DR_CAPTURE, DR_SHIFT, DR_UPDATE = newElement()
}

class Ieee1500Fsm(ieee1500_intf: Ieee1500Intf) extends Area {
  import Ieee1500State._
  val stateNext = Ieee1500State()
  val state = RegNext(stateNext) randBoot()


  when(ieee1500_intf.selectWIR) {
    when(ieee1500_intf.capture) {
      stateNext := IR_CAPTURE
    }.elsewhen(ieee1500_intf.shift) {
      stateNext := IR_SHIFT
    }.elsewhen(ieee1500_intf.update) {
      stateNext := IR_UPDATE
    }.otherwise {
      stateNext := IDLE
    }
  }.otherwise {
    when(ieee1500_intf.capture) {
      stateNext := DR_CAPTURE
    }.elsewhen(ieee1500_intf.shift) {
      stateNext := DR_SHIFT
    }.elsewhen(ieee1500_intf.update) {
      stateNext := DR_UPDATE
    }.otherwise {
      stateNext := IDLE
    }
  }

}

class Ieee1500(val ieee1500_intf: Ieee1500Intf, instructionWidth: Int) extends Area with Ieee1500Access{
  val fsm = new Ieee1500Fsm(ieee1500_intf)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val bypass = Reg(Bool)

  ieee1500_intf.wso := bypass

  switch(fsm.state) {
    is(Ieee1500State.IR_CAPTURE) {
      instructionShift := instruction
    }
    is(Ieee1500State.IR_SHIFT) {
      instructionShift := (ieee1500_intf.wsi ## instructionShift) >> 1
      ieee1500_intf.wso := instructionShift.lsb
    }
    is(Ieee1500State.IR_UPDATE) {
      instruction := instructionShift
    }
    is(Ieee1500State.DR_SHIFT) {
      bypass := ieee1500_intf.wsi
    }
  }


  override def getWsi: Bool = ieee1500_intf.wsi
  override def setWso(value: Bool): Unit = ieee1500_intf.wso := value

  override def getState: Ieee1500State.C = fsm.state
  override def getInstruction(): Bits = instruction
  override def setInstruction(value: Bits): Unit = instruction := value.resized
}

trait Ieee1500Access {
  def getWsi: Bool
  def setWso(value: Bool): Unit

  def getState: Ieee1500State.C
  def getInstruction(): Bits
  def setInstruction(value: Bits): Unit

  def idcode(value: Bits)(instructionId: Bits) = new Ieee1500InstructionIdcode(value)(this,instructionId)

  def read[T <: Data](data: T)(instructionId: Bits) = new Ieee1500InstructionRead(data)(this,instructionId)

  def write[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Bits) =
    new Ieee1500InstructionWrite[T](data)(this,instructionId)
}

class Ieee1500Instruction(ieee1500Access: Ieee1500Access, val instructionId: Bits) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = ieee1500Access.getInstruction === instructionId.resized

  Component.current.addPrePopTask(() => {
    when(instructionHit) {
      when(ieee1500Access.getState === Ieee1500State.DR_CAPTURE) {
        doCapture()
      }
      when(ieee1500Access.getState === Ieee1500State.DR_SHIFT) {
        doShift()
      }
      when(ieee1500Access.getState === Ieee1500State.DR_UPDATE) {
        doUpdate()
      }
      when(ieee1500Access.getState === Ieee1500State.RESET) {
        doReset()
      }
    }
  })
}


class Ieee1500InstructionRead[T <: Data](data: T) (ieee1500Access: Ieee1500Access, instructionId: Bits) extends Ieee1500Instruction(ieee1500Access, instructionId) {
  val shifter = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (ieee1500Access.getWsi ## shifter) >> 1
    ieee1500Access.setWso(shifter.lsb)
  }

}

class Ieee1500InstructionWrite[T <: Data](data: T) (ieee1500Access: Ieee1500Access,instructionId: Bits) extends Ieee1500Instruction(ieee1500Access,instructionId) {
  val shifter, store = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (ieee1500Access.getWsi ## shifter) >> 1
    ieee1500Access.setWso(shifter.lsb)
  }

  override def doUpdate(): Unit = {
    store := shifter
  }

  data.assignFromBits(store)
}


class Ieee1500InstructionIdcode[T <: Data](value: Bits)(ieee1500Access: Ieee1500Access, instructionId: Bits) extends Ieee1500Instruction(ieee1500Access, instructionId) {
  val shifter = Reg(Bits(32 bit))
  override def doShift(): Unit = {
    shifter := (ieee1500Access.getWsi ## shifter) >> 1
    ieee1500Access.setWso(shifter.lsb)
  }

  override def doReset(): Unit = {
    shifter := value
    ieee1500Access.setInstruction(instructionId)
  }
}

class SimpleIeee1500 extends Component {
  val io = new Bundle {
    val ieee1500_intf = slave(Ieee1500Intf())
    val switches = in Bits(8 bit)
    val keys = in Bits(4 bit)
    val leds = out Bits(8 bit)
    val databus = out Bits(2048 bit)
  }

  val ieee1500 = new Ieee1500(io.ieee1500_intf, 8)
  val idcodeArea = ieee1500.idcode(B"x12345678") (instructionId=4)
  val switchesArea = ieee1500.read(io.switches)  (instructionId=5)
  val keysArea = ieee1500.read(io.keys)          (instructionId=6)
  val ledsArea = ieee1500.write(io.leds)         (instructionId=7)
  val databusArea = ieee1500.write(io.databus)   (instructionId=8)
}

object SimpleIeee1500Verilog {
  def main(args: Array[String]) {
    SpinalVerilog(new SimpleIeee1500)
  }
}

