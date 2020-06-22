package jtag

import spinal.core._
import spinal.lib._

import scala.util.Random

case class Jtag() extends Bundle with IMasterSlave {
  val tms = Bool
  val tdi = Bool
  val tdo = Bool

  override def asMaster() : Unit = {
    out(tdi, tms)
    in(tdo)
  }
}

object JtagState extends SpinalEnum {
  val RESET, IDLE, IR_SELECT, IR_CAPTURE, IR_SHIFT, IR_EXIT1, IR_PAUSE, IR_EXIT2, IR_UPDATE, DR_SELECT, DR_CAPTURE, DR_SHIFT, DR_EXIT1, DR_PAUSE, DR_EXIT2, DR_UPDATE = newElement()
}

class JtagFsm(jtag: Jtag) extends Area {
  import JtagState._
  val stateNext = JtagState()
  val state = RegNext(stateNext) randBoot()

  stateNext := state.mux(
    default    -> (jtag.tms ? RESET     | IDLE),           //RESET
    IDLE       -> (jtag.tms ? DR_SELECT | IDLE),
    IR_SELECT  -> (jtag.tms ? RESET     | IR_CAPTURE),
    IR_CAPTURE -> (jtag.tms ? IR_EXIT1  | IR_SHIFT),
    IR_SHIFT   -> (jtag.tms ? IR_EXIT1  | IR_SHIFT),
    IR_EXIT1   -> (jtag.tms ? IR_UPDATE | IR_PAUSE),
    IR_PAUSE   -> (jtag.tms ? IR_EXIT2  | IR_PAUSE),
    IR_EXIT2   -> (jtag.tms ? IR_UPDATE | IR_SHIFT),
    IR_UPDATE  -> (jtag.tms ? DR_SELECT | IDLE),
    DR_SELECT  -> (jtag.tms ? IR_SELECT | DR_CAPTURE),
    DR_CAPTURE -> (jtag.tms ? DR_EXIT1  | DR_SHIFT),
    DR_SHIFT   -> (jtag.tms ? DR_EXIT1  | DR_SHIFT),
    DR_EXIT1   -> (jtag.tms ? DR_UPDATE | DR_PAUSE),
    DR_PAUSE   -> (jtag.tms ? DR_EXIT2  | DR_PAUSE),
    DR_EXIT2   -> (jtag.tms ? DR_UPDATE | DR_SHIFT),
    DR_UPDATE  -> (jtag.tms ? DR_SELECT | IDLE)
  )
}

class JtagTap(val jtag: Jtag, instructionWidth: Int) extends Area with JtagTapAccess{
  val fsm = new JtagFsm(jtag)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val bypass = Reg(Bool)

  jtag.tdo := bypass

  switch(fsm.state) {
    is(JtagState.IR_CAPTURE) {
      instructionShift := instruction
    }
    is(JtagState.IR_SHIFT) {
      instructionShift := (jtag.tdi ## instructionShift) >> 1
      jtag.tdo := instructionShift.lsb
    }
    is(JtagState.IR_UPDATE) {
      instruction := instructionShift
    }
    is(JtagState.DR_SHIFT) {
      bypass := jtag.tdi
    }
  }

  override def getTdi: Bool = jtag.tdi
  override def setTdo(value: Bool): Unit = jtag.tdo := value
  override def getTms: Bool = jtag.tms

  override def getState: JtagState.C = fsm.state
  override def getInstruction(): Bits = instruction
  override def setInstruction(value: Bits): Unit = instruction := value.resized
}

trait JtagTapAccess {
  def getTdi : Bool
  def getTms : Bool
  def setTdo(value : Bool) : Unit

  def getState : JtagState.C
  def getInstruction() : Bits
  def setInstruction(value : Bits) : Unit

  def idcode(value: Bits)(instructionId: Bits) = new JtagInstructionIdcode(value)(this,instructionId)

  def read[T <: Data](data: T)(instructionId: Bits) = new JtagInstructionRead(data)(this,instructionId)

  def write[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Bits) =
    new JtagInstructionWrite[T](data)(this,instructionId)
}

class JtagInstruction(tap: JtagTapAccess, val instructionId: Bits) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = tap.getInstruction === instructionId.resized

  Component.current.addPrePopTask(() => {
    when(instructionHit) {
      when(tap.getState === JtagState.DR_CAPTURE) {
        doCapture()
      }
      when(tap.getState === JtagState.DR_SHIFT) {
        doShift()
      }
      when(tap.getState === JtagState.DR_UPDATE) {
        doUpdate()
      }
    }
    when(tap.getState === JtagState.RESET) {
      doReset()
    }
  })
}

class JtagInstructionRead[T <: Data](data: T) (tap: JtagTapAccess, instructionId: Bits) extends JtagInstruction(tap, instructionId) {
  val shifter = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }

}

class JtagInstructionWrite[T <: Data](data: T) (tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId) {
  val shifter, store = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }

  override def doUpdate(): Unit = {
    store := shifter
  }

  data.assignFromBits(store)
}

class JtagInstructionIdcode[T <: Data](value: Bits)(tap: JtagTapAccess, instructionId: Bits) extends JtagInstruction(tap,instructionId) {
  val shifter = Reg(Bits(32 bit))

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }

  override def doReset(): Unit = {
    shifter := value
    tap.setInstruction(instructionId)
  }
}


class SimpleJtagTap extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val switches = in Bits(8 bit)
    val keys = in Bits(4 bit)
    val leds = out Bits(8 bit)
    val databus = out Bits(2048 bit)
  }

  val tap = new JtagTap(io.jtag, 8)
  val idcodeArea = tap.idcode(B"x87654321") (instructionId=4)
  val switchesArea = tap.read(io.switches)  (instructionId=5)
  val keysArea = tap.read(io.keys)          (instructionId=6)
  val ledsArea = tap.write(io.leds)         (instructionId=7)
  val databusArea = tap.write(io.databus)   (instructionId=8)
}

object SimpleJtagTapVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new SimpleJtagTap)
  }
}
