// Copyright 2021 Howard Lau
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package riscv.core

import chisel3._
import chisel3.util.{Cat, MuxLookup}
import riscv.Parameters

class Execute extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(Parameters.InstructionWidth))
    val instruction_address = Input(UInt(Parameters.AddrWidth))
    val reg1_data = Input(UInt(Parameters.DataWidth))
    val reg2_data = Input(UInt(Parameters.DataWidth))
    val immediate = Input(UInt(Parameters.DataWidth))
    val aluop1_source = Input(UInt(1.W))
    val aluop2_source = Input(UInt(1.W))
    val csr_reg_read_data = Input(UInt(Parameters.DataWidth))

    val mem_alu_result = Output(UInt(Parameters.DataWidth))
    val csr_reg_write_data = Output(UInt(Parameters.DataWidth))
    val if_jump_flag = Output(Bool())
    val if_jump_address = Output(UInt(Parameters.DataWidth))
  })

  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val funct7 = io.instruction(31, 25)

  val alu = Module(new ALU)
  val alu_ctrl = Module(new ALUControl)

  alu_ctrl.io.opcode := opcode
  alu_ctrl.io.funct3 := funct3
  alu_ctrl.io.funct7 := funct7
  alu.io.func := alu_ctrl.io.alu_funct
  alu.io.op1 := Mux(
    io.aluop1_source === ALUOp1Source.InstructionAddress,
    io.instruction_address,
    io.reg1_data,
  )
  alu.io.op2 := Mux(
    io.aluop2_source === ALUOp2Source.Immediate,
    io.immediate,
    io.reg2_data,
  )
  io.if_jump_flag := opcode === Instructions.jal ||
    (opcode === Instructions.jalr) ||
    (opcode === InstructionTypes.B) && MuxLookup(
      funct3,
      false.B,
      IndexedSeq(
        InstructionsTypeB.beq -> (io.reg1_data === io.reg2_data),
        InstructionsTypeB.bne -> (io.reg1_data =/= io.reg2_data),
        InstructionsTypeB.blt -> (io.reg1_data.asSInt < io.reg2_data.asSInt),
        InstructionsTypeB.bge -> (io.reg1_data.asSInt >= io.reg2_data.asSInt),
        InstructionsTypeB.bltu -> (io.reg1_data.asUInt < io.reg2_data.asUInt),
        InstructionsTypeB.bgeu -> (io.reg1_data.asUInt >= io.reg2_data.asUInt)
      )
    )
  io.if_jump_address := io.immediate + Mux(opcode === Instructions.jalr, io.reg1_data, io.instruction_address)
  io.mem_alu_result := alu.io.result
  //lab2(CLINTCSR)
  /*
  io.csr_reg_write_data :=
  */
  // ================== lab2(CLINTCSR)：CSR 指令运算 ==================
  // rs1 字段（既是寄存器编号，也是 *I 指令里的 5bit 立即数 zimm）
  val rs1 = io.instruction(19, 15)

  // 这是 CSR 指令吗？
  val isCSR = opcode === "h73".U(7.W)

  // 立即数版本用到的 zimm（5bit），在位运算时会自动零扩展
  val zimm = rs1.asUInt

  // 默认写回 0（非 CSR 指令时不会用到）
  io.csr_reg_write_data := 0.U

  when(isCSR) {
    // ---------- CSRRW：csr <- rs1 ----------
    when(funct3 === InstructionsTypeCSR.csrrw) {
      io.csr_reg_write_data := io.reg1_data
    }

      // ---------- CSRRS：csr <- csr | rs1 （rs1=0 只读） ----------
      .elsewhen(funct3 === InstructionsTypeCSR.csrrs) {
        io.csr_reg_write_data := Mux(
          io.reg1_data =/= 0.U,
          io.csr_reg_read_data | io.reg1_data,
          io.csr_reg_read_data
        )
      }

      // ---------- CSRRC：csr <- csr & ~rs1 （rs1=0 只读） ----------
      .elsewhen(funct3 === InstructionsTypeCSR.csrrc) {
        io.csr_reg_write_data := Mux(
          io.reg1_data =/= 0.U,
          io.csr_reg_read_data & (~io.reg1_data).asUInt,
          io.csr_reg_read_data
        )
      }

      // ---------- CSRRWI：csr <- zimm ----------
      .elsewhen(funct3 === InstructionsTypeCSR.csrrwi) {
        val zimmExt = zimm   // 5bit，Chisel 自动零扩展到 DataWidth
        io.csr_reg_write_data := zimmExt
      }

      // ---------- CSRRSI：csr <- csr | zimm （zimm=0 只读） ----------
      .elsewhen(funct3 === InstructionsTypeCSR.csrrsi) {
        val zimmExt = zimm
        io.csr_reg_write_data := Mux(
          zimmExt =/= 0.U,
          io.csr_reg_read_data | zimmExt,
          io.csr_reg_read_data
        )
      }

      // ---------- CSRRCI：csr <- csr & ~zimm （zimm=0 只读） ----------
      .elsewhen(funct3 === InstructionsTypeCSR.csrrci) {
        val zimmExt = zimm                      // 低 5 位是立即数，其余位自动补 0
        io.csr_reg_write_data := Mux(
          zimmExt =/= 0.U,
          io.csr_reg_read_data & (~zimmExt).asUInt,
          io.csr_reg_read_data
        )
      }
  }
  // ================== lab2(CLINTCSR) 结束 ==================

}
