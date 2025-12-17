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
import chisel3.util._
import riscv.Parameters

object InterruptCode {
  val None = 0x0.U(8.W)
  val Timer0 = 0x1.U(8.W)
  val Ret = 0xFF.U(8.W)
}

object InterruptEntry {
  val Timer0 = 0x4.U(8.W)
}


class CSRDirectAccessBundle extends Bundle {
  val mstatus = Input(UInt(Parameters.DataWidth))
  val mepc = Input(UInt(Parameters.DataWidth))
  val mcause = Input(UInt(Parameters.DataWidth))
  val mtvec = Input(UInt(Parameters.DataWidth))

  val mstatus_write_data= Output(UInt(Parameters.DataWidth))
  val mepc_write_data= Output(UInt(Parameters.DataWidth))
  val mcause_write_data= Output(UInt(Parameters.DataWidth))

  val direct_write_enable = Output(Bool())
}

// Core Local Interrupt Controller
class CLINT extends Module {
  val io = IO(new Bundle {
    // Interrupt signals from peripherals
    val interrupt_flag = Input(UInt(Parameters.InterruptFlagWidth))

    val instruction = Input(UInt(Parameters.InstructionWidth))
    val instruction_address = Input(UInt(Parameters.AddrWidth))

    val jump_flag = Input(Bool())
    val jump_address = Input(UInt(Parameters.AddrWidth))

    val interrupt_handler_address = Output(UInt(Parameters.AddrWidth))
    val interrupt_assert = Output(Bool())

    val csr_bundle = new CSRDirectAccessBundle
  })
  val interrupt_enable = io.csr_bundle.mstatus(3)
  val instruction_address = Mux(
    io.jump_flag,
    io.jump_address,
    io.instruction_address + 4.U,
  )
  //lab2(CLINTCSR)
  /*
  val interrupt_enable =

  when(io.interrupt_flag =/= InterruptCode.None && interrupt_enable) {
    io.csr_bundle.mstatus_write_data :=
    io.csr_bundle.mepc_write_data :=
    io.csr_bundle.mcause_write_data :=
    io.csr_bundle.direct_write_enable :=
    io.interrupt_assert :=
    io.interrupt_handler_address :=
  }
  .elsewhen(io.instruction === InstructionsEnv.ebreak || io.instruction === InstructionsEnv.ecall) {
    ......
  }
  .elsewhen(io.instruction === InstructionsRet.mret) {
    ......
  }.otherwise {
    ......
  }
   */

  // ================== lab2(CLINTCSR) ==================
  // 1. 获取中断使能状态 (MIE 位在 mstatus 的第 3 位)
  // 2. 确定返回地址 (MEPC)
  val next_pc = instruction_address

  // 3. 默认输出配置
  io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
  io.csr_bundle.mepc_write_data := io.csr_bundle.mepc
  io.csr_bundle.mcause_write_data := io.csr_bundle.mcause
  io.csr_bundle.direct_write_enable := false.B
  io.interrupt_assert := false.B
  io.interrupt_handler_address := 0.U

  // 4. 中断/异常处理逻辑
  when(io.interrupt_flag =/= InterruptCode.None && interrupt_enable) {
    // --- 处理外设中断 (Timer / External) ---
    io.csr_bundle.mepc_write_data := next_pc // 保存下一条指令地址(包含跳转目标)

    // 设置 MCAUSE (Bit 31 = 1 表示中断，低位表示类型)
    // 假设 interrupt_flag(0) 是 Timer 中断 (0x80000007)，否则是 External (0x8000000B)
    io.csr_bundle.mcause_write_data := Mux(io.interrupt_flag(0), 0x80000007L.U, 0x8000000BL.U)

    // 更新 MSTATUS:
    // MPP(12:11)=11(Machine), MPIE(7)=MIE(old), MIE(3)=0
    io.csr_bundle.mstatus_write_data := Cat(
      io.csr_bundle.mstatus(31, 13),
      3.U(2.W),                      // MPP = 11 (Machine Mode)
      io.csr_bundle.mstatus(10, 8),
      io.csr_bundle.mstatus(3),      // MPIE = 旧的 MIE
      io.csr_bundle.mstatus(6, 4),
      0.U(1.W),                      // MIE = 0 (关中断)
      io.csr_bundle.mstatus(2, 0)
    )
    //跳转到中断向量表地址
    io.csr_bundle.direct_write_enable := true.B
    io.interrupt_assert := true.B
    io.interrupt_handler_address := io.csr_bundle.mtvec
  }
    .elsewhen(io.instruction === InstructionsEnv.ecall || io.instruction === InstructionsEnv.ebreak) {
      // --- 处理同步异常 (Ecall / Ebreak) ---
      io.csr_bundle.mepc_write_data := next_pc // 异常返回地址

      // 设置 MCAUSE: Ecall(11), Ebreak(3)
      // 注意：异常的 MCAUSE 最高位为 0
      io.csr_bundle.mcause_write_data := Mux(io.instruction === InstructionsEnv.ecall, 11.U, 3.U)

      // MSTATUS 更新同上
      io.csr_bundle.mstatus_write_data := Cat(
        io.csr_bundle.mstatus(31, 13),
        3.U(2.W),
        io.csr_bundle.mstatus(10, 8),
        io.csr_bundle.mstatus(3),
        io.csr_bundle.mstatus(6, 4),
        0.U(1.W),
        io.csr_bundle.mstatus(2, 0)
      )

      io.csr_bundle.direct_write_enable := true.B
      io.interrupt_assert := true.B
      io.interrupt_handler_address := io.csr_bundle.mtvec
    }
    .elsewhen(io.instruction === InstructionsRet.mret) {
      // --- 处理中断返回 (mret) ---
      // 恢复状态: MIE=MPIE, MPIE=1, MPP=11(或保持不变/恢复为U模式，此处简化设为11)
      io.csr_bundle.mstatus_write_data := Cat(
        io.csr_bundle.mstatus(31, 13),
        3.U(2.W),                      // MPP
        io.csr_bundle.mstatus(10, 8),
        1.U(1.W),                      // MPIE = 1
        io.csr_bundle.mstatus(6, 4),
        io.csr_bundle.mstatus(7),      // MIE = 旧的 MPIE
        io.csr_bundle.mstatus(2, 0)
      )

      io.csr_bundle.direct_write_enable := true.B
      io.interrupt_assert := true.B
      // 返回到 MEPC 指定的地址
      io.interrupt_handler_address := io.csr_bundle.mepc
    }
  // ================== lab2(CLINTCSR) End ==================


}
