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

  // 1. 从 mstatus 中取出全局中断使能 MIE 位
  // mstatus[3] = MIE, mstatus[7] = MPIE

  // 2. 有外设中断 (interrupt_flag != None) 且 MIE=1，则进入中断入口
  when (io.interrupt_flag =/= InterruptCode.None && interrupt_enable) {
    // 关中断：MIE ← 0, MPIE ← 1，特权级设置为 M 模式（这里简单写成固定 11）
    io.csr_bundle.mstatus_write_data :=
      Cat(
        io.csr_bundle.mstatus(31,13),  // 高位保持
        3.U(2.W),                      // MPP = 11 (Machine mode)
        io.csr_bundle.mstatus(10,4),   // 其它位保持
        0.U(1.W),                      // MIE = 0，关中断
        io.csr_bundle.mstatus(2,0)     // 低 3 位保持
      )

    // 保存返回地址：注意是“下一条要执行的 PC”
    val instruction_address = io.instruction_address+4.U
    io.csr_bundle.mepc_write_data := instruction_address

    // 根据 interrupt_flag 判断是 Timer 中断还是其他中断
    io.csr_bundle.mcause_write_data := Mux(
      io.interrupt_flag(0),     // 比如约定 bit0=Timer
      "h80000007".U,            // Machine timer interrupt
      "h8000000B".U             // Machine external/software interrupt
    )

    io.csr_bundle.direct_write_enable := true.B
    io.interrupt_assert := true.B
    // 跳转到 mtvec
    io.interrupt_handler_address := io.csr_bundle.mtvec
  }

    // 3. 遇到 mret 指令：从中断返回
    .elsewhen (io.instruction === InstructionsRet.mret) {
      // 恢复 MIE：MIE ← 1, 继续保持 MPP=11（本实验只跑 M-mode）
      io.csr_bundle.mstatus_write_data :=
        Cat(
          io.csr_bundle.mstatus(31,13),
          3.U(2.W),
          io.csr_bundle.mstatus(10,4),
          1.U(1.W),                  // MIE = 1，开中断
          io.csr_bundle.mstatus(2,0)
        )

      // MEPC / MCAUSE 保持原样（这里不需要修改）
      io.csr_bundle.mepc_write_data   := io.csr_bundle.mepc
      io.csr_bundle.mcause_write_data := io.csr_bundle.mcause

      io.csr_bundle.direct_write_enable := true.B
      io.interrupt_assert := true.B
      // mret 返回地址就是 mepc
      io.interrupt_handler_address := io.csr_bundle.mepc
    }

    // 4. 普通情况：既没有中断发生，也不是 mret
    .otherwise {
      io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
      io.csr_bundle.mepc_write_data    := io.csr_bundle.mepc
      io.csr_bundle.mcause_write_data  := io.csr_bundle.mcause

      io.csr_bundle.direct_write_enable := false.B
      io.interrupt_assert := false.B
      io.interrupt_handler_address := io.csr_bundle.mtvec // don't care
    }


}
