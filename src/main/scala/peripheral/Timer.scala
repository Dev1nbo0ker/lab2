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

package peripheral

import chisel3._
import chisel3.util._
import riscv.Parameters

class Timer extends Module {
  val io = IO(new Bundle {
    val bundle = new RAMBundle
    val signal_interrupt = Output(Bool())

    val debug_limit = Output(UInt(Parameters.DataWidth))
    val debug_enabled = Output(Bool())
  })

  val count = RegInit(0.U(32.W))
  val limit = RegInit(100000000.U(32.W))
  io.debug_limit := limit
  val enabled = RegInit(true.B)
  io.debug_enabled := enabled

  //lab2(CLINTCSR)
  //finish the read-write for count,limit,enabled. And produce appropriate signal_interrupt
  // 只用低 4 位做内部寄存器寻址
  val localAddr = io.bundle.address(3, 0)

  // 三个寄存器的偏移
  val ADDR_COUNT  = "h0".U(4.W)  // 当前计数
  val ADDR_LIMIT  = "h4".U(4.W)  // limit
  val ADDR_ENABLE = "h8".U(4.W)  // enable

  // -------- 读操作 --------
  // 没有 read_enable，直接根据地址组合输出
  io.bundle.read_data := MuxLookup(localAddr, 0.U, Array(
    ADDR_COUNT  -> count,
    ADDR_LIMIT  -> limit,
    ADDR_ENABLE -> enabled.asUInt
  ))

  // -------- 写操作 + 计数逻辑 --------
  // 用一个 nextCount 线网来统一决定本拍写回的计数值
  val nextCount = WireDefault(count)

  // 正常启用时自增
  when (enabled) {
    nextCount := count + 1.U
  }

  // 有写操作时，根据地址覆写寄存器
  when (io.bundle.write_enable) {
    switch(localAddr) {
      is(ADDR_COUNT)  { nextCount := io.bundle.write_data }
      is(ADDR_LIMIT)  { limit     := io.bundle.write_data }
      is(ADDR_ENABLE) { enabled   := io.bundle.write_data(0) }
    }
  }

  // 最后统一更新 count
  count := nextCount

  // -------- 中断信号 --------
  // 计数到达 limit，且 enable=1 时产生中断
  val hitLimit      = enabled && (count === limit)
  val interruptReg  = RegInit(false.B)

  // 让中断信号至少保持 1 个周期
  interruptReg := hitLimit
  io.signal_interrupt := interruptReg
  // lab2(CLINTCSR) end
}
