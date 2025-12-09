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

  // ================== lab2(CLINTCSR) ==================
  //finish the read-write for count,limit,enabled. And produce appropriate signal_interrupt
  // Timer MMIO 读写与中断逻辑

  // MMIO 读逻辑 (组合逻辑)
  // 地址 0x4 -> limit, 0x8 -> enabled, 0x0 -> count (假设 count 在 offset 0)
  io.bundle.read_data := MuxLookup(io.bundle.address(3, 0), 0.U, Seq(
    0.U -> count,
    4.U -> limit,
    8.U -> enabled.asUInt
  ))

  // 计数与写逻辑
  // 默认情况下，如果 enabled 为真，count 自增
  val count_next = Mux(enabled, count + 1.U, count)

  when(io.bundle.write_enable) {
    switch(io.bundle.address(3, 0)) {
      is(0.U) {
        count := io.bundle.write_data
      }
      is(4.U) {
        limit := io.bundle.write_data
        count := count_next // 写 limit 时，count 仍需根据 enable 状态更新
      }
      is(8.U) {
        enabled := io.bundle.write_data =/= 0.U
        count := count_next
      }
    }
  }.otherwise {
    count := count_next
  }

  // 中断逻辑：计数器达到 limit 且使能时触发
  io.signal_interrupt := enabled && (count >= limit)
  // ================== lab2(CLINTCSR) End ==================
}
