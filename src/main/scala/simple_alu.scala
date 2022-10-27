import chisel3._
import chisel3.util._

// create a simple alu unit
class Alu extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Input(UInt(8.W))
        val op = Input(Bool())
        val out = Output(UInt(8.W))
    })
    val x = Wire(UInt())
    val y = Wire(UInt())
    val reg_out = RegInit(0.U(8.W))
    x := io.a + io.b
    y := io.a - io.b
    reg_out := Mux(io.op, x, y)
    io.out := reg_out
}

// test of ALU module
//class alu_test
//     test(new Alu) { c =>
//         import scala.util.Random
//         for (i <- 0 until 20) {
//             val a = Random.nextInt(256)
//             val b = Random.nextInt(256)
//             c.io.op.poke(true.B)
//             c.io.a.poke(a.U(8.W))
//             c.io.b.poke(b.U(8.W))
//             c.clock.step(1)
//             if (a + b > 255) {
//                 c.io.out.expect(((a + b) % 256).U(8.W))
//             } else {
//                 c.io.out.expect((a + b).U(8.W))
//             }
        
//             c.io.op.poke(false.B)
//             c.io.a.poke(a.U(8.W))
//             c.io.b.poke(b.U(8.W))
//             c.clock.step(1)
//             if (a - b < 0) {
//                 c.io.out.expect((256 + (a - b)).U(8.W))
//             } else {
//                 c.io.out.expect((a - b).U(8.W))
//             }
//         }
// }
// println("SUCESS")