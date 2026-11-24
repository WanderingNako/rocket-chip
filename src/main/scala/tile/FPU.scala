// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import chisel3.{DontCare, WireInit, withClock, withReset}
import chisel3.experimental.SourceInfo
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

case class FPUParams(
  minFLen: Int = 32,
  fLen: Int = 64,
  divSqrt: Boolean = true,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4,
  fpmuLatency: Int = 2,
  ifpuLatency: Int = 2
)

object FPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}

trait HasFPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
  val vec = Bool()
}

class FPUCtrlSigs extends Bundle with HasFPUCtrlSigs

class FPUDecoder(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val inst = Input(Bits(32.W))
    val sigs = Output(new FPUCtrlSigs())
  })

  private val X2 = BitPat.dontCare(2)

  val default =       List(X,X,X,X,X,X,X,X2,X2,X,X,X,X,X,X,X,N)
  val h: Array[(BitPat, List[BitPat])] =
    Array(FLH      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N),
          FSH      -> List(Y,N,N,Y,N,Y,X, I, H,N,Y,N,N,N,N,N,N),
          FMV_H_X  -> List(N,Y,N,N,N,X,X, H, I,Y,N,N,N,N,N,N,N),
          FCVT_H_W -> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_WU-> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_L -> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_LU-> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FMV_X_H  -> List(N,N,Y,N,N,N,X, I, H,N,Y,N,N,N,N,N,N),
          FCLASS_H -> List(N,N,Y,N,N,N,X, H, H,N,Y,N,N,N,N,N,N),
          FCVT_W_H -> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_WU_H-> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_L_H -> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_LU_H-> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_S_H -> List(N,Y,Y,N,N,N,X, H, S,N,N,Y,N,N,N,Y,N),
          FCVT_H_S -> List(N,Y,Y,N,N,N,X, S, H,N,N,Y,N,N,N,Y,N),
          FEQ_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FLT_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FLE_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FSGNJ_H  -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FSGNJN_H -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FSGNJX_H -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FMIN_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,Y,N),
          FMAX_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,Y,N),
          FADD_H   -> List(N,Y,Y,Y,N,N,Y, H, H,N,N,N,Y,N,N,Y,N),
          FSUB_H   -> List(N,Y,Y,Y,N,N,Y, H, H,N,N,N,Y,N,N,Y,N),
          FMUL_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FMADD_H  -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FMSUB_H  -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FNMADD_H -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FNMSUB_H -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FDIV_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,N,N,Y,N,Y,N),
          FSQRT_H  -> List(N,Y,Y,N,N,N,X, H, H,N,N,N,N,N,Y,Y,N))
  val f: Array[(BitPat, List[BitPat])] =
    Array(FLW      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N),
          FSW      -> List(Y,N,N,Y,N,Y,X, I, S,N,Y,N,N,N,N,N,N),
          FMV_W_X  -> List(N,Y,N,N,N,X,X, S, I,Y,N,N,N,N,N,N,N),
          FCVT_S_W -> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N),
          FCVT_S_WU-> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N),
          FCVT_S_L -> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N),
          FCVT_S_LU-> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N),
          FMV_X_W  -> List(N,N,Y,N,N,N,X, I, S,N,Y,N,N,N,N,N,N),
          FCLASS_S -> List(N,N,Y,N,N,N,X, S, S,N,Y,N,N,N,N,N,N),
          FCVT_W_S -> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N),
          FCVT_WU_S-> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N),
          FCVT_L_S -> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N),
          FCVT_LU_S-> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N),
          FEQ_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N),
          FLT_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N),
          FLE_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N),
          FSGNJ_S  -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N),
          FSGNJN_S -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N),
          FSGNJX_S -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N),
          FMIN_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,Y,N),
          FMAX_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,Y,N),
          FADD_S   -> List(N,Y,Y,Y,N,N,Y, S, S,N,N,N,Y,N,N,Y,N),
          FSUB_S   -> List(N,Y,Y,Y,N,N,Y, S, S,N,N,N,Y,N,N,Y,N),
          FMUL_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,Y,N,N,Y,N),
          FMADD_S  -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N),
          FMSUB_S  -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N),
          FNMADD_S -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N),
          FNMSUB_S -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N),
          FDIV_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,N,Y,N,Y,N),
          FSQRT_S  -> List(N,Y,Y,N,N,N,X, S, S,N,N,N,N,N,Y,Y,N))
  val d: Array[(BitPat, List[BitPat])] =
    Array(FLD      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N),
          FSD      -> List(Y,N,N,Y,N,Y,X, I, D,N,Y,N,N,N,N,N,N),
          FMV_D_X  -> List(N,Y,N,N,N,X,X, D, I,Y,N,N,N,N,N,N,N),
          FCVT_D_W -> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_WU-> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_L -> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_LU-> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FMV_X_D  -> List(N,N,Y,N,N,N,X, I, D,N,Y,N,N,N,N,N,N),
          FCLASS_D -> List(N,N,Y,N,N,N,X, D, D,N,Y,N,N,N,N,N,N),
          FCVT_W_D -> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_WU_D-> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_L_D -> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_LU_D-> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_S_D -> List(N,Y,Y,N,N,N,X, D, S,N,N,Y,N,N,N,Y,N),
          FCVT_D_S -> List(N,Y,Y,N,N,N,X, S, D,N,N,Y,N,N,N,Y,N),
          FEQ_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FLT_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FLE_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FSGNJ_D  -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FSGNJN_D -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FSGNJX_D -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FMIN_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,Y,N),
          FMAX_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,Y,N),
          FADD_D   -> List(N,Y,Y,Y,N,N,Y, D, D,N,N,N,Y,N,N,Y,N),
          FSUB_D   -> List(N,Y,Y,Y,N,N,Y, D, D,N,N,N,Y,N,N,Y,N),
          FMUL_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FMADD_D  -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FMSUB_D  -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FNMADD_D -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FNMSUB_D -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FDIV_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,N,N,Y,N,Y,N),
          FSQRT_D  -> List(N,Y,Y,N,N,N,X, D, D,N,N,N,N,N,Y,Y,N))
  val fcvt_hd: Array[(BitPat, List[BitPat])] =
    Array(FCVT_H_D -> List(N,Y,Y,N,N,N,X, D, H,N,N,Y,N,N,N,Y,N),
          FCVT_D_H -> List(N,Y,Y,N,N,N,X, H, D,N,N,Y,N,N,N,Y,N))
  val vfmv_f_s: Array[(BitPat, List[BitPat])] =
    Array(VFMV_F_S -> List(N,Y,N,N,N,N,X,X2,X2,N,N,N,N,N,N,N,Y))

  val insns = ((minFLen, fLen) match {
    case (32, 32) => f
    case (16, 32) => h ++ f
    case (32, 64) => f ++ d
    case (16, 64) => h ++ f ++ d ++ fcvt_hd
    case other => throw new Exception(s"minFLen = ${minFLen} & fLen = ${fLen} is an unsupported configuration")
  }) ++ (if (usingVector) vfmv_f_s else Array[(BitPat, List[BitPat])]())
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.typeTagIn, s.typeTagOut, s.fromint, s.toint,
                 s.fastpipe, s.fma, s.div, s.sqrt, s.wflags, s.vec)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val hartid = Input(UInt(hartIdLen.W))
  val time = Input(UInt(xLen.W))

  val inst = Input(Bits(32.W))
  val fromint_data = Input(Bits(xLen.W))

  val fcsr_rm = Input(Bits(FPConstants.RM_SZ.W))
  val fcsr_flags = Valid(Bits(FPConstants.FLAGS_SZ.W))

  val v_sew = Input(UInt(3.W))

  val store_data = Output(Bits(fLen.W))
  val toint_data = Output(Bits(xLen.W))

  val ll_resp_val = Input(Bool())
  val ll_resp_type = Input(Bits(3.W))
  val ll_resp_tag = Input(UInt(5.W))
  val ll_resp_data = Input(Bits(fLen.W))

  val valid = Input(Bool())
  val fcsr_rdy = Output(Bool())
  val nack_mem = Output(Bool())
  val illegal_rm = Output(Bool())
  val killx = Input(Bool())
  val killm = Input(Bool())
  val dec = Output(new FPUCtrlSigs())
  val sboard_set = Output(Bool())
  val sboard_clr = Output(Bool())
  val sboard_clra = Output(UInt(5.W))

  val keep_clock_enabled = Input(Bool())
}

class FPUIO(implicit p: Parameters) extends FPUCoreIO ()(p) {
  val cp_req = Flipped(Decoupled(new FPInput())) //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits((fLen max xLen).W)
  val exc = Bits(FPConstants.FLAGS_SZ.W)
}

class IntToFPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(FPConstants.RM_SZ.W)
  val typ = Bits(2.W)
  val in1 = Bits(xLen.W)
}

class FPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(FPConstants.RM_SZ.W)
  val fmaCmd = Bits(2.W)
  val typ = Bits(2.W)
  val fmt = Bits(2.W)
  val in1 = Bits(fLen.W)
  val in2 = Bits(fLen.W)
  val in3 = Bits(fLen.W)

}

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def qNaN = ((BigInt(1) << (ieeeWidth - 1)) - (BigInt(1) << (sig - 2))).U(ieeeWidth.W)
  def isNaN(x: UInt) = x(ieeeWidth - 2, sig - 1).andR && x(sig - 2, 0).orR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(ieeeWidth - 1)
    val exponent = x(ieeeWidth - 2, sig - 1)
    val fraction = x(sig - 2, 0)
    val isZero = !exponent.orR && !fraction.orR
    val isSubnormal = !exponent.orR && fraction.orR
    val isNormal = exponent.orR && !exponent.andR
    val isInf = exponent.andR && !fraction.orR
    val isQNaN = isNaN(x) && fraction(sig - 2)

    Cat(isQNaN, isSNaN(x), isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  private def ieeeBundle = {
    val expWidth = exp
    class IEEEBundle extends Bundle {
      val sign = Bool()
      val exp = UInt(expWidth.W)
      val sig = UInt((ieeeWidth-expWidth-1).W)
    }
    new IEEEBundle
  }

  def unpackIEEE(x: UInt) = x.asTypeOf(ieeeBundle)
}

object FType {
  val H = new FType(5, 11)
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(H, S, D)
}

trait HasFPUParameters {
  require(fLen == 0 || FType.all.exists(_.ieeeWidth == fLen))
  val minFLen: Int
  val fLen: Int
  def xLen: Int
  val minXLen = 32
  val nIntTypes = log2Ceil(xLen/minXLen) + 1
  def floatTypes = FType.all.filter(t => minFLen <= t.ieeeWidth && t.ieeeWidth <= fLen)
  def minType = floatTypes.head
  def maxType = floatTypes.last
  def maxExpWidth = maxType.exp
  def maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)
  def typeTagWbOffset = (FType.all.indexOf(minType) + 1).U
  def typeTagGroup(t: FType) = (if (floatTypes.contains(t)) typeTag(t) else typeTag(maxType)).U
  // typeTag
  def H = typeTagGroup(FType.H)
  def S = typeTagGroup(FType.S)
  def D = typeTagGroup(FType.D)
  def I = typeTag(maxType).U

  // Operands are right-aligned IEEE values at their original precision.
  def unbox(x: UInt, tag: UInt): UInt = {
    val bits = x.pad(fLen)
    val operands = floatTypes.map { t =>
      val value = bits(t.ieeeWidth - 1, 0)
      if (t == maxType) value
      else Mux(bits(fLen - 1, t.ieeeWidth).andR, value, t.qNaN)
    }
    operands(tag)
  }

  def box(x: UInt, t: FType): UInt = {
    val value = x.pad(t.ieeeWidth)(t.ieeeWidth - 1, 0)
    if (t == maxType) value
    else Cat(Fill(fLen - t.ieeeWidth, 1.U), value)
  }

  def box(x: UInt, tag: UInt): UInt = floatTypes.map(t => box(x, t))(tag)
}

abstract class FPUModule(implicit val p: Parameters) extends Module with HasCoreParameters with HasFPUParameters

class FPToInt(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  class Output extends Bundle {
    val in = new FPInput
    val lt = Bool()
    val store = Bits(fLen.W)
    val toint = Bits(xLen.W)
    val exc = Bits(FPConstants.FLAGS_SZ.W)
  }
  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new Output)
  })

  val in = RegEnable(io.in.bits, io.in.valid)
  val valid = RegNext(io.in.valid)

  val tag = in.typeTagOut
  val raw = floatTypes.map { t =>
    in.in1((t.ieeeWidth min xLen) - 1, 0).sextTo(xLen)
  }(tag)
  val classifications = floatTypes.map(t => t.classify(in.in1))
  val comparisons = floatTypes.map { t =>
    val cmp = Module(new hardfloat.CompareFN(t.exp, t.sig))
    cmp.io.a := in.in1(t.ieeeWidth - 1, 0)
    cmp.io.b := in.in2(t.ieeeWidth - 1, 0)
    cmp.io.signaling := !in.rm(1)
    cmp
  }
  val comparison = comparisons.map(c => (~in.rm & Cat(c.io.lt, c.io.eq)).orR)(in.typeTagIn)
  val comparisonFlags = comparisons.map(_.io.exceptionFlags)(in.typeTagIn)

  val conversions = floatTypes.map { t =>
    (0 until nIntTypes).map { i =>
      val width = minXLen << i
      val conv = Module(new hardfloat.FNToIN(t.exp, t.sig, width))
      conv.io.in := in.in1(t.ieeeWidth - 1, 0)
      conv.io.roundingMode := in.rm
      conv.io.signedOut := !in.typ(0)
      val flags = conv.io.intExceptionFlags
      (conv.io.out.sextTo(xLen), Cat(flags(2, 1).orR, 0.U(3.W), flags(0)))
    }
  }
  val intTag = in.typ.extract(log2Ceil(nIntTypes), 1)
  val converted = conversions.map(_.map(_._1)(intTag))(in.typeTagIn)
  val conversionFlags = conversions.map(_.map(_._2)(intTag))(in.typeTagIn)

  io.out.bits.store := floatTypes.map(t => Fill(fLen / t.ieeeWidth, in.in1(t.ieeeWidth - 1, 0)))(tag)
  io.out.bits.toint := Mux(in.rm(0), classifications(tag), raw)
  io.out.bits.exc := 0.U
  when (in.wflags) {
    io.out.bits.toint := Mux(in.ren2, comparison, converted)
    io.out.bits.exc := Mux(in.ren2, comparisonFlags, conversionFlags)
  }

  io.out.valid := valid
  io.out.bits.lt := floatTypes.zip(comparisons).map { case (t, cmp) =>
    cmp.io.lt || (in.in1(t.ieeeWidth - 1) && !in.in2(t.ieeeWidth - 1))
  }(in.typeTagIn)
  io.out.bits.in := in
}

class IntToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new IntToFPInput))
    val out = Valid(new FPResult)
  })

  val in = Pipe(io.in)
  val tag = in.bits.typeTagIn

  val mux = Wire(new FPResult)
  mux.exc := 0.U
  mux.data := in.bits.in1

  val intValue = {
    val res = WireDefault(in.bits.in1.asSInt)
    for (i <- 0 until nIntTypes-1) {
      val smallInt = in.bits.in1((minXLen << i) - 1, 0)
      when (in.bits.typ.extract(log2Ceil(nIntTypes), 1) === i.U) {
        res := Mux(in.bits.typ(0), smallInt.zext, smallInt.asSInt)
      }
    }
    res.asUInt
  }

  when (in.bits.wflags) { // fcvt
    // could be improved for RVD/RVQ with a single variable-position rounding
    // unit, rather than N fixed-position ones
    val i2fResults = for (t <- floatTypes) yield {
      val i2f = Module(new hardfloat.INToFN(xLen, t.exp, t.sig))
      i2f.io.signedIn := ~in.bits.typ(0)
      i2f.io.in := intValue
      i2f.io.roundingMode := in.bits.rm
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (i2f.io.out, i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    mux.data := data(tag)
    mux.exc := exc(tag)
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new FPResult)
    val lt = Input(Bool()) // from FPToInt
  })

  val in = Pipe(io.in)

  val results = floatTypes.map { t =>
    val a = in.bits.in1(t.ieeeWidth - 1, 0)
    val b = in.bits.in2(t.ieeeWidth - 1, 0)
    val signA = a(t.ieeeWidth - 1)
    val signB = b(t.ieeeWidth - 1)
    val sign = Mux(in.bits.rm(1), signA ^ signB, signB ^ in.bits.rm(0))
    val nanA = t.isNaN(a)
    val nanB = t.isNaN(b)
    val selectA = nanB || (!nanA && (in.bits.rm(0) =/= io.lt))
    val minMax = Mux(nanA && nanB, t.qNaN, Mux(selectA, a, b))
    val invalid = t.isSNaN(a) || t.isSNaN(b)
    (Mux(in.bits.wflags, minMax, Cat(sign, a(t.ieeeWidth - 2, 0))),
      Cat(in.bits.wflags && invalid, 0.U(4.W)))
  }

  val mux = Wire(new FPResult)
  mux.data := results.map(_._1)(in.bits.typeTagIn)
  mux.exc := results.map(_._2)(in.bits.typeTagIn)
  for (from <- floatTypes; to <- floatTypes if from != to) {
    val convert = Module(new hardfloat.FNToFN(from.exp, from.sig, to.exp, to.sig))
    convert.io.in := in.bits.in1(from.ieeeWidth - 1, 0)
    convert.io.roundingMode := in.bits.rm
    convert.io.detectTininess := hardfloat.consts.tininess_afterRounding
    when (in.bits.wflags && !in.bits.ren2 &&
        in.bits.typeTagIn === typeTag(from).U && in.bits.typeTagOut === typeTag(to).U) {
      val nan = from.isNaN(convert.io.in)
      mux.data := Mux(nan, to.qNaN, convert.io.out)
      mux.exc := Mux(nan, Cat(from.isSNaN(convert.io.in), 0.U(4.W)), convert.io.exceptionFlags)
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class MulAddFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
    override def desiredName = s"MulAddFNPipe_l${latency}_e${expWidth}_s${sigWidth}"
    require(latency<=2)

    val io = IO(new Bundle {
        val validin = Input(Bool())
        val op = Input(Bits(2.W))
        val a = Input(Bits((expWidth + sigWidth).W))
        val b = Input(Bits((expWidth + sigWidth).W))
        val c = Input(Bits((expWidth + sigWidth).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output(Bits((expWidth + sigWidth).W))
        val exceptionFlags = Output(Bits(5.W))
        val validout = Output(Bool())
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := hardfloat.recFNFromFN(expWidth, sigWidth, io.a)
    mulAddRecFNToRaw_preMul.io.b  := hardfloat.recFNFromFN(expWidth, sigWidth, io.b)
    mulAddRecFNToRaw_preMul.io.c  := hardfloat.recFNFromFN(expWidth, sigWidth, io.c)

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
             mulAddRecFNToRaw_preMul.io.mulAddB) +&
            mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(3.W))
    val detectTininess_stage0 = Wire(UInt(1.W))

    val postmul_regs = if(latency>0) 1 else 0
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = if(latency==2) 1 else 0
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := false.B

    io.out            := hardfloat.fNFromRecFN(expWidth, sigWidth, roundRawFNToRecFN.io.out)
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

class FPUFMAPipe(val latency: Int, val t: FType)
                (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  override def desiredName = s"FPUFMAPipe_l${latency}_f${t.ieeeWidth}"
  require(latency>0)

  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new FPResult)
  })

  val valid = RegNext(io.in.valid)
  val in = Reg(new FPInput)
  when (io.in.valid) {
    val one = ((BigInt(1) << (t.exp - 1)) - 1).U << (t.sig - 1)
    val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (1.U << (t.ieeeWidth - 1))
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in := io.in.bits
    when (cmd_addsub) { in.in2 := one }
    when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
  }

  val fma = Module(new MulAddFNPipe((latency-1) min 2, t.exp, t.sig))
  fma.io.validin := valid
  fma.io.op := in.fmaCmd
  fma.io.roundingMode := in.rm
  fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := fma.io.out
  res.exc := fma.io.exceptionFlags

  io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}

class FPU(cfg: FPUParams)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new FPUIO)

  val (useClockGating, useDebugROB) = coreParams match {
    case r: RocketCoreParams =>
      val sz = if (r.debugROB.isDefined) r.debugROB.get.size else 1
      (r.clockGate, sz < 1)
    case _ => (false, false)
  }
  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cp_req.valid
  val gated_clock =
    if (!useClockGating) clock
    else ClockGate(clock, clock_en, "fpu_clock_gate")

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst
  val id_ctrl = WireInit(fp_decoder.io.sigs)
  coreParams match { case r: RocketCoreParams => r.vector.map(v => {
    val v_decode = v.decoder(p) // Only need to get ren1
    v_decode.io.inst := io.inst
    v_decode.io.vconfig := DontCare // core deals with this
    when (v_decode.io.legal && v_decode.io.read_frs1) {
      id_ctrl.ren1 := true.B
      id_ctrl.swap12 := false.B
      id_ctrl.toint := true.B
      id_ctrl.typeTagIn := I
      id_ctrl.typeTagOut := Mux(io.v_sew === 3.U, D, S)
    }
    when (v_decode.io.write_frd) { id_ctrl.wen := true.B }
  })}

  val ex_reg_valid = RegNext(io.valid, false.B)
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_reg_ctrl = RegEnable(id_ctrl, io.valid)
  val ex_ra = List.fill(3)(Reg(UInt()))

  // load/vector response
  val load_wb = RegNext(io.ll_resp_val)
  val load_typeTag = io.ll_resp_type(1,0) - typeTagWbOffset
  val load_wb_data = RegEnable(box(io.ll_resp_data, load_typeTag), io.ll_resp_val)
  val load_wb_tag = RegEnable(io.ll_resp_tag, io.ll_resp_val)

  class FPUImpl { // entering gated-clock domain

  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_cp_valid = io.cp_req.fire
  val mem_cp_valid = RegNext(ex_cp_valid, false.B)
  val wb_cp_valid = RegNext(mem_cp_valid, false.B)
  val mem_reg_valid = RegInit(false.B)
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  // Kill X-stage instruction if M-stage is killed.  This prevents it from
  // speculatively being sent to the div-sqrt unit, which can cause priority
  // inversion for two back-to-back divides, the first of which is killed.
  val killx = io.killx || mem_reg_valid && killm
  mem_reg_valid := ex_reg_valid && !killx || ex_cp_valid
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val wb_reg_valid = RegNext(mem_reg_valid && (!killm || mem_cp_valid), false.B)

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl :<>= io.cp_req.bits.viewAsSupertype(new FPUCtrlSigs)
  io.cp_resp.valid := false.B
  io.cp_resp.bits.data := 0.U
  io.cp_resp.bits.exc := DontCare

  val ex_ctrl = Mux(ex_cp_valid, cp_ctrl, ex_reg_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // CoreMonitorBundle to monitor fp register file writes
  val frfWriteBundle = Seq.fill(2)(WireInit(new CoreMonitorBundle(xLen, fLen), DontCare))
  frfWriteBundle.foreach { i =>
    i.clock := clock
    i.reset := reset
    i.hartid := io.hartid
    i.timer := io.time(31,0)
    i.valid := false.B
    i.wrenx := false.B
    i.wrenf := false.B
    i.excpt := false.B
  }

  // regfile
  val regfile = Mem(32, Bits(fLen.W))
  when (load_wb) {
    val wdata = load_wb_data
    regfile(load_wb_tag) := wdata
    if (enableCommitLog)
      printf("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + 32.U, wdata)
    if (useDebugROB)
      DebugROB.pushWb(clock, reset, io.hartid, load_wb, load_wb_tag + 32.U, wdata)
    frfWriteBundle(0).wrdst := load_wb_tag
    frfWriteBundle(0).wrenf := true.B
    frfWriteBundle(0).wrdata := wdata
  }

  val ex_rs = ex_ra.map(a => regfile(a))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra(0) := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra(1) := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra(0) := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra(2) := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra(1) := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra(2) := io.inst(31,27) }
  }
  val ex_rm = Mux(ex_reg_inst(14,12) === 7.U, io.fcsr_rm, ex_reg_inst(14,12))

  def fuInput: FPInput = {
    val req = Wire(new FPInput)
    val tag = ex_ctrl.typeTagIn
    req.viewAsSupertype(new Bundle with HasFPUCtrlSigs) :#= ex_ctrl.viewAsSupertype(new Bundle with HasFPUCtrlSigs)
    req.rm := ex_rm
    req.in1 := unbox(ex_rs(0), tag)
    req.in2 := unbox(ex_rs(1), tag)
    req.in3 := unbox(ex_rs(2), tag)
    req.typ := ex_reg_inst(21,20)
    req.fmt := ex_reg_inst(26,25)
    req.fmaCmd := ex_reg_inst(3,2) | (!ex_ctrl.ren3 && ex_reg_inst(27))
    when (ex_cp_valid) {
      req := io.cp_req.bits
      when (io.cp_req.bits.swap12) {
        req.in1 := io.cp_req.bits.in2
        req.in2 := io.cp_req.bits.in1
      }
      when (io.cp_req.bits.swap23) {
        req.in2 := io.cp_req.bits.in3
        req.in3 := io.cp_req.bits.in2
      }
    }
    req
  }

  val sfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.S))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === S
  sfma.io.in.bits := fuInput

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || (ex_ctrl.fastpipe && ex_ctrl.wflags))
  fpiu.io.in.bits := fuInput
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := true.B
  }

  val ifpu = Module(new IntToFP(cfg.ifpuLatency))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := fpiu.io.in.bits
  ifpu.io.in.bits.in1 := Mux(ex_cp_valid, io.cp_req.bits.in1, io.fromint_data)

  val fpmu = Module(new FPToFP(cfg.fpmuLatency))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = WireDefault(false.B)
  val divSqrt_inFlight = WireDefault(false.B)
  val divSqrt_waddr = Reg(UInt(5.W))
  val divSqrt_cp = Reg(Bool())
  val divSqrt_typeTag = Wire(UInt(log2Up(floatTypes.size).W))
  val divSqrt_wdata = Wire(UInt(fLen.W))
  val divSqrt_flags = Wire(UInt(FPConstants.FLAGS_SZ.W))
  divSqrt_typeTag := DontCare
  divSqrt_wdata := DontCare
  divSqrt_flags := DontCare
  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === S, sfma.io.out.bits)) ++
    (fLen > 32).option({
          val dfma = Module(new FPUFMAPipe(cfg.dfmaLatency, FType.D))
          dfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === D
          dfma.io.in.bits := fuInput
          Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === D, dfma.io.out.bits)
        }) ++
    (minFLen == 16).option({
          val hfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.H))
          hfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === H
          hfma.io.in.bits := fuInput
          Pipe(hfma, hfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === H, hfma.io.out.bits)
        })
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), (1 << p.lat-offset).U, 0.U)).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), p._2.U, 0.U)).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  class WBInfo extends Bundle {
    val rd = UInt(5.W)
    val typeTag = UInt(log2Up(floatTypes.size).W)
    val cp = Bool()
    val pipeid = UInt(log2Ceil(pipes.size).W)
  }

  val wen = RegInit(0.U((maxLatency-1).W))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  ccover(mem_reg_valid && write_port_busy, "WB_STRUCTURAL", "structural hazard on writeback")

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        wbInfo(i).cp := mem_cp_valid
        wbInfo(i).typeTag := mem_ctrl.typeTagOut
        wbInfo(i).pipeid := pipeid(mem_ctrl)
        wbInfo(i).rd := mem_reg_inst(11,7)
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, wbInfo(0).rd)
  val wb_cp = Mux(divSqrt_wen, divSqrt_cp, wbInfo(0).cp)
  val wtypeTag = Mux(divSqrt_wen, divSqrt_typeTag, wbInfo(0).typeTag)
  val wdata = box(Mux(divSqrt_wen, divSqrt_wdata, (pipes.map(_.res.data): Seq[UInt])(wbInfo(0).pipeid)), wtypeTag)
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wbInfo(0).pipeid)
  when ((!wbInfo(0).cp && wen(0)) || divSqrt_wen) {
    regfile(waddr) := wdata
    if (enableCommitLog) {
      printf("f%d p%d 0x%x\n", waddr, waddr + 32.U, wdata)
    }
    frfWriteBundle(1).wrdst := waddr
    frfWriteBundle(1).wrenf := true.B
    frfWriteBundle(1).wrdata := wdata
  }
  if (useDebugROB) {
    DebugROB.pushWb(clock, reset, io.hartid, (!wbInfo(0).cp && wen(0)) || divSqrt_wen, waddr + 32.U, wdata)
  }

  when (wb_cp && (wen(0) || divSqrt_wen)) {
    io.cp_resp.bits.data := wdata
    io.cp_resp.valid := true.B
  }

  assert(!io.cp_req.valid || pipes.forall(_.lat == pipes.head.lat).B,
    s"FPU only supports coprocessor if FMA pipes have uniform latency ${pipes.map(_.lat)}")
  // Avoid structural hazards and nacking of external requests
  // toint responds in the MEM stage, so an incoming toint can induce a structural hazard against inflight FMAs
  io.cp_req.ready := !ex_reg_valid && !(cp_ctrl.toint && wen =/= 0.U) && !divSqrt_inFlight

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.fcsr_flags.valid := wb_toint_valid || divSqrt_wen || wen(0)
  io.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, 0.U) |
    Mux(divSqrt_wen, divSqrt_flags, 0.U) |
    Mux(wen(0), wexc, 0.U)

  val divSqrt_write_port_busy = (mem_ctrl.div || mem_ctrl.sqrt) && wen.orR
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_inFlight)
  io.nack_mem := (write_port_busy || divSqrt_write_port_busy || divSqrt_inFlight) && !mem_cp_valid
  io.dec <> id_ctrl
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(false.B)(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && RegNext(useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt || mem_ctrl.vec)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wbInfo(0).pipeid === x._2.U)))
  io.sboard_clra := waddr
  ccover(io.sboard_clr && load_wb, "DUAL_WRITEBACK", "load and FMA writeback on same cycle")
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := io.inst(14,12).isOneOf(5.U, 6.U) || io.inst(14,12) === 7.U && io.fcsr_rm >= 5.U

  if (cfg.divSqrt) {
    val divSqrt_inValid = mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_inFlight
    val divSqrt_killed = RegNext(divSqrt_inValid && killm, true.B)
    when (divSqrt_inValid) {
      divSqrt_waddr := mem_reg_inst(11,7)
      divSqrt_cp := mem_cp_valid
    }

    ccover(divSqrt_inFlight && divSqrt_killed, "DIV_KILLED", "divide killed after issued to divider")
    ccover(divSqrt_inFlight && mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt), "DIV_BUSY", "divider structural hazard")
    ccover(mem_reg_valid && divSqrt_write_port_busy, "DIV_WB_STRUCTURAL", "structural hazard on division writeback")

    for (t <- floatTypes) {
      val tag = mem_ctrl.typeTagOut
      val divSqrt = withReset(divSqrt_killed) { Module(new hardfloat.DivSqrtFN_small(t.exp, t.sig, 0)) }
      divSqrt.io.inValid := divSqrt_inValid && tag === typeTag(t).U
      divSqrt.io.sqrtOp := mem_ctrl.sqrt
      divSqrt.io.a := fpiu.io.out.bits.in.in1(t.ieeeWidth - 1, 0)
      divSqrt.io.b := fpiu.io.out.bits.in.in2(t.ieeeWidth - 1, 0)
      divSqrt.io.roundingMode := fpiu.io.out.bits.in.rm
      divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding

      when (!divSqrt.io.inReady) { divSqrt_inFlight := true.B } // only 1 in flight

      when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
        divSqrt_wen := !divSqrt_killed
        divSqrt_wdata := divSqrt.io.out
        divSqrt_flags := divSqrt.io.exceptionFlags
        divSqrt_typeTag := typeTag(t).U
      }
    }

    when (divSqrt_killed) { divSqrt_inFlight := false.B }
  } else {
    when (id_ctrl.div || id_ctrl.sqrt) { io.illegal_rm := true.B }
  }

  // gate the clock
  clock_en_reg := !useClockGating.B ||
    io.keep_clock_enabled || // chicken bit
    io.valid || // ID stage
    req_valid || // EX stage
    mem_reg_valid || mem_cp_valid || // MEM stage
    wb_reg_valid || wb_cp_valid || // WB stage
    wen.orR || divSqrt_inFlight || // post-WB stage
    io.ll_resp_val // load writeback

  } // leaving gated-clock domain
  val fpuImpl = withClock (gated_clock) { new FPUImpl }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"FPU_$label", "Core;;" + desc)
}
