package vexriscv.demo


import vexriscv.plugin._
import vexriscv._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.jtag.sim.JtagTcp
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{Axi4VgaCtrl, Axi4VgaCtrlGenerics, Vga}
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram.SdramGeneration.SDR
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.memory.sdram.sdr.{Axi4SharedSdramCtrl, IS42x320D, SdramInterface, SdramTimings}
import spinal.lib.misc.HexTools
import spinal.lib.soc.pinsec.{PinsecTimerCtrl, PinsecTimerCtrlExternal}
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, JtagBridge, SystemDebugger, SystemDebuggerConfig}

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

case class BrieyConfig(axiFrequency : HertzNumber,
                       onChipRamSize : BigInt,
                       cpuPlugins : ArrayBuffer[Plugin[VexRiscv]],
                       uartCtrlConfig : UartCtrlMemoryMappedConfig)

object BrieyConfig{

  def default = {
    val config = BrieyConfig(
      axiFrequency = 50 MHz,
      onChipRamSize  = 4 kB,
      uartCtrlConfig = UartCtrlMemoryMappedConfig(
        uartCtrlConfig = UartCtrlGenerics(
          dataWidthMax      = 8,
          clockDividerWidth = 20,
          preSamplingSize   = 1,
          samplingSize      = 5,
          postSamplingSize  = 2
        ),
        txFifoDepth = 16,
        rxFifoDepth = 16
      ),
      cpuPlugins = ArrayBuffer(
        new PcManagerSimplePlugin(0x80000000l, false),
        new IBusSimplePlugin(
          resetVector = 0x80000000l,
          cmdForkOnSecondStage = false,
          cmdForkPersistence = true,
          prediction = STATIC,
          catchAccessFault = false,
          compressedGen = false
        ),
        new DBusSimplePlugin(
          catchAddressMisaligned = false,
          catchAccessFault = false,
          emitCmdInMemoryStage = true
        ),
        new StaticMemoryTranslatorPlugin(
          ioRange      = _(31 downto 28) === 0xF
        ),
        new DecoderSimplePlugin(
          catchIllegalInstruction = true
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = true
        ),
        new FullBarrelShifterPlugin,
        new MulPlugin,
        new DivPlugin,
        new HazardSimplePlugin(
          bypassExecute           = true,
          bypassMemory            = true,
          bypassWriteBack         = true,
          bypassWriteBackBuffer   = true,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
        ),
        new CsrPlugin(
          config = CsrPluginConfig(
            catchIllegalAccess = false,
            mvendorid      = null,
            marchid        = null,
            mimpid         = null,
            mhartid        = null,
            misaExtensionsInit = 66,
            misaAccess     = CsrAccess.NONE,
            mtvecAccess    = CsrAccess.NONE,
            mtvecInit      = 0x80000020l,
            mepcAccess     = CsrAccess.READ_WRITE,
            mscratchGen    = false,
            mcauseAccess   = CsrAccess.READ_ONLY,
            mbadaddrAccess = CsrAccess.READ_ONLY,
            mcycleAccess   = CsrAccess.NONE,
            minstretAccess = CsrAccess.NONE,
            ecallGen       = false,
            wfiGenAsWait         = false,
            ucycleAccess   = CsrAccess.NONE,
            uinstretAccess = CsrAccess.NONE
          )
        ),
        new YamlPlugin("cpu0.yaml")
      )
    )
    config
  }
}



class Briey(val config: BrieyConfig) extends Component{

  //Legacy constructor
  def this(axiFrequency: HertzNumber) {
    this(BrieyConfig.default.copy(axiFrequency = axiFrequency))
  }

  import config._
  val debug = true
  val interruptCount = 4

  val io = new Bundle{
    //Clocks / reset
    val asyncReset = in Bool()
    val axiClk     = in Bool()

    //Main components IO
    val jtag       = slave(Jtag())
    // Peripherals IO
    //Peripherals IO
    val gpioA         = master(TriStateArray(32 bits))
    val gpioB         = master(TriStateArray(32 bits))
    val uart          = master(Uart())
    val timerExternal = in(PinsecTimerCtrlExternal())
    val coreInterrupt = in Bool()
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.axiClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val systemResetUnbuffered  = False
    //    val coreResetUnbuffered = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automaticly do a reset when the system boot.
    val systemResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemResetCounter =/= U(systemResetCounter.range -> true)){
      systemResetCounter := systemResetCounter + 1
      systemResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)){
      systemResetCounter := 0
    }

    //Create all reset used later in the design
    val systemReset  = RegNext(systemResetUnbuffered)
    val axiReset     = RegNext(systemResetUnbuffered)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(axiFrequency) //The frequency information is used by the SDRAM controller
  )

  val debugClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(axiFrequency)
  )

  val axi = new ClockingArea(axiClockDomain) {
    val ahblite_config = AhbLite3Config(
      addressWidth = 32,
      dataWidth = 32
    )

    val apb3_config = Apb3Config(
      addressWidth = 32,
      dataWidth = 32
    )

    val ram = AhbLite3OnChipRam(
      AhbLite3Config = ahblite_config,
      byteCount = onChipRamSize
    )

    val apbBridge = new AhbLite3ToApb3Bridge(
      ahbConfig = ahblite_config,
      apbConfig = apb3_config
    )

    val gpioACtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )
    val gpioBCtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )
    val timerCtrl = PinsecTimerCtrl()


    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
    uartCtrl.io.apb.addAttribute(Verilator.public)

    val core = new Area{
      val config = VexRiscvConfig(
        plugins = cpuPlugins += new DebugPlugin(debugClockDomain)
      )

      val cpu = new VexRiscv(config)
      var iBus : AhbLite3 = null
      var dBus : AhbLite3 = null
      for(plugin <- config.plugins) plugin match{
        case plugin: IBusSimplePlugin => {
          val iBustmp = new IBusSimpleBus(plugin)
          iBustmp.cmd << plugin.iBus.cmd.halfPipe()
          iBustmp.rsp <> plugin.iBus.rsp
          iBus = iBustmp.toAhbLite3Master().toAhbLite3()
        }
        case plugin: DBusSimplePlugin => {
          val dBustmp = new DBusSimpleBus
          dBustmp.cmd << plugin.dBus.cmd.halfPipe()
          dBustmp.rsp <> plugin.dBus.rsp
          dBus = dBustmp.toAhbLite3Master(avoidWriteToReadHazard = true).toAhbLite3()
        }
        case plugin : CsrPlugin        => {
          plugin.externalInterrupt := BufferCC(io.coreInterrupt)
          plugin.timerInterrupt := timerCtrl.io.interrupt
        }
        case plugin : DebugPlugin      => debugClockDomain{
          resetCtrl.axiReset setWhen(RegNext(plugin.io.resetOut))
          io.jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }

    val crossbar = AhbLite3CrossbarFactory(ahblite_config)
    crossbar.addSlaves(
      ram.io.ahb       -> (0x80000000L,   onChipRamSize),
      apbBridge.io.ahb -> (0xF0000000L,   1 MB)
    )
    crossbar.addConnections(
      core.iBus -> List(ram.io.ahb),
      core.dBus -> List(ram.io.ahb, apbBridge.io.ahb)
    )
    crossbar.build()

   val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 kB),
        gpioBCtrl.io.apb -> (0x01000, 4 kB),
        uartCtrl.io.apb  -> (0x10000, 4 kB),
        timerCtrl.io.apb -> (0x20000, 4 kB)
      )
    )
  }
  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.gpioB          <> axi.gpioBCtrl.io.gpio
  io.timerExternal  <> axi.timerCtrl.io.external
  io.uart           <> axi.uartCtrl.io.uart
}

//DE1-SoC
object Briey{
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new Briey(BrieyConfig.default)
      toplevel
    })
  }
}