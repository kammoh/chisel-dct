/**
 * Created by kamyar on 8/24/15.
 */

package dct

import Chisel._
import dct.Implementation._


case object InWidth extends Field[Int]

case object OutWidth extends Field[Int]

case object FwdShift extends Field[Int]

case object InvShift extends Field[Int]

case object PreShift1 extends Field[Int]

case object PreShift2 extends Field[Int]

case object N extends Field[Int]

case object NumTests extends Field[Int]

case object OptimizeMCM extends Field[Boolean]

case object Saturate extends Field[Boolean]

case object ModName extends Field[() => String]

case object Impl extends Field[Implementation]

class DCTConfig extends ChiselConfig(
  topDefinitions = { (pname, site, here) => {
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any, default: Any = None):Any = {
      (here[PF](site[Any](sname)) orElse PartialFunction[Any,Any]({case pname => default}) ) (pname)
    }
    def findByMod(default: Any = None) = findBy(ModName, default)
    PartialFunction[Any,Any]{
      case InWidth => findByMod(8)
      case OutWidth => findByMod(default = 8)
      case PreShift1 => findByMod(0)
      case PreShift2 => findByMod(0)
      case N => 8
      case Saturate => true
      case Impl => findBy(ModName, Iterative)
      case OptimizeMCM => true
      case FwdShift => 8
      case InvShift => 6
      case "DCTFwd" => {
        case PreShift1 => 5
        case PreShift2 => 1
//        case OutWidth => 16

      }:PF
      case "DCTInv" => {
//        case InWidth => 16
//        case OutWidth => 24
        case PreShift1 => 4
        case PreShift2 => 1
        case Impl => Combinational
      }:PF
      case "DCTB2B" => {
        case InWidth => 8
//        case OutWidth => 24
      }:PF
    }(pname)
  }
  }
)