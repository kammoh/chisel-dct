/**
 * Created by kamyar on 8/24/15.
 */

package dct

import Chisel._
import dct.Implementation._


case object DataWidth extends Field[Int]

case object PreShift1 extends Field[Int]

case object InvPreShift1 extends Field[Int]

case object PreShift2 extends Field[Int]

case object InvPreShift2 extends Field[Int]

case object N extends Field[Int]

case object NumTests extends Field[Int]

case object OptimizeMCM extends Field[Boolean]

case object Saturate extends Field[Boolean]

case object Impl extends Field[Implementation]

class DCTConfig extends ChiselConfig(
  topDefinitions = { (pname, site, here) => {
    pname match {
      case DataWidth => 8
      case PreShift1 => 5
      case InvPreShift1 => 4
      case PreShift2 => 1
      case InvPreShift2 => 1
      case N => 8
      case Saturate => true
      case Impl => Iterative
      case OptimizeMCM => true
    }
  }
  }
)