/**
 * Created by kamyar on 8/25/15.
 */

import Chisel._

package object ConstMult {
  implicit class ConstTypeSInt(sint: SInt) {
    def *&(rhs: Int) : SInt = {
      ConstMult(rhs, sint)
    }
  }
  implicit class ConstTypeInt(const: Int) {
    def *&(rhs: SInt) : SInt = {
      ConstMult(const, rhs)
    }
  }
}