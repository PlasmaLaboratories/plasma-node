package org.plasmalabs.common

package object application {
  implicit val unitContainsDebugFlag: ContainsDebugFlag[Unit] = _ => false
  implicit val unitContainsUserConfigs: ContainsUserConfigs[Unit] = _ => Nil
}
