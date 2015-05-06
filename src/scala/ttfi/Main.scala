package ttfi

import language.experimental.macros

trait Main extends App {
  def println(params: Any*): Unit = macro debug.DebugMacro.debug_impl
}
