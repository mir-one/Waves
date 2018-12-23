package one.mir.lang

import one.mir.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = one.mir.lang.Global // Hack for IDEA
}
