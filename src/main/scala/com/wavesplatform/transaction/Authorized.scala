package one.mir.transaction

import one.mir.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
