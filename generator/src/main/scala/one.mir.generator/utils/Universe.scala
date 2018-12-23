package one.mir.generator.utils

import one.mir.account.PrivateKeyAccount
import one.mir.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
