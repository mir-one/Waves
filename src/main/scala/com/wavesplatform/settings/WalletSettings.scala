package one.mir.settings

import java.io.File

import one.mir.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
