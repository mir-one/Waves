package tools

import play.api.libs.json.Json

object FirstDifferentBlock extends App {

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def blockAt(nodeHttp: String, blockHeight: Int)    = get(nodeHttp + "/blocks/at/" + blockHeight)
  def blockSigAt(nodeHttp: String, blockHeight: Int) = (Json.parse(blockAt(nodeHttp, blockHeight)) \ "signature").get.as[String]

  def nodeComparator(node1: String, node2: String)(h: Int): Boolean = {
    blockSigAt(node1, h) == blockSigAt(node2, h)
  }

  val TESTNET1 = "http://185.181.164.136:1402"
  val TESTNET2 = "http://185.58.207.12:1402"

  val MAINNET1 = "http://185.181.164.136:1400"
  val MAINNET2 = "http://185.58.207.12:1400"
}
