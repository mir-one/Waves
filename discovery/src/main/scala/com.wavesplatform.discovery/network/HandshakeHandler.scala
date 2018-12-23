package one.mir.discovery.network

import java.util

import one.mir.Version
import one.mir.network.Handshake
import one.mir.settings.Constants
import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ReplayingDecoder
import scorex.utils.ScorexLogging

import scala.util.Random

class HandshakeHandler(chainId: Char) extends ReplayingDecoder[Void] with ScorexLogging {
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, "discovery", new Random().nextLong(), None)

  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit = {
    out.add(Handshake.decode(in))
    ctx.pipeline().remove(this)
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
  }
}
