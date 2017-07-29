package io.dac.mara.ports

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

/**
  * Created by dcollins on 8/6/16.
  */
class PortClientHandler extends ChannelInboundHandlerAdapter {
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    println("Active")
    super.channelActive(ctx)
  }
  override def channelRead(ctx: ChannelHandlerContext, msg: Any) = {
    println(s"Read: ${msg}")
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    println(s"Exception Caught: ${cause}")
    ctx.close()
  }

}
