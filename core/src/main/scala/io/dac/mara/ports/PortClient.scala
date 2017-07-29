package io.dac.mara.ports

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio._
import io.netty.handler.codec.string.{StringDecoder, StringEncoder}
import io.netty.util.CharsetUtil

/**
  * Created by dcollins on 8/6/16.
  */
class PortClient {
  val group = new NioEventLoopGroup()

  def connect(group: EventLoopGroup): ChannelFuture = {
    val bootstrap = new Bootstrap()
      .group(group)
      .channel(classOf[NioSocketChannel])
      .option[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
      .handler(new ChannelInitializer[SocketChannel]{
        override def initChannel(ch: SocketChannel): Unit = {
          val pipeline = ch.pipeline()
          println("Adding handler to pipeline")
          pipeline.addLast(new StringEncoder(CharsetUtil.UTF_8))
          pipeline.addLast(new StringDecoder(CharsetUtil.UTF_8))
          pipeline.addLast(new PortClientHandler)
        }
      })

    bootstrap.connect("127.0.0.1", 12345).sync()
  }

  def start: Unit = {
    try {
      val future = connect(group)
      val channel = future.channel()

      val message = "D0\"hello, world\""
      val buffer = Unpooled.buffer(message.length)

      message.foreach { ch =>
        buffer.writeByte(ch.toInt)
      }

      channel.writeAndFlush(buffer).sync()

    } finally {
      Thread.sleep(10000)
      group.shutdownGracefully()
      println("Shut down")
    }
  }
}

object PortClient {
  def main(args: Array[String]): Unit = {
    val client = new PortClient
    client.start
  }
}
