package org.grumpysoft

import java.net.Socket

object Client {

  def main(args: Array[String]) = {
    val socket = new Socket("localhost", 8080)
    val player = RemotePlayer.from(CommandLinePlayer.forStdInAndOut(args.toList.head), socket.getInputStream, socket.getOutputStream)
    while (true) player.readAndForward
  }

}