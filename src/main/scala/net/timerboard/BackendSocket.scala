package net.timerboard

import io.circe.*
import tyrian.Html.*
import tyrian.*
import tyrian.cmds.Logger
import tyrian.websocket.WebSocketEvent
import tyrian.websocket.*

final case class BackendSocket(socketUrl: String, socket: Option[WebSocket]):

  def connectDisconnectButton =
    if socket.isDefined then button(onClick(BackendSocket.Status.Disconnecting.asMsg))("Disconnect")
    else button(onClick(BackendSocket.Status.Connecting.asMsg))("Connect")

  def update(status: BackendSocket.Status): (BackendSocket, Cmd[Msg]) =
    status match
      case BackendSocket.Status.ConnectionError(err) =>
        (this, Logger.error(s"Failed to open WebSocket connection: $err"))

      case BackendSocket.Status.Connected(ws) =>
        (this.copy(socket = Some(ws)), Cmd.Empty)

      case BackendSocket.Status.Connecting =>
        val connect =
          WebSocket.connect(
            address = socketUrl,
            onOpenMessage = "Connect me!",
            keepAliveSettings = KeepAliveSettings.default
          ) {
            case Left(err) => BackendSocket.Status.ConnectionError(err).asMsg
            case Right(ws) => BackendSocket.Status.Connected(ws).asMsg
          }

        (this, connect)

      case BackendSocket.Status.Disconnecting =>
        val log  = Logger.info("Graceful shutdown of BackendSocket connection")
        val cmds =
          socket.map(ws => Cmd.Batch(log, ws.disconnect)).getOrElse(log)

        (this.copy(socket = None), cmds)

      case BackendSocket.Status.Disconnected =>
        (this, Logger.info("WebSocket not connected yet"))

  def publish(message: String): Cmd[Msg] =
    socket.map(_.publish(message)).getOrElse(Cmd.Empty)

  def subscribe(toMessage: WebSocketEvent => Msg): Sub[Msg] =
    socket.fold(Sub.emit(BackendSocket.Status.Disconnected.asMsg)) {
      _.subscribe(toMessage)
    }

object BackendSocket:

  val init: BackendSocket =
    BackendSocket("wss://beta.timerboard.net/stream", None)

  enum Status:
    case Connecting
    case Connected(ws: WebSocket)
    case ConnectionError(msg: String)
    case Disconnecting
    case Disconnected

    def asMsg: Msg = Msg.WebSocketStatus(this)
