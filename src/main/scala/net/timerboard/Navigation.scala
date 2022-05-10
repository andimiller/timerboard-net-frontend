package net.timerboard

import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.window
import tyrian.Cmd
import tyrian.cmds.*

object Navigation:
  import tyrian.cmds.Dom.NotFound

  def pushPath[Msg](path: String): Cmd[Msg] =
    Cmd.SideEffect(() =>
      window.history.pushState("", "", window.location.protocol + "//" + window.location.host + "/" + path.stripPrefix("/"))
    )

  def pushUrl[Msg](url: String): Cmd[Msg] =
    Cmd.SideEffect(() => window.history.pushState("", "", url))
