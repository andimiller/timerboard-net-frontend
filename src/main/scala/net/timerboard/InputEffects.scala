package net.timerboard

import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLInputElement
import tyrian.Cmd
import tyrian.cmds.*

/** Dom utilities */
object InputEffects:

  import tyrian.cmds.Dom.NotFound

  /** Focus (highlight) on a DOM input element */
  def clear[Msg](elementId: String)(resultToMessage: Either[NotFound, Unit] => Msg): Cmd[Msg] =
    affectInputElement(elementId, _.value = "", resultToMessage)

  def setContents(elementId: String)(value: String): Cmd[Nothing] =
    Cmd.SideEffect(() => document.getElementById(elementId).asInstanceOf[HTMLInputElement].value = value)

  @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
  private def affectInputElement[Msg](
      elementId: String,
      modifier: HTMLInputElement => Unit,
      resultToMessage: Either[NotFound, Unit] => Msg
  ): Cmd[Msg] =
    Cmd
      .Run[NotFound, Unit] { observer =>
        val node = document.getElementById(elementId)

        if node != null then observer.onNext(modifier(node.asInstanceOf[HTMLInputElement]))
        else observer.onError(NotFound(elementId))

        () => ()
      }
      .attempt(resultToMessage)
