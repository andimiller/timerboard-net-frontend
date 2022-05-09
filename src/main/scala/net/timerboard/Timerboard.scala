package net.timerboard

import cats.implicits.*
import diffson.circe.*
import diffson.jsonmergepatch.*
import diffson.jsonpatch.*
import io.circe.*
import io.circe.syntax.*
import tyrian.Html.*
import tyrian.*
import tyrian.cmds.Dom
import tyrian.cmds.Logger
import tyrian.http.*
import tyrian.websocket.WebSocketEvent
import tyrian.websocket.*

import java.time.Duration
import java.time.ZonedDateTime
import scala.concurrent.duration.*
import scala.scalajs.js.annotation.*
import scala.util.Try

import CirceEnumHelpers.*
import InputEffects.*

object MapData:
  private def decodeMapData: Http.Decoder[Map[String, String]] =
    Http.Decoder { response =>
      val json = response.body
      io.circe.parser.parse(json).leftWiden[Throwable].flatMap(_.as[Map[String, String]]).leftMap(_.getLocalizedMessage)
    }

  val getMapData: Cmd[Msg] =
    Http.send(Request.get("./system2region.json", decodeMapData), _.fold(Msg.BadHttp(_), Msg.LoadSystems(_)))

object TimeDiff:
  def apply(now: ZonedDateTime, event: ZonedDateTime): String =
    val d       = Duration.between(now, event)
    val days    = d.toDays
    val hours   = Math.abs(d.toHours % 24)
    val minutes = Math.abs(d.toMinutes & 60)
    val seconds = Math.abs(d.getSeconds % 60)
    s"${days}d ${hours}m ${minutes}m ${seconds}s"

@JSExportTopLevel("TyrianApp")
object Timerboard extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[Msg]) =
    (
      Model(BackendSocket.init, List(), List(), ZonedDateTime.now()),
      Cmd.Batch(
        Cmd.Emit(Msg.WebSocketStatus(BackendSocket.Status.Connecting)),
        MapData.getMapData,
        Dom.focus("search-box")(_ => Msg.None)
      )
    )

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) =
    msg match
      case Msg.LoadSystems(db)    => (model.copy(systems = db), Cmd.Empty)
      case Msg.SortBy(f)          =>
        (
          if (model.sortBy == f)
            model.copy(sortDirection = model.sortDirection.flip)
          else
            model.copy(sortBy = f),
          Cmd.Empty
        )
      case Msg.Payload(j)         =>
        j match
          case WebsocketEvent.Initial(es) =>
            (model.copy(state = es), Cmd.Empty)
          case WebsocketEvent.Diff(patch) =>
            patch[Try](model.state.asJson).toEither
              .flatMap(_.as[List[Event]]) match {
              case Left(e)        =>
                (model, Cmd.Emit(Msg.BadPayload(e)))
              case Right(patched) =>
                (model.copy(state = patched), Cmd.Empty)
            }
      case Msg.BadPayload(e)      => (model, Logger.error(e.getLocalizedMessage))
      case Msg.BadHttp(e)         => (model, Logger.error(e.toString))
      case Msg.Other(s)           => (model.copy(logs = s :: model.logs), Cmd.Empty)
      case Msg.WebSocketStatus(s) =>
        val (nextWS, cmds) = model.socket.update(s)
        (model.copy(socket = nextWS), cmds)
      case Msg.Tick(now)          => (model.copy(now = now), Cmd.Empty)
      case Msg.Search(s)          => (model.copy(search = s), Logger.info(s"searched for $s"))
      case Msg.SearchBackspace    =>
        if (model.search == "") (model.copy(searchTags = model.searchTags.tail), Cmd.Empty) else (model, Cmd.Empty)
      case Msg.EnterTag           => (model.copy(searchTags = model.search :: model.searchTags, search = ""), clear("search-box")(_ => Msg.None))
      case Msg.DeleteTag(s)       => (model.copy(searchTags = model.searchTags.filterNot(_ == s)), Cmd.Empty)
      case Msg.None               => (model, Cmd.Empty)

  def header(model: Model, field: Field, name: String) =
    th(onClick(Msg.SortBy(field)))(
      tyrian.Html.span(name),
      if (field == model.sortBy) model.sortDirection.icon else tyrian.Html.span()
    )

  extension (res: List[RenderEvent])
    def sortByField(f: Field, d: Direction): List[RenderEvent] = d match {
      case Direction.Asc  => sortByField(f)
      case Direction.Desc => sortByField(f).reverse
    }
    def sortByField(f: Field): List[RenderEvent]               = f match {
      case Field.Time          => res.sortBy(_.time)
      case Field.Type          => res.sortBy(_.event_type.toString)
      case Field.System        => res.sortBy(_.system)
      case Field.Region        => res.sortBy(_.region)
      case Field.Owner         => res.sortBy(_.owner)
      case Field.DefenderScore => res.sortBy(_.defender_score)
    }

  def searchTag(s: String) = div(`class` := "badge badge-outline gap-2")(
    i(`class` := "fa-solid fa-xmark", onClick(Msg.DeleteTag(s)))(),
    p(s)
  )

  val inputKeyConfig = onKeyDown {
    case k if k.keyCode == 8  => Msg.SearchBackspace
    case k if k.keyCode == 13 => Msg.EnterTag
    case _                    => Msg.None
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "overflow-x-auto")(
      div(`class` := "form-control")(
        label(`class` := "input-group")(
          tyrian.Html.span(model.searchTags.reverse.map(searchTag)),
          input(id    := "search-box", `type` := "text", `class` := "input w-full", onInput(Msg.Search(_)), inputKeyConfig)
        )
      ),
      table(`class` := "table table-zebra table-compact w-full")(
        thead(
          tr(
            header(model, Field.Type, "Type"),
            header(model, Field.System, "System"),
            header(model, Field.Region, "Region"),
            header(model, Field.Owner, "Owner"),
            header(model, Field.Time, "Time"),
            header(model, Field.Time, "Remaining"),
            header(model, Field.DefenderScore, "Defender Score")
          )
        ),
        tbody(
          model.state
            .map(_.compact(model.now, model.systems))
            .filter(e => (model.search :: model.searchTags).forall(e.matches))
            .sortByField(model.sortBy, model.sortDirection)
            .map { e =>
              tr(
                id := e.id.toString
              )(
                td(e.event_type.toString),
                td(a(href := s"http://evemaps.dotlan.net/search?q=${e.system}")(e.system)),
                td(e.region),
                td(e.owner),
                td(e.time.toString),
                td(e.remaining),
                td(s"${e.defender_score * 100}%")
              )
            }
        )
      )
    )

  def subscriptions(model: Model): Sub[Msg] =
    Sub.Batch(
      model.socket.subscribe {
        case WebSocketEvent.Error(e)            => Msg.Other(e.toString)
        case WebSocketEvent.Receive(r)          =>
          io.circe.parser
            .parse(r)
            .flatMap(j => j.as[WebsocketEvent])
            .fold(Msg.BadPayload(_), Msg.Payload(_))
        case WebSocketEvent.Open                => Msg.Other("opened socket")
        case WebSocketEvent.Close(code, reason) =>
          Msg.Other(s"close: $code, $reason")
        case WebSocketEvent.Heartbeat           => Msg.Other("tick")
      },
      Sub.every(1.second, "tick").map(_ => Msg.Tick(ZonedDateTime.now()))
    )

case class Model(
    socket: BackendSocket,
    logs: List[String],
    state: List[Event],
    now: ZonedDateTime,
    search: String = "",
    searchTags: List[String] = List.empty,
    sortBy: Field = Field.Time,
    sortDirection: Direction = Direction.Asc,
    systems: Map[String, String] = Map.empty
)

enum Direction:
  case Asc, Desc
  def flip = this match
    case Asc  => Desc
    case Desc => Asc
  def icon = this match
    case Asc  => i(`class` := "fa-solid fa-arrow-down-short-wide")("")
    case Desc => i(`class` := "fa-solid fa-arrow-down-wide-short")("")
enum Field:
  case Time, Type, System, Region, Owner, DefenderScore

enum Msg:
  case LoadSystems(db: Map[String, String])
  case SortBy(f: Field)
  case Payload(we: WebsocketEvent)
  case BadHttp(e: HttpError)
  case BadPayload(e: Throwable)
  case Other(s: String)
  case WebSocketStatus(status: BackendSocket.Status)
  case Tick(now: ZonedDateTime)
  case Search(s: String)
  case DeleteTag(s: String)
  case SearchBackspace
  case EnterTag
  case None

enum EventType derives EnumCodec:
  case tcu_defense
  case ihub_defense
  case station_defense
  case station_freeport

case class Participant(
    alliance_id: Long,
    score: BigDecimal
) derives Decoder,
      Encoder.AsObject

case class InnerEvent(
    attackers_score: BigDecimal,
    campaign_id: Long,
    constellation_id: Long,
    defender_id: Long,
    defender_score: BigDecimal,
    event_type: EventType,
    participants: Option[List[Participant]],
    solar_system_id: Long,
    start_time: ZonedDateTime,
    structure_id: Long
) derives Decoder,
      Encoder.AsObject

case class Alliance(
    id: Long,
    ticker: String,
    name: String
) derives Decoder,
      Encoder.AsObject

case class Event(
    solar_system_name: String,
    event: InnerEvent,
    alliance: Alliance
) derives Decoder,
      Encoder.AsObject:
  def compact(now: ZonedDateTime, systems: Map[String, String]): RenderEvent =
    RenderEvent(
      event.campaign_id,
      event.event_type,
      solar_system_name,
      systems.get(solar_system_name).getOrElse("Unknown"),
      alliance.name,
      event.start_time,
      TimeDiff(now, event.start_time),
      event.defender_score
    )

case class RenderEvent(
    id: Long,
    event_type: EventType,
    system: String,
    region: String,
    owner: String,
    time: ZonedDateTime,
    remaining: String,
    defender_score: BigDecimal
) {
  def matches(s: String): Boolean =
    (event_type.toString ++ system ++ region ++ owner).toLowerCase().contains(s.toLowerCase())
}

sealed trait WebsocketEvent
object WebsocketEvent:
  case class Initial(initial: List[Event]) extends WebsocketEvent derives Decoder, Encoder.AsObject
  case class Diff(diff: JsonPatch[Json])   extends WebsocketEvent derives Decoder, Encoder.AsObject
  given Decoder[WebsocketEvent] = Decoder[Initial]
    .widen[WebsocketEvent]
    .orElse(Decoder[Diff].widen[WebsocketEvent])
  given Encoder[WebsocketEvent] =
    case i: Initial => Encoder[Initial].apply(i)
    case d: Diff    => Encoder[Diff].apply(d)
