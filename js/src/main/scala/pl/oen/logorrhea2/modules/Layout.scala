package pl.oen.logorrhea2.modules

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{Resolution, RouterCtl}
import japgolly.scalajs.react.vdom.html_<^._
import diode.react.ModelProxy
import pl.oen.logorrhea2.Logorrhea2Main.{Loc, RoomLoc, locs}
import pl.oen.logorrhea2.services._
import cats.implicits._

import scala.scalajs.js.URIUtils

object Layout {

  case class MenuItem(idx: Int, label: String, location: Loc)

  case class Props(router: RouterCtl[Loc], resolution: Resolution[Loc], proxy: ModelProxy[Root])

  def locsMenu(router: RouterCtl[Loc], resolution: Resolution[Loc]): VdomArray = {
    locs.map(l => {
      <.li(^.key := l.name, ^.cls := "pure-menu-item",
        (^.cls := "pure-menu-selected").when(l == resolution.page),
        ^.key := l.name, router.link(l)(^.cls := "pure-menu-link", l.name))
    }).toVdomArray
  }

  def userListFromRoom(t: (RoomData, User)): VdomArray = userList(t._1.users, t._2)

  def userListJustMe(me: User): VdomArray = userList(Seq(me), me)

  def userList(users: Seq[User], me: User): VdomArray = {
    users.map(u => {
      <.div(^.key := u.id,
        ^.cls := s"email-item email-item-selectd pure-g",
        (^.cls := "email-item-unread").when(u.id == me.id),
        <.div(^.cls := "pure-u",
          <.img(^.width := "64", ^.height := "64", ^.alt := "avatar", ^.cls := "email-avatar", ^.src := "front-res/img/default-user-icon-8.jpg")
        ),

        <.div(^.cls := "pure-u-3-4",
          <.h5(^.cls := "email-name", u.id),
          <.h4(^.cls := "email-subject", u.name),
          <.p(^.cls := "email-dsc", u.lastMsg)
        )
      )
    }).toVdomArray
  }

  def logMsg(logs: Seq[LogMsg]): VdomArray = {
    logs.take(3).zipWithIndex.map { case (log, id) =>
      val infoTextCls = log.status match {
        case LogOk => "info-text-ok"
        case LogError => "info-text-error"
      }
      <.div(^.key := id, ^.cls := "email-item email-item-selectd pure-g",
        <.div(^.cls := "pure-u", ^.cls := infoTextCls, log.msg)
      )
    }.toVdomArray
  }

  def roomsList(router: RouterCtl[Loc], resolution: Resolution[Loc], rooms: Vector[String]): VdomArray = {
    rooms.map { roomName =>
      val loc = RoomLoc(roomName)
      val decodedRoomName = URIUtils.decodeURI(roomName)
      <.li(^.key := roomName, ^.cls := "pure-menu-item",
        (^.cls := "pure-menu-selected").when(loc == resolution.page),
        router.link(loc)(^.cls := "pure-menu-link", decodedRoomName))
    }.toVdomArray
  }

  class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      val maybeMe = props.proxy.value.me
      val maybeRoom = props.proxy.value.roomData
      val logs = props.proxy.value.logs

      <.div(^.id := "layout", ^.cls := "content pure-g",
        <.div(^.id := "nav", ^.cls := "pure-u",
          <.a(^.cls := "nav-menu-button", "Menu"),
          <.div(^.cls := "nav-inner",

            <.div(^.cls := "pure-menu",
              <.ul(^.cls := "pure-menu-list",
                locsMenu(props.router, props.resolution),
                <.li(^.cls := "pure-menu-heading", "rooms"),
                roomsList(props.router, props.resolution, props.proxy.value.rooms)
              )
            )
          )
        ),

        <.div(^.id := "list", ^.cls := "pure-u-1",
          (maybeRoom, maybeMe).bisequence
            .map(userListFromRoom)
            .orElse(maybeMe.map(userListJustMe))
            .getOrElse(logMsg(logs))
        ),

        <.div(^.id := "main", ^.cls := "pure-u-1",
          props.resolution.render()
        )
      )
    }
  }

  val component = ScalaComponent.builder[Props]("Layout")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[Loc], resolution: Resolution[Loc], proxy: ModelProxy[Root]) = component(Props(ctl, resolution, proxy))
}
