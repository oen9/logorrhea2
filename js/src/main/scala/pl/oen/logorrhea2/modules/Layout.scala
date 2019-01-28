package pl.oen.logorrhea2.modules

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{Resolution, RouterCtl}
import japgolly.scalajs.react.vdom.html_<^._
import diode.react.ModelProxy
import pl.oen.logorrhea2.Logorrhea2Main.{Loc, locs}
import pl.oen.logorrhea2.services.{Root, User}

object Layout {

  case class MenuItem(idx: Int, label: String, location: Loc)

  case class Props(router: RouterCtl[Loc], resolution: Resolution[Loc], proxy: ModelProxy[Root])

  def locsMenu(router: RouterCtl[Loc], resolution: Resolution[Loc]): VdomArray = {
    locs.map(l => {
      <.li(^.cls := "pure-menu-item",
        (^.cls := "pure-menu-selected").when(l == resolution.page),
        ^.key := l.name, router.link(l)(^.cls := "pure-menu-link", l.name))
    }).toVdomArray
  }

  def userList(users: Vector[User]): VdomArray = {
    users.map(u => {
      <.div(^.key := u.id, ^.cls := "email-item email-item-selectd pure-g",
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

  class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      <.div(^.id := "layout", ^.cls := "content pure-g",
        <.div(^.id := "nav", ^.cls := "pure-u",
          <.a(^.cls := "nav-menu-button", "Menu"),
          <.div(^.cls := "nav-inner",

            <.div(^.cls := "pure-menu",
              <.ul(^.cls := "pure-menu-list",
                locsMenu(props.router, props.resolution),
                <.li(^.cls := "pure-menu-heading", "rooms"),
                <.li(^.cls := "pure-menu-item", <.a(^.href := "#room/general", ^.cls := "pure-menu-link", "general")),
                <.li(^.cls := "pure-menu-item", <.a(^.href := "#room/second", ^.cls := "pure-menu-link", "second")),
              )
            )
          )
        ),

        <.div(^.id := "list", ^.cls := "pure-u-1",
          userList(props.proxy.value.users),
          <.div(^.cls := "email-item email-item-selectd pure-g",
            <.div(^.cls := "pure-u", "no users outside room")
          ).when(props.proxy.value.users.isEmpty)
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
