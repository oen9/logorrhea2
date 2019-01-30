package pl.oen.logorrhea2.modules

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object About {
  val component = ScalaComponent.builder[Unit]("About")
    .renderStatic(
      <.div(^.cls := "email-content",
        <.div(^.cls := "email-content-header pure-g",
          <.div(^.cls := "pure-u-1-2",
            <.h1(^.cls := "email-content-title", "About"),
          )
        ),
        <.div(^.cls := "email-content-body",
          <.div(^.cls := "pure-g odd-row",
            <.div(^.cls := "pure-u-1-2", "use"),
            <.div(^.cls := "pure-u-1-2", "do whatever you want!")
          ),
          <.div(^.cls := "pure-g even-row",
            <.div(^.cls := "pure-u-1-2", "github"),
            <.div(^.cls := "pure-u-1-2", <.a(^.target := "_blank", ^.href := "https://github.com/oen9/logorrhea2", "https://github.com/oen9/logorrhea2"))
          ),
          <.div(^.cls := "pure-g odd-row",
            <.div(^.cls := "pure-u-1-2", "heroku"),
            <.div(^.cls := "pure-u-1-2", <.a(^.target := "_blank", ^.href := "https://logorrhea2.herokuapp.com/", "https://logorrhea2.herokuapp.com/"))
          )
        )
      )
    )
    .build

  def apply() = component()
}
