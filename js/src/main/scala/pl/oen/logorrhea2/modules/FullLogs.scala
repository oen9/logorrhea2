package pl.oen.logorrhea2.modules

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import pl.oen.logorrhea2.services.{LogError, LogMsg, LogOk}

object FullLogs {

  case class Props(proxy: ModelProxy[List[LogMsg]])

  def logMsg(logs: Seq[LogMsg]): VdomArray = {
    logs.zip(logs.size to 0 by -1).map { case (log, id) =>
      val infoTextCls = log.status match {
        case LogOk => "info-text-ok"
        case LogError => "info-text-error"
      }
      <.div(^.key := id, ^.cls := "email-item email-item-selectd pure-g",
        <.div(^.cls := "pure-u", ^.cls := infoTextCls, log.msg)
      )
    }.toVdomArray
  }

  val component = ScalaComponent.builder[Props]("Full logs")
    .render_P(props =>
      <.div(^.cls := "email-content",
        <.div(^.cls := "email-content-header pure-g",
          <.div(^.cls := "pure-u-1-2",
            <.h1(^.cls := "email-content-title", "Full logs"),
          )
        ),
        <.div(^.cls := "email-content-body",
          logMsg(props.proxy.value)
        )
      )
    )
    .build

  def apply(proxy: ModelProxy[List[LogMsg]]) = component(Props(proxy))
}
