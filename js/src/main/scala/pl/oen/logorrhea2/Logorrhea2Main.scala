package pl.oen.logorrhea2

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import pl.oen.logorrhea2.modules._
import pl.oen.logorrhea2.services.AppCircuit

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Logorrhea2Main")
object Logorrhea2Main {

  sealed abstract class Loc(val name: String)
  case object AboutLoc extends Loc("about")
  case object ChangeNameLoc extends Loc("change name")
  case object NewRoomLoc extends Loc("new room")
  case object FullLogsLoc extends Loc("full logs")
  case class RoomLoc(roomName: String) extends Loc(roomName)

  val locs: List[Loc]= List(ChangeNameLoc, NewRoomLoc, AboutLoc, FullLogsLoc)

  @JSExport
  def main(target: html.Div): Unit = {

    val rootWrapper = AppCircuit.connect(_.root)
    val logsWrapper = AppCircuit.connect(_.root.logs)
    val meWrapper = AppCircuit.connect(_.root.me)

    def layoutWrapper(ctl: RouterCtl[Loc], resolution: Resolution[Loc]) = rootWrapper(p => Layout(ctl, resolution, p))

    val routerConfig = RouterConfigDsl[Loc].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, ChangeNameLoc) ~> render(rootWrapper(ChangeName(_)))
        | staticRoute("#changename", ChangeNameLoc) ~> render(rootWrapper(ChangeName(_)))
        | staticRoute("#newroom", NewRoomLoc) ~> render(meWrapper(NewRoom(_)))
        | staticRoute("#about", AboutLoc) ~> render(About())
        | staticRoute("#fulllogs", FullLogsLoc) ~> render(logsWrapper(FullLogs(_)))
        | dynamicRouteCT("#room" / remainingPath.caseClass[RoomLoc]) ~> dynRenderR((roomName, router) => rootWrapper(Room(roomName.name, router)))
        )
        .notFound(redirectToPage(AboutLoc)(Redirect.Replace))
        .setTitle(loc => s"logorrhea2 - ${loc.name}")
    }.renderWith(layoutWrapper)


    val router = Router(BaseUrl.until_#, routerConfig)
    router().renderIntoDOM(target)
  }
}
