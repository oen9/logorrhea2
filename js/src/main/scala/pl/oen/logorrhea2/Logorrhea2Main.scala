package pl.oen.logorrhea2

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import pl.oen.logorrhea2.modules.{About, Layout, Room}
import pl.oen.logorrhea2.services.AppCircuit

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Logorrhea2Main")
object Logorrhea2Main {

  sealed abstract class Loc(val name: String)
  case object AboutLoc extends Loc("about")
  case object ChangeNameLoc extends Loc("change name")
  case object NewRoomLoc extends Loc("new room")
  case class RoomLoc(roomName: String) extends Loc(roomName)

  val locs: List[Loc]= List(ChangeNameLoc, NewRoomLoc, AboutLoc)

  @JSExport
  def main(target: html.Div): Unit = {

    val homeWrapper = AppCircuit.connect(_.root)
    def layoutWrapper(ctl: RouterCtl[Loc], resolution: Resolution[Loc]) = homeWrapper(p => Layout(ctl, resolution, p))

    val routerConfig = RouterConfigDsl[Loc].buildConfig { dsl =>
      import dsl._

      (emptyRule
//        | staticRoute(root, RoomLoc) ~> render(homeWrapper(Home(_)))
        | staticRoute(root, AboutLoc) ~> render(About())
        | staticRoute("#changename", ChangeNameLoc) ~> render(About())
        | staticRoute("#newroom", NewRoomLoc) ~> render(About())
        | staticRoute("#about", AboutLoc) ~> render(About())
        | dynamicRouteCT("#room" / remainingPath.caseClass[RoomLoc]) ~> dynRender(roomName => homeWrapper(Room(roomName.name)))
        )
        .notFound(redirectToPage(AboutLoc)(Redirect.Replace))
        .setTitle(loc => s"logorrhea2 - ${loc.name}")
    }.renderWith(layoutWrapper)


    val router = Router(BaseUrl.until_#, routerConfig)
    router().renderIntoDOM(target)
  }
}
