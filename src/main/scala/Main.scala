import scala.scalajs.js
import org.scalajs.dom
import scala.math
import com.raquo.laminar.api.L.{*, given}

@main def viteproject():Unit =
    renderOnDomContentLoaded(
        dom.document.querySelector("#app"), Main.appElement()
    )

object Main:
    def appElement(): Element = 
        div(
            h1("Hello Scala.js, Vite and Laminar!"),
            renderDataTable
        )
    //Var: mutible container for (ideally immutable) data to be propagated thru Laminar   
    val dataVar = Var(List(DataItem("one", 1.0)))
    val dataSignal = dataVar.signal  //read-only view of var
    //We cannot directly effect changes but only schedule changes which happen 
    //on the following Event-Loop tick. This is how ScalaJS steps through the program
    //the ticks update the state of Var and the signals propagate transformations of
    //Var back to the UI.  
    def addDataItem(item:DataItem) = dataVar.update(dataList => dataList :+ item)
    def rmDataItem(id: DataID) = dataVar.update(dataList => dataList.filter(_.id != id))
    
    def renderDataItem(id: DataID, sig: Signal[DataItem]): Element =
        val button0 = button("E", onClick --> (_ => rmDataItem(id)))
        tr(
            td(child.text <-- sig.map(_.label)),
            td(child.text <-- sig.map(_.value)),
            td(button0)
        )

    def renderDataTable: Element =
        val button0 = button("+", onClick --> (_ => addDataItem(DataItem())))
        table(
            thead(tr(th("Label"), th("Value"), th("Action"))),
            tbody(children <-- dataSignal.split(_.id): //closure
                (id, initial, itemSignal) => renderDataItem(id, itemSignal)
            ),
            tfoot(tr(td(button0)))
        )

final class DataID  //will create many instances that are compared by address
trait DomRefObj(val id: DataID)
case class DataItem(label: String, value: Double) extends DomRefObj(DataID())
object DataItem:
    def apply(): DataItem = DataItem("?", math.random())



    
