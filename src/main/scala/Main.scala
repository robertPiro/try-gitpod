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
            renderDataTable,
            renderDataGraph
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
    
    def renderDataGraph: HtmlElement =
        import typings.chartJs.mod.* 
        import scala.scalajs.js.JSConverters.iterableOnceConvertible2JSRichIterableOnce
        var optChart: Option[Chart] = None

        //function to fill in the chart data
        def fillInChartData(optChart:Option[Chart])(xyData:List[DataItem]) = 
            for chart <- optChart 
            do
                chart.data.labels = xyData.map(_.label).toJSArray
                chart.data.datasets.get(0).data = xyData.map(_.value).toJSArray
                chart.update()

        val chartConf = new ChartConfiguration{
            `type`= ChartType.bar
            data = new ChartData {
                datasets = js.Array(
                    new ChartDataSets {
                        label = "Price"
                        borderWidth = 1
                        backgroundColor = "green"
                    },
                    new ChartDataSets {
                        label = "Full price"
                        borderWidth = 1
                        backgroundColor = "blue"
                    }
                )
            }
        }
        canvasTag(
            width := "100%",
            height := "200px",
            onMountUnmountCallback(
                mount = { nodeCtx => 
                    val ctx = nodeCtx.thisNode.ref
                    val chart = Chart.apply.newInstance2(ctx, chartConf)
                    optChart = Some(chart) // defined as var above 
                },
                unmount = { thisNode => 
                    for chart <- optChart
                    do
                        chart.destroy()
                    optChart = None // defined as var above
                }
            ),
            dataSignal --> {xyData => fillInChartData(optChart)(xyData)} //extracts data from signal

                  
        )
    end renderDataGraph

    
final class DataID  //will create many instances that are compared by address
trait DomRefObj(val id: DataID)
case class DataItem(label: String, value: Double) extends DomRefObj(DataID())
object DataItem:
    def apply(): DataItem = DataItem("?", math.random())



    
