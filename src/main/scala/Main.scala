import scala.scalajs.js
import org.scalajs.dom

import scala.math
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLInputElement


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

    //Var: mutable container for (ideally immutable) data to be propagated thru Laminar
    val dataVar = Var(List( DataItem(DataID(),"one", 1.0)))
    val dataSignal = dataVar.signal  //read-only view of var
    //We cannot directly affect changes but only schedule changes which happen
    //on the following Event-Loop tick. This is how ScalaJS steps through the program
    //the ticks update the state of Var and the signals propagate transformations of
    //Var back to the UI.  
    def addDataItem(item: DataItem) = dataVar.update(dataList => dataList :+ item)
    def rmDataItem(id: DataID) = dataVar.update(dataList => dataList.filter(_.id != id))

    def dataItemUpdater[A](id: DataID, f: (DataItem, A) => DataItem): Observer[A] =
        dataVar.updater[A]:
            //data: contents of var, which we know to be List
            //newValue: the only free variable
            (data, newValue) => data.map(item => if item.id == id then f(item, newValue) else item)

    def inputStringElement(valueSignal: Signal[String], valueUpdater: Observer[String]):Input =
        input(
            typ := "text",
            // value reads from the signal which is set by valueUpdater. Note that the valueUpdater
            // acts like a sinkhole, writing to dataVar which then surfaces as the valueSignal setting
            // the value of the input field. For this reason, intercepting and delaying the signal here
            // is not useful.
            controlled(
                onInput.mapToValue --> valueUpdater, // --> dataVar --> valueSignal
                value <-- valueSignal
            ),
        )


    def renderDataItem(id: DataID, sig: Signal[DataItem]): Element =
        val button0 = button("E", onClick --> (_ => rmDataItem(id)))
        val inputElement = inputStringElement(
            sig.map(_.label),
            dataItemUpdater[String](id, (item, newLabel) => item.copy(label=newLabel)))
        tr(
            td(inputElement),
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
        def fillInChartData(optChart:Option[Chart])(xyData:List[DataItem]): Unit =
            for chart <- optChart 
            do
                chart.data.labels = xyData.map(_.label).toJSArray
                chart.data.datasets.get(0).data = xyData.map(_.value).toJSArray
                chart.update()

        val chartConf = new ChartConfiguration:
            `type`= ChartType.bar
            data  = new ChartData:
                datasets = js.Array(
                    new ChartDataSets:
                        label = "Price"
                        borderWidth = 1
                        backgroundColor = "green"
                    ,
                    new ChartDataSets:
                        label = "Full price"
                        borderWidth = 1
                        backgroundColor = "blue"
                )

        canvasTag(
            width := "100%",
            height := "200px",
            onMountUnmountCallback(
                mount = nodeCtx =>
                    val ctx = nodeCtx.thisNode.ref
                    val chart = Chart.apply.newInstance2(ctx, chartConf)
                    optChart = Some(chart) // defined as var above 
                ,
                unmount = thisNode =>
                    for chart <- optChart
                    do
                        chart.destroy()
                    optChart = None // defined as var above

            ),
            dataSignal --> {xyData => fillInChartData(optChart)(xyData)} //extracts data from signal
        )
    end renderDataGraph

    
final class DataID  //will create many instances that are compared by address
//Case class needs `id` in constructor args for func .copy(...) to copy it.
case class DataItem(id: DataID, label: String, value: Double):
    override def toString(): String = label

object DataItem:
    def apply(): DataItem = DataItem(DataID(), "?", math.random())



    
