import scala.scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

@main def viteproject():Unit =
    renderOnDomContentLoaded(
        dom.document.querySelector("#app"), Main.appElement()
    )

object Main:
    def appElement(): Element = {
        div(
            h1("Hello Scala.js, Vite and Laminar!")
        )

    }


    
