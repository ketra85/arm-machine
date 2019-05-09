package com.ketra85.ArmMachine

import org.scalatra._
import org.scalatra.scalate.ScalateSupport

class Servlet extends ScalatraServlet with ScalateSupport {
  private def displayPage = Template.page()
  get("/") {
    displayPage
//    contentType = "text/html"

//    ssp("/memView", "layout" -> "WEB-INF/layouts/memLayout.ssp", "memory" -> Memory.readByte(0))
//    views.html.hello()
  }

  object Template {
    def page(): Unit = {
      <html>
        <head>
          <script src="https://pixijs.download/release/pixi.min.js"></script>
        </head>
        <body>
          hello out there
//          insert the drawing script here
          <script></script>
        </body>
      </html>
    }
  }

}