package com.ketra85.ArmMachine

import org.scalatra._
import org.scalatra.scalate.ScalateSupport

class Servlet extends ScalatraServlet with ScalateSupport {
  private def displayPage = Template.page()
  get("/") {
    displayPage
  }

  object Template {
    def page(): Unit = {
      <html>
        <head>
          <head>ARM Learning Tool - ALT</head>
        </head>
        <body>
          hello there!
          <script></script>
        </body>
      </html>
    }
  }

}