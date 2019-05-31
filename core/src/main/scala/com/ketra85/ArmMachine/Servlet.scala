package com.ketra85.ArmMachine

import org.scalatra._
import org.scalatra.scalate.ScalateSupport

class Servlet extends ScalatraServlet with ScalateSupport {
  private def displayPage(title: String) = Template.page(title)
  get("/") {
    displayPage("ARM Learning Tool - ALT")
  }

  object Template {
    def page(title: String): Unit = {
      <html>
        <head>
          <title>{ title }</title>
        </head>
        <body>
          hello there!
        </body>
      </html>
    }
  }
}