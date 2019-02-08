package com.ketra85.ArmMachine

import org.scalatra.test.scalatest._

class ServletTests extends ScalatraFunSuite {

  addServlet(classOf[Servlet], "/*")

  test("GET / on Servlet should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
