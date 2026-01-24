package dev.bosatsu

import munit.Suite

trait ParTest { self: Suite =>

  private var ec: Par.EC = scala.compiletime.uninitialized
  private var es: Par.ExecutionService = scala.compiletime.uninitialized
  override def beforeAll(): Unit = {
    es = Par.newService()
    ec = Par.ecFromService(es)
  }

  override def afterAll(): Unit =
    Par.shutdownService(es)

  implicit def implicitParEC: Par.EC = ec
}
