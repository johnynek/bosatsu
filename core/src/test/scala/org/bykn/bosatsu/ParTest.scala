package org.bykn.bosatsu

import org.scalatest.{BeforeAndAfterAll, Suite}

trait ParTest extends BeforeAndAfterAll { self: Suite =>

  private var ec: Par.EC = scala.compiletime.uninitialized
  private var es: Par.ExecutionService = scala.compiletime.uninitialized
  override protected def beforeAll(): Unit = {
    es = Par.newService()
    ec = Par.ecFromService(es)
  }

  override protected def afterAll(): Unit =
    Par.shutdownService(es)

  implicit def implicitParEC: Par.EC = ec
}
