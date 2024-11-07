package io.github.searler.zio_tcp

import zio._
import zio.test.Assertion.{equalTo, isEmpty}
import zio.test._

import java.net.InetAddress

object AddressSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Live]] = Chunk(TestAspect.timeout(1.seconds))

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Address")(

    test("localhost is loopback") {

      for {
        addresses <- Address.byName(Set("localhost"))
      } yield assert(addresses)(equalTo(Map("localhost" -> InetAddress.getLoopbackAddress)))
    },

    test("::1 is v6 loopback") {

      for {
        addresses <- Address.byName(Set("::1"))
      } yield assert(addresses)(equalTo(Map("::1" -> InetAddress.getByName("0:0:0:0:0:0:0:1"))))
    }
    ,
    test("Non existent") {

      for {
        addresses <- Address.byName(Set("nonexistent_really"))
      } yield assert(addresses)(isEmpty)
    },

    test("localhost AND nonexistent is loopback") {

      for {
        addresses <- Address.byName(Set("localhost", "nonexistent_really"))
      } yield assert(addresses)(equalTo(Map("localhost" -> InetAddress.getLoopbackAddress)))
    }

  )
}
