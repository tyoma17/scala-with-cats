package chapter8_testing_async_code

import scala.concurrent.Future

trait UptimeClient_V1 {
  def getUptime(hostname: String): Future[Int]
}

import cats.instances.future._ // for Applicative
import cats.instances.list._   // for Traverse
import cats.syntax.traverse._  // for traverse
import scala.concurrent.ExecutionContext.Implicits.global

class UptimeService_V1(client: UptimeClient_V1) {
  def getTotalUptime(hostnames: List[String]): Future[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClient_V1(hosts: Map[String, Int]) extends UptimeClient_V1 {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

object UptimeServiceSpec extends App {
  def testTotalUptime() = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient_V1(hosts)
    val service  = new UptimeService_V1(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
//    assert(actual == expected) // compilation issue
  }
  testTotalUptime()
}