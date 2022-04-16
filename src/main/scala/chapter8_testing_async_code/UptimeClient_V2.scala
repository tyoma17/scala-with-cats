package chapter8_testing_async_code

import cats.{Applicative, Id}

import scala.concurrent.Future

trait UptimeClient_V2[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient_V2[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient_V2 extends UptimeClient_V2[Id] {
  def getUptime(hostname: String): Int
}

class TestUptimeClientImpl(hosts: Map[String, Int]) extends UptimeClient_V2[Id] {
  def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
}
import cats.instances.list._   // for Traverse
import cats.syntax.traverse._  // for traverse
import cats.syntax.functor._

class UptimeService_V2[F[_]: Applicative](client: UptimeClient_V2[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

object Demo extends App {
  def testTotalUptime() = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClientImpl(hosts)
    val service  = new UptimeService_V2(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}