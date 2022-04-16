package chapter11_crdt

final case class GCounter_V1(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) = {
    val value = amount + counters.getOrElse(machine, 0)
    GCounter_V1(counters + (machine -> value))
  }

  def merge(that: GCounter_V1): GCounter_V1 =
    GCounter_V1(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int =
    counters.values.sum
}