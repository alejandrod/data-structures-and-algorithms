package sections.section3

import org.scalameter.api._

import scala.util.Random

object Section3Benchmark extends Bench.LocalTime {
  val seeds: Gen[Int] = Gen.range("seed")(0, 100000, 100)

  val groups: Gen[Array[String]] = for (seed <- seeds) yield {
    val a = Array.fill(seed)(s"character_$seed")
    if (seed > 0) {
      a(Random.nextInt(seed).min(seed - 1)) = "nemo"
    }
    a
  }

  performance of "section3" in {
    measure method "findNemo" in {
      using(groups) config(
        exec.benchRuns -> 30,
        exec.independentSamples -> 5
      ) in {
        r => findNemo(r)
      }
    }
  }
}
