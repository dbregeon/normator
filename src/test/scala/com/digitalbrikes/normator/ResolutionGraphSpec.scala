package com.digitalbrikes.normator

import org.scalatest.FlatSpec

class ResolutionGraphSpec extends FlatSpec {
  "A ResolutionGraph arcs " should " not contain an arc between unrelated sources." in {
    val source1 = new PayerSource
    val source2 = new InputSource(new PropertyInput[Double](Amount, 0.0))
    val graph = new ResolutionGraph(Set(source1, source2))

    assert(graph.arcs.get(source1).isEmpty)
    assert(graph.arcs.get(source2).isEmpty)
  }

  "A ResolutionGraph arcs " should " contains an arc from the providing source to a dependent source." in {
    val source1 = new PayerSource
    val source2 = new InputSource(new PropertyInput[String](PayerId, "TestId"))

    val graph = new ResolutionGraph(Set(source1, source2))

    assert(graph.arcs(source1) == source2)
  }
}
