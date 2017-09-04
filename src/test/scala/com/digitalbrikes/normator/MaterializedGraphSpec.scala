package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.util.Success

class MaterializedGraphSpec extends FlatSpec {
  "A MaterializedGraph arcs " should " not contain an arc between unrelated sources." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[Double](Amount)
    val graph = new MaterializedGraph(Set(source1, source2))


  }

}
