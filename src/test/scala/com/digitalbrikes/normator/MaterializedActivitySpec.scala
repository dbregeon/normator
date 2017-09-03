package com.digitalbrikes.normator

import org.scalatest.FlatSpec

class MaterializedActivitySpec extends FlatSpec {
  "A MaterializedActivity required inputs " should " contain the properties that cannot be provided by a node." in {
    val graph = new ResolutionGraph(Set(new PayerSource))
    val materializedActivity = new MaterializedActivity[Bill](graph, Set(AmountNormalizer))

    assert(materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity required inputs " should " not contain a property provided by a node." in {
    val graph = new ResolutionGraph(Set(new MissingSource[Double](Amount)))
    val materializedActivity = new MaterializedActivity[Bill](graph, Set(AmountNormalizer))

    assert(!materializedActivity.requiredInputs.contains(Amount))
  }
}
