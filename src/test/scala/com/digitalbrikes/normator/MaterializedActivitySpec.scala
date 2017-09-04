package com.digitalbrikes.normator

import org.scalatest.FlatSpec

class MaterializedActivitySpec extends FlatSpec {
  "A MaterializedActivity required inputs " should " contain the properties that cannot be provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill](Set(new PayerSource), Set(AmountNormalizer))

    assert(materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity required inputs " should " not contain a property provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill](Set(new MissingSource[Double](Amount)), Set(AmountNormalizer))

    assert(!materializedActivity.requiredInputs.contains(Amount))
  }
}
