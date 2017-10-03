package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.util.Success

class MaterializedActivitySpec extends FlatSpec {
  "A MaterializedActivity required inputs " should " contain the properties that cannot be provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill](Set(new PayerSource), Set(AmountNormalizer))

    assert(materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity required inputs " should " not contain a property provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill](Set(new MissingSource[Double](Amount)), Set(AmountNormalizer))

    assert(!materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity " should " return property outputs from the normalizers on the input update." in {
    val source1 = new PayerSource
    val source2 = new PayeeSource
    val source3 = new BillSource

    val materializedActivity = new MaterializedActivity[Bill](Set(source1, source2, source3), Set(PayerNormalizer, PayeeNormalizer, BillNormalizer))

    assert(materializedActivity.update(Set(
      new PropertyInput[String](PayerId, "Test Payer"),
      new PropertyInput[String](PayeeId, "Test Payee"),
      new PropertyInput[Double](Amount, 30.0))
    ).equals(Set(
      new PropertyOutput[Party](PayerProperty, Some(Party("Test Payer")), "Validated!"),
      new PropertyOutput[Party](PayeeProperty, Some(Party("Test Payee")), "Validated!"),
      new PropertyOutput[Bill](BillProperty, Some(Bill(Party("Test Payer"), Party("Test Payee"), 30.0)), "Validated!")
    )))
  }
}
