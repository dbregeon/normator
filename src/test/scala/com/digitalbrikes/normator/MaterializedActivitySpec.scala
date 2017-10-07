package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

class MaterializedActivitySpec extends FlatSpec {
  implicit val context : BillContext = new BillContext
  implicit val executionContext : ExecutionContext = ExecutionContext.global

  "A MaterializedActivity required inputs " should " contain the properties that cannot be provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill, BillContext](Set(new PayerSource), Set(AmountNormalizer))

    assert(materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity required inputs " should " not contain a property provided by a node." in {
    val materializedActivity = new MaterializedActivity[Bill, BillContext](Set(new MissingSource[Double, BillContext](Amount)), Set(AmountNormalizer))

    assert(!materializedActivity.requiredInputs.contains(Amount))
  }

  "A  MaterializedActivity " should " return property outputs from the normalizers on the input update." in {
    val source1 = new PayerSource
    val source2 = new PayeeSource
    val source3 = new BillSource

    val materializedActivity = new MaterializedActivity[Bill, BillContext](Set(source1, source2, source3), Set(PayerNormalizer, PayeeNormalizer, BillNormalizer))

    val set = Await.result(materializedActivity.update(Set(
      new PropertyInput[String](PayerId, "Test Payer"),
      new PropertyInput[String](PayeeId, "Test Payee"),
      new PropertyInput[Double](Amount, 30.0))
    ), Duration(10, "seconds"))
    assert(set.equals(Set(
      new PropertyOutput[Party](PayerProperty, Some(Party("Test Payer")), "Validated!"),
      new PropertyOutput[Party](PayeeProperty, Some(Party("Test Payee")), "Validated!"),
      new PropertyOutput[Bill](BillProperty, Some(Bill(Party("Test Payer"), Party("Test Payee"), 30.0)), "Validated!")
    )))
  }
}
