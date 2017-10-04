package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class MaterializedGraphSpec extends FlatSpec {
  implicit val context : BillContext = new BillContext

  "A MaterializedGraph inputs " should " contains the sources inputs." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[Double, BillContext](Amount)
    val graph = new MaterializedGraph[BillContext](Set(source1, source2))

    assert(graph.inputs.equals(source1.inputProperties ++ source2.inputProperties))
  }

  "A MaterializedGraph inputs " should " contains the sources inputs not provided by another source." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[String, BillContext](PayerId)
    val graph = new MaterializedGraph[BillContext](Set(source1, source2))

    assert(graph.inputs.isEmpty)
  }

  "A MaterializedGraph recompute " should " provide a value for the source ouput." in {
    val source1 = new PayerSource
    val graph = new MaterializedGraph[BillContext](Set(source1))

    val set = graph.recompute(Set(new PropertyInput[String](PayerId, "Test Payer")))
    assert(set.equals(Set(
      new PropertyValue[String](PayerId, Success("Test Payer")),
      new PropertyValue[Party](PayerProperty, Success(Party("Test Payer")))
    )))
  }

  "A MaterializedGraph recompute " should " provide a value for the non source ouputs." in {
    val source1 = new PayerSource
    val source2 = new PayeeSource
    val source3 = new BillSource
    val graph = new MaterializedGraph[BillContext](Set(source1, source2, source3))

    val set = graph.recompute(Set(
      new PropertyInput[String](PayerId, "Test Payer"),
      new PropertyInput[String](PayeeId, "Test Payee"),
      new PropertyInput[Double](Amount, 30.0)
    ))
    assert(set.equals(Set(
      new PropertyValue[Double](Amount, Success(30.0)),
      new PropertyValue[String](PayerId, Success("Test Payer")),
      new PropertyValue[String](PayeeId, Success("Test Payee")),
      new PropertyValue[Party](PayerProperty, Success(Party("Test Payer"))),
      new PropertyValue[Party](PayeeProperty, Success(Party("Test Payee"))),
      new PropertyValue[Bill](BillProperty, Success(Bill(Party("Test Payer"), Party("Test Payee"), 30.0)))
    )))
  }

  "A MaterializedGraph recompute " should " provide an error when the input is missing." in {
    val source1 = new PayerSource
    val graph = new MaterializedGraph[BillContext](Set(source1))

    val set = graph.recompute(Set(new PropertyInput[String](PayeeId, "Test Payee")))
    assert(set.equals(Set(
      new PropertyValue[Party](PayerId, Failure(InsufficientInputException)),
      new PropertyValue[Party](PayerProperty, Failure(MissingInputException("PayerId is missing")))
    )))
  }
}
