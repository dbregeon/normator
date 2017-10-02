package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class MaterializedGraphSpec extends FlatSpec {
  "A MaterializedGraph inputs " should " contains the sources inputs." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[Double](Amount)
    val graph = new MaterializedGraph(Set(source1, source2))

    assert(graph.inputs.equals(source1.inputProperties ++ source2.inputProperties))
  }

  "A MaterializedGraph inputs " should " contains the sources inputs not provided by another source." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[String](PayerId)
    val graph = new MaterializedGraph(Set(source1, source2))

    assert(graph.inputs.isEmpty)
  }

  "A MaterializedGraph recompute " should " provide a value for the source ouput." in {
    val source1 = new PayerSource
    val graph = new MaterializedGraph(Set(source1))

    val set = graph.recompute(Set(new PropertyInput[String](PayerId, "Test Payer")))
    assert(set.equals(Set(
      new PropertyValue[String](PayerId, Success("Test Payer")),
      new PropertyValue[Party](PayerProperty, Success(Party("Test Payer")))
    )))
  }

  "A MaterializedGraph recompute " should " provide a value for the non source ouputs." in {
    case object BillProperty extends Property
    val source1 = new PayerSource
    val source2 = new PayeeSource
    val source3 = new Source[Bill] {
      override def inputProperties: Set[Property] = Set(PayerProperty, PayeeProperty, Amount)
      override def outputProperty: Property = BillProperty.asInstanceOf[Property]

      override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Bill] = PropertyValue(
        outputProperty,
        Success(Bill(
          inputs.find(p => p.property == PayerProperty).get.value.get.asInstanceOf[Party],
          inputs.find(p => p.property == PayeeProperty).get.value.get.asInstanceOf[Party],
          inputs.find(p => p.property == Amount).get.value.get.asInstanceOf[Double]
        )))

    }
    val graph = new MaterializedGraph(Set(source1, source2, source3))

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
      new PropertyValue[Double](Amount, Success(30.0)),
      new PropertyValue[Bill](BillProperty, Success(Bill(Party("Test Payer"), Party("Test Payee"), 30.0)))
    )))
  }

  "A MaterializedGraph recompute " should " provide an error when the input is missing." in {
    val source1 = new PayerSource
    val graph = new MaterializedGraph(Set(source1))

    val set = graph.recompute(Set(new PropertyInput[String](PayeeId, "Test Payee")))
    assert(set.equals(Set(
      new PropertyValue[Party](PayerId, Failure(InsufficientInputException)),
      new PropertyValue[Party](PayerProperty, Failure(MissingInputException("PayerId is missing")))
    )))
  }
}