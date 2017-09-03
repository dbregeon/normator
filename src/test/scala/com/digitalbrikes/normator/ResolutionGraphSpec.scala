package com.digitalbrikes.normator

import org.scalatest.FlatSpec

import scala.util.Success

class ResolutionGraphSpec extends FlatSpec {
  "A ResolutionGraph arcs " should " not contain an arc between unrelated sources." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[Double](Amount)
    val graph = new ResolutionGraph(Set(source1, source2))

    assert(graph.arcs((source1, PayerId)).isInstanceOf[MissingSource[_]])
    assert(!graph.arcs.keySet.exists(p => p._1 == source2))
  }

  "A ResolutionGraph arcs " should " contains an arc from the providing source to a dependent source." in {
    val source1 = new PayerSource
    val source2 = new MissingSource[String](PayerId)

    val graph = new ResolutionGraph(Set(source1, source2))

    assert(graph.arcs((source1, PayerId)) == source2)
  }

  "A ResolutionGraph arcs " should " contains an arc for each property." in {
    case object BillProperty extends Property
    val source1 = new PayerSource
    val source2 = new PayeeSource
    val source3 = new MissingSource[Double](Amount)
    val source4 = new Source[Bill] {
      override def inputProperties: Set[Property] = Set(PayerProperty, PayeeProperty, Amount)
      override def outputProperty: Property = BillProperty.asInstanceOf[Property]

      override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[Bill] = PropertyValue(
        outputProperty,
        Success(new Bill(
          inputs.find(p => p.property == PayerProperty).get.value.asInstanceOf[Party],
          inputs.find(p => p.property == PayeeProperty).get.value.asInstanceOf[Party],
          inputs.find(p => p.property == Amount).get.value.asInstanceOf[Double]
      )))

    }

    val graph = new ResolutionGraph(Set(source1, source2, source3, source4))

    assert(graph.arcs((source4, PayerProperty)) == source1)
    assert(graph.arcs((source4, PayeeProperty)) == source2)
    assert(graph.arcs((source4, Amount)) == source3)
  }
}
