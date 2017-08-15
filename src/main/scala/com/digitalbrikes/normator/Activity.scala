package com.digitalbrikes.normator

/**
  * An Activity represents an action to perform from user inputs.
  *
  * @tparam T the type of the outcome when performing the activity.
  */
trait Activity[T] {
  def resolvers : Set[Source[_]]
  def normalizers : Set[Normalizer[_]]

  val resolutionGraph = new ResolutionGraph(resolvers)

  def materialize() : MaterializedActivity[T] = new MaterializedActivity[T](resolutionGraph, normalizers)
}

/**
  * A Normalizer takes a value for a property previously computed (either from direct input or from a Source)
  * and builds an output suitable for presentation.
  * @tparam T the type of the property to output.
  */
trait Normalizer[T] {
  def inputProperties : Set[Property] = Set(outputProperty)

  def outputProperty : Property

  def normalize(value : Option[PropertyValue[T]]) : PropertyOutput[T]

  implicit def asValue(propValue : PropertyValue[T]) : Option[T] = Some(propValue.value)
}

/**
  * Resolved value of a Property
  * @param property
  * @param value
  * @tparam T
  */
case class PropertyValue[T](property: Property, value: T)


/**
  * A Source provides a property's value from a set of input property values.
  * @tparam T
  */
trait Source[T] {
  def inputProperties : Set[Property]
  def outputProperty : Property

  def resolve(inputs : Set[PropertyValue[_]]) : PropertyValue[T]
}

/**
  * Source implicitely created by an input.
  * @param input
  * @tparam T
  */
class InputSource[T](input : PropertyInput[T]) extends Source[T] {
  override def inputProperties: Set[Property] = Set.empty[Property]

  override def outputProperty: Property = input.property

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[T] = PropertyValue(input.property, input.value)
}

/**
  * Value of a property assigned from the outside.
  * @param property
  * @param value
  * @tparam T
  */
case class PropertyInput[T](property: Property, value: T) {
  lazy val source : Source[T] = new InputSource[T](this)
}

/**
  * Output ot an activity for presentation.
  * @param property
  * @param value
  * @param message
  * @tparam T
  */
case class PropertyOutput[+T](property: Property, value: T, message: String)
