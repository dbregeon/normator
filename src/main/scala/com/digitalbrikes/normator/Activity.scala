package com.digitalbrikes.normator

import scala.util.Try

/**
  * An Activity represents an action to perform from user inputs.
  *
  * @tparam T the type of the outcome when performing the activity.
  */
trait Activity[T] {
  def resolvers : Set[Source[_]]
  def normalizers : Set[Normalizer[_]]

  def materialize() : MaterializedActivity[T] = new MaterializedActivity[T](resolvers, normalizers)
}

/**
  * A Normalizer takes a value for a property previously computed (either from direct input or from a Source)
  * and builds an output suitable for presentation.
  * @tparam T the type of the property to output.
  */
trait Normalizer[T] {
  def inputProperties : Set[Property] = Set(outputProperty)

  def outputProperty : Property

  def normalize(value : PropertyValue[T]) : PropertyOutput[T]
}

/**
  * Resolved value of a Property
  * @param property
  * @param value
  * @tparam T
  */
case class PropertyValue[T](property: Property, value: Try[T])

/**
  * A Source provides a property's value from a set of input property values.
  * @tparam T
  */
trait Source[T] {
  def inputProperties : Set[Property]
  def outputProperty : Property

  def resolve(inputs : Set[PropertyValue[_]]) : PropertyValue[T]
}

case class MissingInputException(message : String) extends RuntimeException(message)

class MissingSource[T](property : Property) extends Source[T] {
  override def inputProperties: Set[Property] = Set.empty

  override def resolve(inputs: Set[PropertyValue[_]]): PropertyValue[T] = throw new RuntimeException("Missing source for " + outputProperty)

  override def outputProperty: Property = property
}

/**
  * Value of a property assigned from the outside.
  * @param property
  * @param value
  * @tparam T
  */
case class PropertyInput[T](property: Property, value: T)

/**
  * Output ot an activity for presentation.
  * @param property
  * @param value
  * @param message
  * @tparam T
  */
case class PropertyOutput[+T](property: Property, value: Option[T], message: String)
