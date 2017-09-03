package com.digitalbrikes.normator

import scala.util.{Failure, Success, Try}

/**
  * A graph in which input properties can be updated and the outputs recomputed from their inputs.
  */
class MaterializedGraph {
  val inputNodes : Set[InputNode[_]] = Set.empty
  val inputs : Map[MaterializedNode[_], Set[Node[_]]] = Map.empty

  def update(inputs: Set[PropertyInput[_]]): Unit = {
    val inputProperties = inputs.foldLeft(Map.empty[Property, PropertyInput[_]])(
      (map, input) => map + ((input.property, input))
    )
    inputNodes.foreach(node => inputProperties.get(node.property).foreach(input => node.update(input.asInstanceOf)))
  }

  def recompute(outputs: Set[Property]): Set[PropertyValue[_]] = {
    inputs.keys.map(node => node.recompute()).filter(value => outputs.contains(value.property)).toSet
  }

  trait Node[T] {
    def recompute() : PropertyValue[T]
  }

  case class InputNode[T](property : Property) extends Node[T] {
    var currentOutput : PropertyValue[T] = new PropertyValue[T](property, Failure(InsufficientInputException()))


    def update(input : PropertyInput[T]) = currentOutput = new PropertyValue[T](property, Success(input.value))

    override def recompute(): PropertyValue[T] = currentOutput
  }

  case class MaterializedNode[T](source : Source[T]) extends Node[T] {
    var previousInputs : Set[PropertyValue[_]] = Set.empty
    var currentOutput : PropertyValue[T] = new PropertyValue[T](source.outputProperty, Failure(InsufficientInputException()))

    def recompute() : PropertyValue[T] = {
      val inputProperties = inputs(this).map(node => node.recompute())
      if (previousInputs.diff(inputProperties.asInstanceOf).nonEmpty) {
        previousInputs = inputProperties.asInstanceOf
        currentOutput = source.resolve(inputProperties.asInstanceOf)
      }
      currentOutput
    }
  }

}

case class InsufficientInputException() extends Exception
