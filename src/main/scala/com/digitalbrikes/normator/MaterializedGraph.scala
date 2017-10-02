package com.digitalbrikes.normator

import scala.util.{Failure, Success}

/**
  * A graph in which input properties can be updated and the outputs recomputed from their inputs.
  */
class MaterializedGraph(val nodes : Set[Source[_]]) {

  private def buildGraph(nodes: Set[Source[_]]): (Map[Property,InputNode[_]],Map[MaterializedNode[_], Set[Node[_]]]) = {
    val propertyNodes = nodes.map(source => (source.outputProperty, MaterializedNode(source))).toMap[Property, MaterializedNode[_]]
    val inputsMap = nodes.flatMap(source => source.inputProperties.filter(p => !propertyNodes.isDefinedAt(p))).map(p => (p, InputNode(p))).toMap
    val sourcesMap = nodes.map(source => (propertyNodes(source.outputProperty), source.inputProperties.map(p => propertyNodes.get(p).orElse(inputsMap.get(p)).get).toSet[Node[_]])).toMap[MaterializedNode[_], Set[Node[_]]]

    (inputsMap, sourcesMap)
  }

  private val (inputNodes, sourceNodes) : (Map[Property, InputNode[_]], Map[MaterializedNode[_], Set[Node[_]]]) = buildGraph(nodes)

  private def update(inputs: Set[PropertyInput[_]]): Unit = {
    def update[T] = (input : PropertyInput[T]) => inputNodes.get(input.property).asInstanceOf[Option[InputNode[T]]].foreach((node : InputNode[T]) => node.update(input))
    inputs.foreach(input => update(input))
  }

  def inputs: Set[Property] = inputNodes.keySet

  def recompute(inputs: Set[PropertyInput[_]]): Set[PropertyValue[_]] = {
    update(inputs)
    inputNodes.values.map(node => node.recompute()).toSet ++ sourceNodes.keys.map(node => node.recompute()).toSet
  }

  trait Node[T] {
    def recompute() : PropertyValue[T]
  }

  case class InputNode[T](property : Property) extends Node[T] {
    var currentOutput : PropertyValue[T] = new PropertyValue[T](property, Failure(InsufficientInputException))


    def update(input : PropertyInput[T]): Unit = currentOutput = new PropertyValue[T](property, Success(input.value))

    override def recompute(): PropertyValue[T] = currentOutput
  }

  case class MaterializedNode[T](source : Source[T]) extends Node[T] {
    var previousInputs : Set[PropertyValue[_]] = Set.empty
    var currentOutput : PropertyValue[T] = new PropertyValue[T](source.outputProperty, Failure(InsufficientInputException))

    def recompute() : PropertyValue[T] = {
      val inputProperties = sourceNodes(this).map(node => node.recompute())
      if (inputProperties.diff(previousInputs).nonEmpty) {
        previousInputs = inputProperties
        currentOutput = source.resolve(inputProperties)
      }
      currentOutput
    }
  }

}

case object InsufficientInputException extends RuntimeException
