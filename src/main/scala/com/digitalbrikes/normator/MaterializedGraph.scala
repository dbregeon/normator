package com.digitalbrikes.normator

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
  * A graph in which input properties can be updated and the outputs recomputed from their inputs.
  *
  * @tparam C the type of the context this graph can use.
  */
class MaterializedGraph[C](val nodes: Set[Source[_, C]]) {
  private type InputNodes = Map[Property, InputNode[_]]
  private type SourceNodes = Map[MaterializedNode[_], Set[Node[_]]]

  private def inputNodes(propertyNodes: Map[Property, MaterializedNode[_]]) =
    nodes.flatMap(source => source.inputProperties.filter(p => !propertyNodes.isDefinedAt(p)))

  private def inputNodesForSource(propertyNodes: Map[Property, Node[_]],
                                  inputsMap: InputNodes,
                                  source: Source[_, C]) =
    source.inputProperties.map(p => propertyNodes.get(p).orElse(inputsMap.get(p)).get)

  private def buildGraph(nodes: Set[Source[_, C]]): (InputNodes, SourceNodes) = {
    val propertyNodes =
      nodes.map(source => (source.outputProperty, MaterializedNode(source))).toMap[Property, MaterializedNode[_]]

    val inputsMap = inputNodes(propertyNodes).map(p => (p, InputNode(p))).toMap

    val sourcesMap =
      nodes.map(source => (propertyNodes(source.outputProperty), inputNodesForSource(propertyNodes, inputsMap, source)))
        .toMap[MaterializedNode[_], Set[Node[_]]]

    (inputsMap, sourcesMap)
  }

  private val (inputNodes, sourceNodes): (InputNodes, SourceNodes) = buildGraph(nodes)

  private def update(inputs: Set[PropertyInput[_]])(implicit context: C, executionContext: ExecutionContext): Unit = {
    def update[T](input: PropertyInput[T]) = inputNodes.get(input.property).asInstanceOf[Option[InputNode[T]]]
      .foreach((node: InputNode[T]) => node.update(input))

    inputs.foreach(input => update(input))
  }

  def inputs: Set[Property] = inputNodes.keySet

  def recompute(inputs: Set[PropertyInput[_]])
               (implicit context: C, execution: ExecutionContext): Future[Set[PropertyValue[_]]] = {
    update(inputs)
    sourceNodes.keys.foreach(node => node.makePromise())
    sourceNodes.keys.foreach(node => node.recompute())
    Future.sequence(promisedFutures(inputNodes.values) ++ promisedFutures(sourceNodes.keys))
  }

  private def promisedFutures(nodes: Iterable[Node[_]]): Set[Future[PropertyValue[_]]] =
    nodes.map(node => node.promisedFuture).toSet

  trait Node[T] {
    def promise: Promise[PropertyValue[T]]

    def promisedFuture: Future[PropertyValue[T]] = promise.future

    def outputProperty: Property
  }

  private def insufficientInput[T](p : Property) = {
    new PropertyValue[T](p, Failure(InsufficientInputException))
  }

  case class InputNode[T](outputProperty: Property) extends Node[T] {
    var currentOutput: PropertyValue[T] = insufficientInput(outputProperty)

    private var _promise: Promise[PropertyValue[T]] = Promise.successful(currentOutput)

    override def promise: Promise[PropertyValue[T]] = _promise

    def update(input: PropertyInput[T])(implicit context: C): Unit = {
      currentOutput = new PropertyValue[T](outputProperty, Success(input.value))
      _promise = Promise.successful(currentOutput)
    }
  }

  case class MaterializedNode[T](source: Source[T, C]) extends Node[T] {
    private var previousInputs: Set[PropertyValue[_]] = Set.empty
    private var currentOutput: PropertyValue[T] = insufficientInput(outputProperty)
    private var _promise: Promise[PropertyValue[T]] = Promise.successful(currentOutput)

    override def promise: Promise[PropertyValue[T]] = _promise

    override def outputProperty: Property = source.outputProperty

    def makePromise(): Unit = _promise = Promise[PropertyValue[T]]()

    def recompute()(implicit context: C, execution: ExecutionContext): Unit = {
      promise.completeWith(Future.sequence(promisedFutures(sourceNodes(this))).map(inputProperties => {
        if (inputProperties.diff(previousInputs).nonEmpty) {
          previousInputs = inputProperties
          currentOutput = source.resolve(inputProperties)
        }
        currentOutput
      }))
    }
  }

}

case object InsufficientInputException extends RuntimeException
