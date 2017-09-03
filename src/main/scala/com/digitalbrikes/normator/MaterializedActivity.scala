package com.digitalbrikes.normator

import scala.util.{Failure, Success, Try}

/**
  * A Materialized Activity has an execution graph built from the Activity's definition.
  *
  * @tparam T the type of the outcome when performing the activity.
  */
class MaterializedActivity[T](graph : ResolutionGraph, normalizers : Set[Normalizer[_]]) {
  /**
    * The inputs necessary for the activity as determined by analyzing the actvity's property graph.
    * @return the properties for which an input need to be provided.
    */
  def requiredInputs: Set[Property] = graph.inputs ++ normalizers.map(n => n.outputProperty).diff(graph.outputs)

  // TODO Should build an executable graph that resolves the normalized properties from a new set of inputs
  val materializedGraph : MaterializedGraph = new MaterializedGraph

  /**
    * Computes a Set of outputs from inputs to display to the user.
    *
    * @param inputs the user's inputs.
    * @return values that should be presented to the user.
    */
  def update(inputs: Set[PropertyInput[_]]) : Set[PropertyOutput[_]] = {
    materializedGraph.update(inputs)
    materializedGraph.recompute(graph.outputs).map(value => value.value match {
      case Success(v) => PropertyOutput(value.property, Some(v), "Valid Value")
      case Failure(t) => PropertyOutput(value.property, None, t.getLocalizedMessage)
    })
  }

  /**
    * Performs the activity from the given inputs.
    * @param inputs the user's inputs.
    * @return the outcome of the activity or an error when the activity failed.
    */
  def perform(inputs: Set[PropertyInput[_]]): Try[T] = ???
}

