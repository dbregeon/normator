package com.digitalbrikes.normator

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * A Materialized Activity has an execution graph built from the Activity's definition.
  *
  * @tparam T the type of the outcome when performing the activity.
  * @tparam C the type of the context this activity can be materialized in.
  */
class MaterializedActivity[T, C](sources : Set[Source[_, C]], normalizers : Set[Normalizer[_, C]]) {
  private val materializedGraph = new MaterializedGraph(sources)
  private val normalizersMap = normalizers.map(n => (n.outputProperty, n)).toMap[Property, Normalizer[_, C]]

  /**
    * The inputs necessary for the activity as determined by analyzing the actvity's property graph.
    * @return the properties for which an input need to be provided.
    */
  def requiredInputs: Set[Property] = materializedGraph.inputs ++ normalizers.map(n => n.outputProperty).diff(sources.map(s => s.outputProperty))

  /**
    * Computes a Set of outputs from inputs to display to the user.
    *
    * @param inputs the user's inputs.
    * @return values that should be presented to the user.
    */
  def update(inputs: Set[PropertyInput[_]])(implicit context : C, executionContext: ExecutionContext) : Future[Set[PropertyOutput[_]]] = {
    def normalize[X] = (value : PropertyValue[X]) => normalizersMap.get(value.property).map(normalizer => normalizer.asInstanceOf[Normalizer[X, C]].normalize(value))

    materializedGraph.recompute(inputs).map(values => values.flatMap(value => normalize(value)))
  }

  /**
    * Performs the activity from the given inputs.
    * @param inputs the user's inputs.
    * @return the outcome of the activity or an error when the activity failed.
    */
  def perform(inputs: Set[PropertyInput[_]])(implicit context : C): Try[T] = ???
}

