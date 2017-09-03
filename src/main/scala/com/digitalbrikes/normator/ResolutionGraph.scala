package com.digitalbrikes.normator
/**
  * Represents a directed graph of property source nodes connected by a property (input of the origin node and output of
  * the destination node.
  * @param nodes the sources contained in the graph.
  */
class ResolutionGraph(val nodes : Set[Source[_]]) {
  /**
    * The graph's arcs as a map of (Origin Node, Property) -> Destination Node
    */
  val arcs : Map[(Source[_], Property), Source[_]] = {
    val propertyMap : Map[Property, Source[_]] = nodes.map(node => (node.outputProperty, node)).toMap

    nodes.foldLeft(Map.empty[(Source[_], Property), Source[_]])((fullMap, node) => {
      node.inputProperties.foldLeft(fullMap)((map, p) => {
        propertyMap.get(p).orElse(Some(new MissingSource(p))).foldLeft(map)((m, endNode) => m + (((node, p), endNode)))
      })
    })
  }

  /**
    * The properties for which no Source exists in the graph.
    */
  val inputs : Set[Property] = nodes.diff(arcs.keySet.map(k => k._1)).flatMap(n => n.inputProperties)

  /**
    * All the properties that can be output by the graph.
    */
  val outputs : Set[Property] = nodes.map(n => n.outputProperty)

  /**
    * Collects the nodes required to product a given output.
    * @param output the desired output properties.
    * @return the nodes that will product values to build the output.
    */
  def outputNodes(output: Set[Property]): Set[Source[_]] = nodes.filter(node => output.contains(node.outputProperty))
}
