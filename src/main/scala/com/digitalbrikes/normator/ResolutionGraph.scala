package com.digitalbrikes.normator

class ResolutionGraph(val nodes : Set[Source[_]]) {
  val arcs : Map[(Source[_], Property), Source[_]] = {
    val propertyMap : Map[Property, Source[_]] = nodes.map(node => (node.outputProperty, node)).toMap

    nodes.foldLeft(Map.empty[(Source[_], Property), Source[_]])((fullMap, node) => {
      node.inputProperties.foldLeft(fullMap)((map, p) => {
        propertyMap.get(p).orElse(Some(new MissingSource(p))).foldLeft(map)((m, endNode) => m + (((node, p), endNode)))
      })
    })
  }

  val inputs : Set[Property] = nodes.diff(arcs.keySet.map(k => k._1)).flatMap(n => n.inputProperties)

  val outputs : Set[Property] = nodes.map(n => n.outputProperty)

  def outputNodes(output: Set[Property]): Set[Source[_]] = nodes.filter(node => output.contains(node.outputProperty))
}
