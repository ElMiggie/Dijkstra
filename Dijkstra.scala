package hw07.sol
import hw07.src.IDijkstra
import hw07.src.DirectedGraph
import hw07.src.IGraph
import scala.collection.mutable.HashMap
import IGraph.Vertex
import scala.collection.mutable.PriorityQueue


class Dijkstra(graph: DirectedGraph, source: Vertex) extends IDijkstra {
  
  private val distances = new HashMap[Vertex, Double]()
  dijkstra()
/**
* Performs Djikstra's algorithm from the given source Vertex, filling the
* distances HashMap with distance from the source.
*/
  def dijkstra() {
    val ordering = Ordering[Double].on[(Vertex, Double)](-1 * _._2)
    var pq = PriorityQueue[(Vertex, Double)]()(ordering)
   for(place <- graph.vertices) {
     if(place.equals(source)) { 
       distances(place) = 0.0
     } else {
       distances(place) = Double.PositiveInfinity
       }
     pq.enqueue((place, distances(place)))
     }
    while(!pq.isEmpty) {
      var v = pq.dequeue()
      for(w <- (v._1).getNeighbors) {
        val newEstimate = distances(v._1) + w.weight
        if(newEstimate < distances(w.target)) {
          distances(w.target) = newEstimate
          var store = List[(Vertex, Double)]()
          for(tuple <- pq) {
            if (tuple._1 == w.target) {
              store = (w.target, newEstimate) :: store
            } else {
              store = tuple :: store
            }
          }
          pq.dequeueAll
          for(x <- store) {
            pq.enqueue(x)
          }
          w.target.parent = Some(v._1)
          } 
        }  
      }
    }
  
  /**
   * Method that finds shortest distance from from a source to a destination
   * @param destination- Vertex that we want to reach
   * @result- option Some(distance) or None if there is no way to destination
   */
  override def findShortestDistance(destination: Vertex): Option[Double] = {
// FINISH FILLING IN HERE
    val dist = distances.get(destination)
    if (!dist.equals(None) && dist.get.equals(Double.PositiveInfinity)) { 
      None
    }else {
      dist
      }
    }
  
  /**
   * Method that finds shortest path from source to a destination
   * @param destination- Vertex that we want to reach
   * @result- option Some(list of vertices that lead to destination) 
   * 				 or None if there is no path to destination
   */
  override def findShortestPath(destination: Vertex): Option[List[Vertex]] = {
// FINISH FILLING IN HERE
    if (destination.equals(source)) {
      return Some (List(source))
    }else {
      var path = List[Vertex](destination)
      var parent = destination.parent
      while(!parent.equals(None)) {
        path = parent.get :: path
        if (parent.get.equals(source)) {
          return Some(path)
        }
        parent = parent.get.parent
      }
      return None
      }
    }
  }