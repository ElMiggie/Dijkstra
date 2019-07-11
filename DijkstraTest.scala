package hw07.sol
import hw07.src.IDijkstra
import hw07.src.DirectedGraph
import hw07.src.IGraph
import scala.collection.mutable.HashMap
import IGraph.Vertex
import tester.Tester


class DijkstraTest()  

trait graph1 extends DijkstraTest {
  val source1 = Vertex(0)
  val node11 = Vertex(1)
  val node12 = Vertex(2)
  
  def testFSDP1(t: Tester) {   
    val g1 = new DirectedGraph
    g1.addVertex(source1)
    g1.addVertex(node11)
    g1.addEdge(source1, node11, 10)
    g1.addVertex(node12)
     
    val d1 = new Dijkstra(g1,source1)
    t.checkExpect(d1.findShortestDistance(source1), Some(0.0))
    t.checkExpect(d1.findShortestDistance(node11), Some (10.0))
    t.checkExpect(d1.findShortestDistance(node12), None)
    t.checkExpect(d1.findShortestPath(source1), Some(List(source1)))
    t.checkExpect(d1.findShortestPath(node11), Some(List(source1, node11)))
    t.checkExpect(d1.findShortestPath(node12), None)  
   }
}

trait graph2 extends DijkstraTest {
  val source2 = Vertex(0)
  val node21 = Vertex(1)
  val node22 = Vertex(2)
  val node23 = Vertex(3)
  
  def testFSDP2(t: Tester) {
    val g2 = new DirectedGraph
    g2.addVertex(source2)
    g2.addVertex(node21)
    g2.addVertex(node22)
    g2.addVertex(node23)
    g2.addEdge(source2, node23, 200)
    g2.addEdge(source2, node21, 10)
    g2.addEdge(node21, node22, 10)
    g2.addEdge(node22, node23, 10)
    
    val d2 = new Dijkstra(g2,source2)
    t.checkExpect(d2.findShortestDistance(node23), Some(30.0))
    t.checkExpect(d2.findShortestPath(node23), Some(List(source2, node21, node22, node23)))
  }
}

trait graph3 extends DijkstraTest {
  
  val source3 = Vertex(0)
  val node31 = Vertex(1)
  val node32 = Vertex(2)
  val node33 = Vertex(3)
  def testFSDP3(t: Tester) {
    val g3 = new DirectedGraph
    g3.addVertex(source3)
    g3.addVertex(node31)
    g3.addVertex(node32)
    g3.addEdge(source3, node32, 1)
    g3.addEdge(source3, node31, 2)
    g3.addEdge(node31, node32, 2)
    
    val d3 = new Dijkstra(g3,source3)
    t.checkExpect(d3.findShortestDistance(node32), Some(1.0))
    t.checkExpect(d3.findShortestPath(node32), Some(List(source3,node32)))
   }
 }

trait graph4 extends DijkstraTest {
  
  val source4 = Vertex(0)
  val node41 = Vertex(1)
  val node42 = Vertex(2)
  val node43 = Vertex(3)
  val node44 = Vertex(4)
  
  def testFSDP4(t: Tester) {
    val g4 = new DirectedGraph
    g4.addVertex(source4)
    g4.addVertex(node41)
    g4.addVertex(node42)
    g4.addVertex(node43)
    g4.addVertex(node44)
    g4.addEdge(source4, node41, 0)
    g4.addEdge(source4, node42, 0)
    g4.addEdge(source4, node43, 0)
    g4.addEdge(node41, node44, 3)
    g4.addEdge(node42, node44, 2)
    g4.addEdge(node43, node44, 1)
    
    val d4 = new Dijkstra(g4, source4)
    t.checkExpect(d4.findShortestDistance(node44),Some(1.0))
    t.checkExpect(d4.findShortestPath(node44), 
        Some(List(source4, node43, node44)))
  }
}
 
 object DijkstraTest extends App {
  Tester.run(new DijkstraTest()
  with graph1
  with graph2
  with graph3
  with graph4)
 }
 
