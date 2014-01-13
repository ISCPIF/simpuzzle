package fr.geocite.marius

/**
 * Created by clementinecottineau on 10/01/14.
 */

import org.graphstream.graph._
import org.graphstream.graph.implementations._
import org.graphstream.ui.swingViewer.Viewer
//import au.com.bytecode.opencsv.CSVReader
//import java.io.FileReader
//import scala.collection.JavaConversions._


object TutoGraphStream extends App {
  val graph = new SingleGraph("Tutorial 1")


  graph.addNode("A")
  graph.addNode("B")
  graph.addNode("C")
  graph.addNode("D")
  graph.addNode("E")
  graph.addNode("F")
  graph.addEdge("AB", "A", "B");
  graph.addEdge("BC", "B", "C");
  graph.addEdge("CA", "C", "A");
  graph.addEdge("AD", "A", "D");
  graph.addEdge("DE", "D", "E");
  graph.addEdge("DF", "D", "F");
  graph.addEdge("EF", "E", "F");



  val A: Node = graph.getNode("A")
  val B: Node = graph.getNode("B")
  val C: Node = graph.getNode("C")
  val D: Node = graph.getNode("D")
  val E: Node = graph.getNode("E")
  val F: Node = graph.getNode("F")


  //val reader = new CSVReader(new FileReader("Desktop/graph-stream.csv"), ",", 1)
 // for (row <- reader.readAll) {
   // println(row(0))
  //}



  A.addAttribute("ui.size", "40")
  B.addAttribute("ui.size", "20")
  C.addAttribute("ui.size", "50")
  D.addAttribute("ui.size", "60")
  E.addAttribute("ui.size", "30")
  F.addAttribute("ui.size", "40")



  val allNodes = List(A, B, C, D, E, F)
  for { i <- allNodes}
  {
    i.addAttribute("ui.label", i.getId())
    i.addAttribute ("ui.class", "marked")
  }

  //A.addAttribute("ui.label", "A")
  //val B: Node = graph.getNode("B")
  //B.addAttribute("ui.label", "B")
  //A.setAttribute ("ui.class", "marked")
 // val degreeA = A.getDegree()
  //println(degreeA)

   for { i <- allNodes}
  {
    println(i.getId(), i.getDegree())
  }



  val styleSheet =
    """
    node.marked {
                 fill-color: blue  ;
                 size-mode : dyn-size ;
         }
    node:clicked {
     fill-color: red;
      }
    """

  graph.addAttribute("ui.stylesheet", styleSheet)

  val viewer = graph.display()
  viewer.setCloseFramePolicy(Viewer.CloseFramePolicy.CLOSE_VIEWER)



}
