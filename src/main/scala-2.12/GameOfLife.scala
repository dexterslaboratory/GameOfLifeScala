/**
  * Created by ssoni on 11/26/16.
  */
object GameOfLife extends App {

  case class Coordinates(x: Int, y: Int)

  def aliveOnGrid(): Set[Coordinates] = {
    //val living = Set(Coordinates(1, 1), Coordinates(1, 2), Coordinates(2, 1),Coordinates(2, 2)) //block pattern
    //val living = Set(Coordinates(1, 1), Coordinates(1, 0), Coordinates(1, 2)) //blinker pattern
    //val living = Set(Coordinates(0, 1), Coordinates(1, 0), Coordinates(2, 1),Coordinates(0, 2),Coordinates(1, 2)) //boat pattern
     val living = Set(Coordinates(1, 1) ,Coordinates(1, 2),Coordinates(1, 3) ,Coordinates(2, 2) ,Coordinates(2, 3) ,Coordinates(2, 4)) //toad pattern
    living
  }

  def mask:List[Coordinates]= {
    val eightNeighbourMask = List(
      Coordinates(-1, -1),
      Coordinates(-1, 1),
      Coordinates(-1, 0),
      Coordinates(1, 1),
      Coordinates(1, -1),
      Coordinates(1, 0),
      Coordinates(0, 1),
      Coordinates(0, -1))
    eightNeighbourMask
  }

  def findNeighbouringDeadCells(aliveOnGrid: Set[Coordinates], eightNeighbourMask: List[Coordinates]):  Set[Coordinates] = {
    val allNeighboursOfLiving = aliveOnGrid.flatMap(
    living => eightNeighbourMask.map(mask => Coordinates(living.x + mask.x, living.y + mask.y)))
    val neighbouringDeadCells = allNeighboursOfLiving.diff(aliveOnGrid)
    neighbouringDeadCells
  }

  def cellAndLivingMembers(cells: Set[Coordinates], eightNeighbourMask:List[Coordinates], livingCells:Set[Coordinates]): List[(Coordinates, Int)] = {
    val cellsAndNoOfLivingNeighbours = cells.flatMap(
    cellInConsideration => Map(cellInConsideration -> eightNeighbourMask
      .map(mask => Coordinates(cellInConsideration.x + mask.x, cellInConsideration.y + mask.y))
      .intersect(livingCells.toList).length))
    cellsAndNoOfLivingNeighbours.toList
  }

  def nextTickSurvivours(livingCellsNeighbourCount: List[ (Coordinates, Int)  ]) = {
    livingCellsNeighbourCount.foreach(x => if(x._2 == 2 || x._2 == 3)println(x._1) )
  }

  def nextTickResurrection(deadCellsNeighbourCount: List[(Coordinates, Int)]) = {
   deadCellsNeighbourCount.foreach(x => if(x._2 == 3 )println(x._1) )
  }

  println(nextTickSurvivours(cellAndLivingMembers(aliveOnGrid,mask,aliveOnGrid)))
  println(nextTickResurrection(cellAndLivingMembers(findNeighbouringDeadCells(aliveOnGrid(),mask),mask,aliveOnGrid)))

}
