/**
  * Created by ssoni on 11/26/16.
  */
object GameOfLife extends App {

  case class Coordinates(x: Int, y: Int)

  case class Neighbours(coordinates: Coordinates, neighbours: List[Coordinates])

  def grid(): List[Coordinates] = {
    val living = List(Coordinates(1, 1), Coordinates(9, 8),Coordinates(0,0))
    living
  }

  def countLiveNeighbours(grid: List[Coordinates]) = {
    val eightNeighbourMask = List(
      Coordinates(-1, -1),
      Coordinates(-1, 1),
      Coordinates(-1, 0),
      Coordinates(1, 1),
      Coordinates(1, -1),
      Coordinates(1, 0),
      Coordinates(0, 1),
      Coordinates(0, -1))
    val coordinateAndNeighbours = grid.map(
      living => Map(living -> eightNeighbourMask
        .map(mask => Coordinates(living.x + mask.x, living.y + mask.y))
        .intersect(grid).length ))
    coordinateAndNeighbours
  }
}
