import Direction.*
/** The abstract class 'Tile' represents tiles in the maze.
 * There are tree concrete kinds of tiles: 'Floor', 'InnerWall' and 'OuterWall'.
 * The position of the tile is a tuple.
  * The first element of the tuple is the x location and the second element is the y location of the tile.
 * @see [[Floor]]
 * @see [[WallTile]] */
abstract class Tile(position: (Int, Int)):

  /** List of the neighboring tiles as a tuple (Tile, Direction).*/
  var neighbors: List[(Tile, Direction.direction)]

  /** Position of this tile. */
  val pos: (Int, Int)

  /** returns 'true' if this tile is a floor tile */
  def isFloor: Boolean
  
  /** Adds a neighbor for this tile in the given direction and 
   * deletes a tile from neighbors if there already is a tile in that direction.*/
  def addNeihbors(add: Tile, dir: Direction.direction) =
    neighbors = neighbors.filter((tile, direction) => direction != dir)
    neighbors = neighbors :+ (add, dir)
  /** returns 'true' if this tile is a OuterWall tile */
  def isOuterWall: Boolean
  /** returns 'true' if this tile is a InnerWall tile */
  def isInnerWall: Boolean

end Tile


/** The class 'Floor' represents floor tiles that are the tiles the palyer can move to. */
class Floor(position: (Int, Int)) extends Tile(position):
  var neighbors = List[(Tile, Direction.direction)]()
  def isFloor = true
  def isOuterWall = false
  def isInnerWall = false
  val pos = position
end Floor


/** The class 'InnerWall' represents inner wall tiles. */
class InnerWall(position: (Int, Int)) extends Tile(position):
  var neighbors = List[(Tile, direction)]()
  def isFloor = false
  def isOuterWall = false
  def isInnerWall = true
  val pos = position
end InnerWall


class OuterWall(position: (Int, Int)) extends Tile(position):
  var neighbors = List[(Tile, Direction.direction)]()
  def isFloor = false
  def isOuterWall = true
  def isInnerWall = false
  val pos = position
end OuterWall


