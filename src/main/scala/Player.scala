import Direction.*
/**
 * Class 'Player' represents the player in the maze and basic functionality that the player has.
 * The player can move to a neighboring free tile, has a maze its in, a current tile and position.
 *
 */
class Player(maze: Maze):
  /** CurrentTile represents the floor tile the palyer is currently on. */
  var currentTile = maze.startTile
  
  /** CanMoveTo is a vector of directions or tiles which the player can move to. */
  def canMoveTo: List[(Tile, Direction.direction)] =
    this.currentTile.neighbors.filter(_._1.isFloor)

  /** Moves the player to the floor given as a parameter. */
  def move(dir: Direction.direction) =
    if canMoveTo.exists(_._2 == dir) then
      currentTile = currentTile.neighbors.find(_._2 == dir).get._1

end Player
