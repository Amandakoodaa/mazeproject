import Main.*
import java.nio.file.{Files, Paths}

/** Object 'Game' represents the game.
 * The object contains the current maze, solution and player.
 * There are methods for creating a maze and saving the current maze to a file.
 * */
object Game:

  /** Maze represents the maze currently in use.
   * The maze is created below by generating a new maze. */
  var maze = Maze(0,0)
  /** Solution represents the solution of them maze. The solution is generated if the user wants to quit the game
   * or has solved the maze. */
  var solution =  Array[Array[Tile]]()
  /** Player represent the character in the game as a instace of the class [[Player]]. */
  var player = Player(maze)
  
  /** Creates a maze for the game and returns the created maze.
   * The width and height of the maze are given as a parametrs x and y.*/
  def createMaze(x: Int, y:Int): Maze =
    //Creates a maze with given width and height.
    val maze = Maze(x, y)
    //Generates x * y sized maze with only walls.
    maze.generateWalls()
    // Generates paths for the maze.
    maze.generatePaths()
    // Adds neighbors for all of the tiles in the maze.
    maze.addNeighbors()
    // Generates overlapping paths for the maze.
    maze.generateOverlaps()
    // Adds an instace of class 'Player' for this maze.
    player = Player(maze)
    this.maze = maze
    maze

  def solveMaze() = solution = maze.solveMaze()

  /** Saves maze to a file where elements are represented as follows:
   *  Player : P
   *  Floor tiles: _
   *  Wall tiles: #
   *  bridge with a floor under it: F
   *  bridge with a wall under it: W
   *  end tile: E
   *  start tile: S
   *  */
  def saveToFile: Unit =

    def mazeToFile(): String =
      maze.tiles.map{row =>
          row.map{ tile =>
            if tile == player.currentTile then "P"
            else if tile == maze.endTile then "E"
            else if tile == maze.startTile then "S"
            else if maze.overlaps.exists((dir, tiles) => tiles.exists(_.pos == tile.pos && tile.isFloor)) then "F"
            else if maze.overlaps.exists((dir, tiles) => tiles.exists(_.pos == tile.pos && (tile.isOuterWall || tile.isInnerWall))) then "W"
            else if tile.isFloor then "_"
            else   "#"
      }}.map(_.mkString).mkString("\n")


    // Maps the maze so that each element is represented whit a char as shown above.
    val toSave = mazeToFile()
    //Creates and saves the string to a file named 'maze.txt'.
    Files.write(Paths.get("maze.txt"), toSave.getBytes)


end Game
