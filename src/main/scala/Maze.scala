import Direction.*
import scala.util.Random
import scala.collection.mutable.Stack

/**
 * Class 'Maze' represents the maze.
 */
class Maze(width: Int, height: Int):

  /** Tiles is is a list of the tiles in the maze exept the overlapping paths.
   * They are either instances of the class [[Floor]], [[InnerWall]] or [[OuterWall]]. */
  var tiles = Array.ofDim[Tile](height, width)

  /** Overlaps is a vector of overlapping paths in the maze.
   * The overlapping paths have a direction and list of tiles.*/
  var overlaps = Vector[(direction, List[Tile])]()

  /** StartTile is an instace of the class [[Floor]].
   * The player is placed there when the maze is generated is started. */
  var startTile: Tile = Floor(width / 2, height / 2)

  /** EndTile is an instace of the class [[Floor]].
   * The maze is solved when the player reaches the end tile.
   * EndTile is choosen at random from instances of class [[OuterWall]].*/
  var endTile: Tile = Floor(0,0)

  /** Generaters a maze that has instances of claa [[OuterWall]] on the edges and
   * instaces of class [[InnerWall]] on all other positions.  */
  def generateWalls() =
    for y <- 0 until this.height
        x <- 0 until this.width do
      // Checks whether the position is on the edge of the maze.
      if (x == 0 || x == this.width - 1 || y == 0 || y == this.height - 1) then
        //Adds OuterWall to that position.
        tiles(y)(x) = OuterWall(x, y)
      else
        //Adds InnerWall to all other indexes.
        tiles(y)(x) = InnerWall(x, y)

  /** Adds neighbors for the tiles in 'tiles' array.
  * Each tile can have four neighboring tiles in directions Up, Down, Left, Right (Check [[Direction]]) */
  def addNeighbors() =
    for y <- tiles.indices; x <- tiles(0).indices do
    // Goes through the neighboring positions of a tile.
    for (i, j, dir) <- Vector((1,0, Down), (0, 1, Right), (-1, 0, Up), (0, -1, Left)) do
      //Checks wheter the neigboring position is out of bounds.
      if y + i >= 0 && y + i < height && x + j >= 0 && x + j < width then
        //Adds the tile and the direction to the neighboring tile list of this tile.
        tiles(y)(x).addNeihbors(tiles(y + i)(x + j), dir)

  /** Generates the floors for the maze with 'Recursive backtracker' algorithm. */
  def generatePaths() =
    //Visited is an array where there is 'true' on positions of tiles that have been visited when creating the maze
    // and else false. Positions of outer wall are set to true since there cannot be paths added to these positions.
    var visited =  for i <- tiles yield i.map(_.isInstanceOf[OuterWall])
    // Stack contains the visited floors so that a floor can be popped from it if a dead end occures.
    val stack = Stack[Tile]()
    // Current tile is set to starting tile.
    var tile: Tile = startTile

    // While the stack is not empty and the current tile not visited creates a new paths form current tile.
    while stack.nonEmpty || !visited(tile.pos._2)(tile.pos._1) do
      val (x, y) = tile.pos
      //Sets current tile as visited.
      visited(y)(x) = true
      //Adds a floor on current tiles position.
      tiles(y)(x) = Floor(x, y)

      //Free neighbors are neighboring tiles filtered with 'canBeFloor' helper method.
      val freeNeighbors = Direction.Up.directions.filter(dir => canBeFloor(dir.neighborPos(x, y)._1, dir.neighborPos(x, y)._2, dir))
      //Those neighboring positions that cannot be floor are set as visited.
      for dir <- Direction.Up.directions do if !freeNeighbors.contains(dir) then visited(dir.neighborPos(x, y)._2)(dir.neighborPos(x, y)._1) = true

      if freeNeighbors.nonEmpty then
        // Chooses at random one of the neighbors that can be floors and have not yet been visited.
        val dir = Random.shuffle(freeNeighbors).head
        //Pushes the current tile to the stack.
        stack.push(tiles(y)(x))
        //Sets the neighboring position as the current one.
        val (newX, newY) = dir.neighborPos(x, y)
        tile = tiles(newY)(newX)

      else
        // If a floor tile cannot be added next to the current tile a tile is popped from the stack.
        tile = stack.pop()

    //Sets start and end tiles.
    startTile = tiles(startTile.pos._2)(startTile.pos._1)
    setEndTile()


    /** Helper method for checking the neighbors.
     * A floor can be added to the position (x, y) if:
     * - the position is not on the edge of the maze,
     * - there aren't any floors on the neighboring positions in clockwise or counter clockwise directions,
     *    in front of this or in clockwise or counter clockwise directions of the tile in front
     * - tile  or the position has not been visited. */
    def canBeFloor(x: Int, y: Int, dir: Direction.direction): Boolean =
      //Position next to this position.
      val (x1, y1) = dir.clockWise.neighborPos(x, y)
      val (x2, y2) = dir.counterClockWise.neighborPos(x, y)
      val (x3, y3) = dir.neighborPos(x, y)
      val (x4, y4) = dir.clockWise.neighborPos(x3, y3)
      val (x5, y5) = dir.counterClockWise.neighborPos(x3, y3)

      //Cheks whether the neighboring position are in bounds.
      List(x1, x2, x3, x4, x5).forall(x => x >= 0 && x < width) &&  List(y1, y2, y3, y4, y5).forall(y => y >= 0 && y < height) &&
      //Checks whether there are floors on the neighboring positions, also in diagonal but not in the opposite direction or its neighboring positions.
      !tiles(y1)(x1).isFloor && !tiles(y2)(x2).isFloor && !tiles(y3)(x3).isFloor && !tiles(y4)(x4).isFloor && !tiles(y5)(x5).isFloor
      //Checks whether the tile is an outer wall or has been visited.
      && !tiles(y)(x).isOuterWall && !visited(y)(x)

    /** Sets the end tile by choosing at random an instance of class 'OuterWall' from list 'tiles' that has a neighboring tile 'Floor'. */
    def setEndTile() =
      //Filters the tiles so that the array 'outerWalls' has only outer walls with neighboring floors.
      val outerWalls = tiles.flatMap(_.filter(_.isOuterWall)).filter {tile =>
        val (x,y) = tile.pos
        Direction.Up.directions.exists( dir =>
          val (x1, y1) =  dir.neighborPos(x, y)
          if x1 >= 0 && x1 < width && y1 >= 0 && y1 < height then
            tiles(y1)(x1).isFloor
          else false
          )
      }
      //Chooses one tiles at random.
      tile = Random.shuffle(outerWalls).head
      val (x, y) = tile.pos
      // Sets a floor in this position and dets the floor as endTile.
      endTile = Floor(x, y)
      tiles(y)(x) = endTile

  /** Generates overlapping paths by choosing at random a floor tile and checking whether a overlap can be generated from this tile.
   * If so, a segment of floors are generated starting from this tile until a floor tile is reached. Not necessary the first floor tile.
   * */
  def generateOverlaps() =
    // The ammount of bridges that are generated.
    var bridgeCount = (tiles(0).length + tiles.length) / (6)
    // While the ammount of bridges havent been reached a new brigde is generated.
    while bridgeCount != 0 do
      //Random tiles are chosen from the tiles as a starting and ending tile for the bridge.
      val start = tiles(Random.between(0, height))(Random.between(0, width))
      // Either the x or y of the end tile is same as start tile so that the bridge is strait.
      val end = if Random.nextBoolean() then tiles(start.pos._2)(Random.between(0, width)) else tiles(Random.between(0, height))(start.pos._1)

      //Gets the direction of the bridge.
      var dir: Direction.direction = Up
      List(Up, Down, Left, Right).foreach( d =>
            val (x,y) = d.neighborPos(start.pos._1, start.pos._2)
            val (xe, ye) = end.pos
            val (xp, yp) = start.pos
            if math.abs(xe-x) < math.abs(xe-xp) || math.abs(ye-y) < math.abs(ye-yp) then
              dir = d
      )
      //A floor tile next to the starting tile in direction 'dir' is set to 'newTile'.
      var newTile = Floor(dir.neighborPos(start.pos._1, start.pos._2)._1, dir.neighborPos(start.pos._1, start.pos._2)._2)

      // Checks whether a bridge can be created.
      if canBeBridge(newTile, dir, start, end) then
        var previous = start
          //Bridges are added to the overlaps vector.
          this.overlaps = this.overlaps :+ (dir, List())
          //While the new tiles position is not the ends position
          while newTile.pos != end.pos do
            // The position of the new tile is x and y.
            val (x, y) = newTile.pos
            //The previous tile is added in opposite direction of the bridge.
            // For the previous tile this new tile is added as a neighbor in the direction the bridge is created.
            previous.addNeihbors(newTile, dir)
            newTile.addNeihbors(previous, dir.oppositeDirection)
            //New tile is added to the list in overlaps.
            this.overlaps = this.overlaps.updated(overlaps.length-1, (dir, overlaps.last._2.appended(newTile)))
            //Previous tile is set as NewTile and a new tile is set as Floor with neighboring position.
            previous = newTile
            newTile = Floor(dir.neighborPos(x, y)._1, dir.neighborPos(x, y)._2)

          // For the end tile previous tile is set as neighbor and for previous tile end tile is set as neighbor.
          end.addNeihbors(previous, dir.oppositeDirection)
          previous.addNeihbors(end, dir)
          bridgeCount -= 1

  /** Checks whether a bridge can be added with given parametrs. Helper method that is used when creating bridges. */
  private def canBeBridge(newTile: Tile, dir: Direction.direction, start: Tile, end: Tile): Boolean =
    // Are end and start tiles floors.
    start.isFloor && end.isFloor
    //Is the length of the bridge max 1/5 of the height of the maze.
    && math.abs(start.pos._1 - end.pos._1) < tiles.length / 5
    && math.abs(start.pos._2 - end.pos._2) < tiles.length / 5
    // Are the starting and ending positions different.
    && start.pos != end.pos
    // Are there neighboring inner walls for the end and start tiles.
    && end.neighbors.exists((tile, dire) => dire == dir.oppositeDirection && tile.isInnerWall)
    // Check whether the new tile is an inner wall.
    && tiles(newTile.pos._2)(newTile.pos._1).isInnerWall
    // Check whether the start or end tiles x or y positions are same as the mazes startingTile.
    // Since a bridge cannot be created on top of the starting tile.
    && start.pos._1 != startTile.pos._1 && start.pos._2 != startTile.pos._2
    && end.pos._1 != startTile.pos._1 && end.pos._2 != startTile.pos._2


  /** Solves the maze with 'Recursive backtracker' algorithm.
   * Starting from the start tile it goes through the maze marking witch tiles has been visited.
   * The visited tiles are pushed to a stack.
   * If an itersection is reached, next tile is chosen at random from unvisited neighboring floor tiles.
   * If a dead end is reached, then a tile is popped from the stack until a tile that has an unvisited neighoring 'Floor' is found (latest intersection).  */
  def solveMaze(): Array[Array[Tile]] =
    var visited = Array[Tile]()
    val solution = Stack[Tile]()
    var tile: Tile = startTile

    // Goes through the tiles until the end tile is reached, i.e maze solved.
    while tile != endTile do
      // Adds the current tile to the visited tiles.
      visited = visited :+ tile
      // Finds the free neighbors (if any) of the current tile by checking whether if it has unvisited neighboring floor tiles.
      val freeNeighbors = tile.neighbors.filter(n => n._1.isFloor && !visited.contains(n._1))

      if freeNeighbors.nonEmpty then
        // If the current tile has free neighbors one of them is chosen at random and set a current tile.
        solution.push(tile)
        tile = Random.shuffle(freeNeighbors).head._1
      else
        // Else a tile is popped from the 'solution' stack
        tile = solution.pop()

    solution.push(endTile)
    // SolutionTiles is a 3D array representing the solution as its
    // own maze with only the solution tiles being instaces of the class 'Floor'.
    val solutionTiles = for y <- tiles yield
      y.map(tile =>
        if solution.exists(_.pos == tile.pos) then Floor(tile.pos._1, tile.pos._2)
        else
          val wall = OuterWall(tile.pos._1, tile.pos._2)
          for x <- tile.neighbors do wall.addNeihbors(x._1, x._2)
          wall
      )
    solutionTiles

end Maze
