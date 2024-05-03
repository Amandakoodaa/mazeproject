/** Object 'Direction' has objects Up, Down, Left, Right and helpper methods.*/
object Direction:
  trait direction:
    /** Method 'neighborPos' returns a neighboring position in the direction for the given x and y. */
    def neighborPos(x: Int, y: Int): (Int, Int)
    /** OppositeDirection returns the opposite direction for this direction. */
    def oppositeDirection: direction
    /** Clockwise returns the clockwise direction. */
    def clockWise: direction
    /** CounterClockWise returns the counter clockwise direction. */
    def counterClockWise: direction
    /** Directions is a list of all of the directions. */
    val directions = List(Up, Down, Left, Right)

  case object Up extends direction:
    def neighborPos(x: Int, y: Int) = (x, y-1)
    def oppositeDirection = Down
    def clockWise = Right
    def counterClockWise = Left

  case object Down extends direction:
    def neighborPos(x: Int, y: Int) = (x, y+1)
    def oppositeDirection = Up
    def clockWise = Left
    def counterClockWise = Right

  case object Left extends direction:
    def neighborPos(x: Int, y: Int) = (x-1, y)
    def oppositeDirection = Right
    def clockWise = Up
    def counterClockWise = Down

  case object Right extends direction:
    def neighborPos(x: Int, y: Int) = (x+1, y)
    def oppositeDirection = Left
    def clockWise = Down
    def counterClockWise = Up