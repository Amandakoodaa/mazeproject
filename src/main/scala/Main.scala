import scalafx.application.JFXApp3
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ButtonBar, Separator, TextField}
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.canvas.Canvas
import Game.*
import Direction.*
import scalafx.scene.input.*
import scalafx.Includes.*



object Main extends  JFXApp3:

  val mazeTitle = "Fun Maze"
  // The current maze.
  var size = 40
  var maze = Game.createMaze(size, size)
  var quit = false
  // Sizes of the elements.
  val width1 = 650
  val height1 = 650
  val stageWidth = width1 + 30
  val stageHeigth = height1 + 70
  val maxTiles = 200
  val minTiles = 30
  var tiles = maze.tiles
  var tileSize = width1 / maze.tiles(0).length
  var lineWidth = tileSize / 10.0

  // Colors of the elements.
  val floorColor = Color.White
  val wallColor = Color.Black
  val startColor = Color.Red
  val characterColor = Color.DeepPink
  val lightCharacter = Color.Pink

  def start() =

    stage = new JFXApp3.PrimaryStage:
      title = mazeTitle
      width = stageWidth
      height = stageHeigth

    val game = new Canvas(width1, height1)

    val g = game.graphicsContext2D
    // Creates a background for the maze.
    g.setFill(wallColor)
    g.fillRect(0, 0, width1, height1)
    //Draws the maze with the character and overlapping paths.
    drawMaze(tiles)
    maze.overlaps.foreach((dir, bridge) => for tile <- bridge do drawOverlaps(tile, dir))
    drawCharacter()


    // Method for drawing the tiles in the maze.
    def drawMaze(tiles: Array[Array[Tile]]) =
      tiles.flatten.foreach(tile => {
        if tile.pos == maze.startTile.pos || tile.pos == maze.endTile.pos then g.setFill(startColor)
        else if tile.isInstanceOf[Floor] then g.setFill(floorColor)
        else g.setFill(wallColor)
        g.fillRect(tile.pos._1 * tileSize, tile.pos._2 * tileSize, tile.pos._1+tileSize, tile.pos._2+tileSize)
      })

    // Method for drawing the character.
    def drawCharacter() =
      val tile = Game.player.currentTile
      // If the character is under a bridge then the color of the character is lighter.
      if maze.overlaps.exists((dir, tiles) => tiles.exists(_.pos == player.currentTile.pos)) && !maze.overlaps.exists((dir, tiles) => tiles.contains(player.currentTile)) then
        g.setFill(lightCharacter)
      else g.setFill(characterColor)

      val size = (tileSize / 4)

      g.fillOval(tile.pos._1 * tileSize + size, tile.pos._2 * tileSize + size, tileSize- (size * 2), tileSize-(size * 2))

    // Method for drawing tiles.
    def drawTile(tile: Tile) =
      if tile.pos == maze.startTile.pos || tile.pos == maze.endTile.pos then g.setFill(startColor)
      else g.setFill(floorColor)
      g.fillRect(tile.pos._1 * tileSize, tile.pos._2 * tileSize, tileSize, tileSize)

    // Method for drawing overlapping paths.
    def drawOverlaps(tile: Tile, dir: Direction.direction) =
          g.setFill(floorColor)
          g.setStroke(wallColor)
          g.setLineWidth(lineWidth)
          // The direction of the bridge determins what kind of rectangle is drawn.
          if dir == Direction.Up || dir == Direction.Down then
            g.fillRect(tile.pos._1 * tileSize + (tileSize / 6), tile.pos._2 * tileSize, tileSize - (tileSize / 3), tileSize)
            g.strokeLine(tile.pos._1 * tileSize + (tileSize / 6), tile.pos._2 * tileSize, tile.pos._1 * tileSize + (tileSize / 6), tile.pos._2 * tileSize + tileSize)
            g.strokeLine(tile.pos._1 * tileSize + (tileSize * 5/6), tile.pos._2 * tileSize, tile.pos._1 * tileSize + (tileSize * 5/6), tile.pos._2 * tileSize + tileSize)
          else
            g.fillRect(tile.pos._1 * tileSize, tile.pos._2 * tileSize + (tileSize / 6), tileSize, tileSize - (tileSize / 3))
            g.strokeLine(tile.pos._1 * tileSize, tile.pos._2 * tileSize+ (tileSize / 6), tile.pos._1 * tileSize + tileSize, tile.pos._2 * tileSize+ (tileSize / 6))
            g.strokeLine(tile.pos._1 * tileSize, tile.pos._2 * tileSize + (tileSize * 5/6), tile.pos._1 * tileSize + tileSize, tile.pos._2 * tileSize + (tileSize * 5/6))

    // Button for ending the game. When this is pressed the solution is generated and shown.
    val quitButton = new Button:
      text = "End Game"
      onAction = (event) =>
        quit = true
        Game.solveMaze()
        drawMaze(Game.solution)
        drawCharacter()

    // Button for saving the maze to a file.
    val saveToFile = new Button:
      text = "Save to file"
      onAction = (event) =>
        Game.saveToFile

    // Button for changing the size of the maze.
    // When this is pressed a new maze is generated with the current size.
    val changeSizeButton = new Button:
      text = "Create maze"
      onAction = (event) =>
        if size >= minTiles && size <= maxTiles then
          maze = Game.createMaze(size, size)
          g.setFill(floorColor)
          g.fillRect(0, 0, width1, height1)
          tileSize = width1 / maze.tiles(0).length
          lineWidth = tileSize / 10.0
          drawMaze(maze.tiles)
          maze.overlaps.foreach((dir, bridge) => for tile <- bridge do drawOverlaps(tile, dir))
          drawCharacter()

    // Text field for giving a maze size.
    val askMazeSize = new TextField:
      promptText = "Enter maze size (" + minTiles + "-" + maxTiles + ")"
      text.onChange{
        val newSize = text.value.toIntOption.getOrElse(-1)
        size = newSize
      }


    // Menu bar for buttons.
    val menuBar = new ButtonBar:
      buttons = Array(askMazeSize, changeSizeButton, quitButton, saveToFile)

    // HBox for the game.
    val view = new HBox:
      children = Array(Separator(Orientation.Horizontal), game)

    // Adding menu bar and HBox to the root.
    val root = BorderPane(view, menuBar, null, null, null)

    val scene = new Scene(parent = root)
    stage.scene = scene

    root.onMouseClicked = (_: MouseEvent) => root.requestFocus()
    root.requestFocus()

    root.onKeyPressed = (e: KeyEvent) =>
      //A tile is drawn on the current position of the character.
      if maze.overlaps.exists((dir, tiles) => tiles.contains(player.currentTile)) then
        drawOverlaps(player.currentTile, maze.overlaps.find((dir, tiles) => tiles.contains(player.currentTile)).get._1)
      else if maze.overlaps.exists((dir, tiles) => tiles.exists(_.pos == player.currentTile.pos)) then
        drawTile(player.currentTile)
        drawOverlaps(player.currentTile, maze.overlaps.find((dir, tiles) => tiles.exists(_.pos == player.currentTile.pos)).get._1)
      else drawTile(player.currentTile)

      // If the player is on the ending tile then it doesnt move.
      if player.currentTile == maze.endTile || quit then
        Game.solveMaze()
        drawMaze(Game.solution)
        drawCharacter()

      // Else moves the character to the direction of the arrow key the user has pressed.
      else
        e.code match
          case KeyCode.Up =>
            Game.player.move(Direction.Up)
          case KeyCode.Down =>
             Game.player.move(Direction.Down)
          case KeyCode.Left =>
            Game.player.move(Direction.Left)
          case KeyCode.Right =>
            Game.player.move(Direction.Right)
          case _ =>

        // Draws the character on the new current tile.
        drawCharacter()


  end start


end Main


