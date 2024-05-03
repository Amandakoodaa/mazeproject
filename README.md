Maze Project

The program creates and draws a maze on the screen. 
The maze has a starting point in the middle of the maze and an exit point in the outer wall. 
Arrow keys are used to control the character moving through the maze. 

The user has the option to quit the game, in which case the solution is shown. 
The maze has overlapping paths. The user has the option to  save the maze to a file. 
The maze has a graphical interface and the user is free to choose the size of the maze. 


User interface

When running the program on the UI a maze and a menu bar is shown. 
As a default a maze with size 40x40 is created. 

On the menu bar there is a text field and three buttons: “Create Maze”, “End Game”, “Save to File”.
In the text field user can give a new size for the maze as an integer between 30 and 200, 
i.e. with integer 30 a new maze with 30x30 tiles is created. 

When pressing the “Create Maze” button a new maze is created with the size given in the text field. 

When pressing the “End Game” button, the solution of the current maze is shown. 
If the maze has several solutions, pressing the button again or any key will show another solution. Only one solution is shown at a time.

When pressing the “Save to File” button, the current maze is saved to a file named “maze.txt”. 
The user can move the character through the maze with arrow keys. 

When the ending tile is reached i.e. the maze is solved, the solution is shown by pressing an arrow key.
If the maze has several solutions, a different solution is shown when any key is pressed again. 
When the character is on a tile where there is an overlapping path on top of it, its color is lighter. 
