(*
Project 1: Numberlink Puzzle
Collin Kolars
Cole Pierce
Kenny Song


TODO: Define the representation of a state of the puzzle
- How to represent the variables: (current location, goal)?
TODO: Read in a file or input of a puzzle and convert to an initial state
TODO: Validate that an initial state is valid
TODO: Validate that a solution state is a solution to an initial state
TODO: Get next state function that gets all possible next states given a state
TODO: Function that calculates the g(s) of a current state, or incorporate into the state
TODO: Figure out what we want to use as our heuristic
- Some combination of Manhattan distance and amount of next states a variables has?
TODO: Implement greedy best first search
TODO: Implement A* search
TODO: Generate random puzzles
TODO: Run tests on the random puzzles
TODO: Implement improvements and run more tests
*)

module Array2D

(*Board/State*)
type pos = int * int //(row, col)
type endOfLine = pos //the current end of a line
type goalOfLine = pos //where the line is trying to get to
type line = endOfLine * goalOfLine //line consists of the end and its goal
type lineName = char //line displayed as its char

//State
type state = {size: int; lines: Map<lineName,line>; board: char list list}
with
  member s.toString =
   for i in s.board do
     for j in i do
       printf "%c" j
       printf "%c" ' '
     printf "%c" '\n'


type initialConfiguration = int * string //type that file will be converted to

//TODO: given an initial configuration get the initial list of lists
//Currently just mimics the initBoard defined below
let fillArray (init: initialConfiguration): char list list = 
  [['0';'0';'r'];['0';'0';'0'];['r';'0';'0']] 


//TODO: given an initial configuration get the map
//Currently just mimics the initBoard defined below
let fillMap (init:initialConfiguration): Map<lineName,line> = 
  Map.empty
  |> Map.add  'r' ((1,3),(3,1)) 


//TODO: get these two values below from a file
let boardSize = 3

let initBoard = "00r\n000\nr00"

let s1 = {size = boardSize; lines = fillMap (boardSize, initBoard); 
board = fillArray (boardSize, initBoard)}

s1.toString