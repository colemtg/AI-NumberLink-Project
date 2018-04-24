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


(* Kenny Edits
type pos = int * int
type line = pos * pos
type sol = Map<pos, line>




let pos1 = (1,1)
let line1 = (pos1,pos1)

    
      
let sample = 
    Map.empty 
        |> Map.add pos1 line1

type item = char

type grid = { size: int; gridValues: Map<pos,item>}
with
  member b.Value (p: pos) =
    match Map.tryFind p b.gridValues with
    | None -> '!' //return ! if value does not exist for the position
    | Some v -> v

let value1 = 'A'
let values1 = 
    Map.empty
        |> Map.add pos1 value1
        |> Map.add (1,2) '0'
        |> Map.add (2,1) 'B'
        |> Map.add (2,1) '0'

let grid = {size = 2; gridValues = values1}

let ss = grid.Value (1,2)
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

let initBoard = "00r000r00"

let s1 = {size = boardSize; lines = fillMap (boardSize, initBoard); 
board = fillArray (boardSize, initBoard)}

s1.toString

<<<<<<< HEAD

//validates if an initial configuration is valid
let isValidBoard (b: string) : bool = false



//checks if a string can be converted to a n  by n char list of lists
// by checking if the length of the string is a perfect square
let validStringLength (str: string) : bool =
  double (int (sqrt (double (str.Length)))) = (sqrt (double (str.Length)))

validStringLength initBoard

//let stringToCharChar (input: string): char list list =
  //if sqrt input.Length 
=======
>>>>>>> 29b61c70a6fdde7593a62ee8831ef4970f5d9a2f
