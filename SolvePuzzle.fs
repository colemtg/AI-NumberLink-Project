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


// let rec validateGrid g =  
//     match with 
//     | 