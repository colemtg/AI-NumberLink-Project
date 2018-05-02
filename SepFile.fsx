//Completed:
// Take the input type of int * string and convert to n by n char list
// Take the input type of int * string and convert to a map where the key
//   is the char and the value is Type line where line has a char name,
//   end position of the line, goal position of the line, and length of line 
// not sure if we need length, but will make calculating cost of state easier I think
// wasn't sure if should be initially 0 or 1 to begin with but shouldn't matter 
//   as long as its consistant
// Board validation
// Checking if a state is a goal state
// g(s) calculation for board
// h(s) calculation for board

//TODO: integrate file reading and randomly generated boards
//TODO: GetNextStates function for BoardState
//TODO: do A* and greedy best

type Pos = int * int

//represents the different lines
type Line = {name: char; endPos: Pos; goalPos: Pos; length: int}
with
  //updates the postion of the end of the line, adds one to length
  member l.UpdateEndPos (p: Pos) = {name = l.name; endPos = p; 
  goalPos = l.goalPos; length = l.length+1}

  //manhatan distance of each line
  member l.GetDistance = 
    match (l.endPos, l.goalPos) with
    (x1,y1),(x2,y2) -> abs (x1-x2) + abs (y1-y2)

  member l.AtGoal = l.endPos = l.goalPos


//update this with functions for getting next states, getting cost and heuristic
type BoardState = {size: int; lines: Map<char,Line>; board: char list list}
with

  //g(s) = sum of length of all lines in state s
  member b.GetCost =
    Map.fold (fun state _ value -> state + value.length) 0 b.lines

  //h(s) = manhattan distance
  member b.GetHeuristic = 
    Map.fold (fun state _ (value:Line) -> state + value.GetDistance) 0 b.lines
  
  member b.AtGoal =
    Map.fold(fun state _ (value:Line) -> state && value.AtGoal) true b.lines

    

//What the function that reads in puzzle from a file should return
type FileInput = int * string


//takes in the file representation and converts to n by n char list
//invalid check should be move elsewhere
let rec convertToCharListList (f: FileInput) : char list list =
  match f with
  (len, str) -> split len (Array.toList (str.ToCharArray())) 
and split (n:int)(c: char list): char list list =
  match c with
  [] -> []
  | _ -> firstN n c :: split n (removeN n c)
and firstN (n: int)(c: char list): char list =
  match c with
  (x::_) when n=1 -> [x]
  |(x::xs) -> x :: firstN (n-1) xs
  | _ -> []
and removeN (n: int)(c: char list): char list =
  match c with
  (_::xs) when n=1 -> xs
  |(_::xs) -> removeN (n-1) xs
  | _ -> []


//takes in the file input and outputs the map representation
//should move the invalid check seperately
let rec convertToMap (f: FileInput) : Map<char, Line> =
  match f with
    (len, str)  -> addToMap len (Array.toList (str.ToCharArray()))
and addToMap (n: int)(c: char list): Map<char, Line> =
  let mutable m = Map.empty<char,Line>
  for i = 0 to c.Length-1 do
    if c.[i] <> '0' && m.ContainsKey (c.[i]) then
     let tempLine = (Map.find c.[i] m)
     m <- m.Remove(c.[i])
     m <- m.Add(c.[i], {name = tempLine.name; endPos = tempLine.endPos; goalPos = (i/n, i%n); length = 1})
    else if c.[i] <> '0' then m <- m.Add(c.[i], {name = c.[i]; endPos = (i/n, i%n); goalPos = (-1,-1); length = 1})
  m


//checks that a board is valid:
// - sqrt string.size = length
// - there are exactly 2 of each none '0' char
let rec checkValidBoard(f: FileInput): bool =
  match f with
  (_, str) -> checkValidInputSize f && checkTwoOfEach (List.sort (Array.toList (str.ToCharArray())))
and checkTwoOfEach(c: char list) : bool = 
  match c with
  (x :: y :: xs) when x <>'0' -> x = y && checkTwoOfEach xs
  | (x :: y :: xs) when x ='0' -> checkTwoOfEach (y::xs)
  | ([_]) -> false
  | _ -> true
and checkValidInputSize (f:FileInput): bool = 
  match f with
  (len, str) when sqrt (double str.Length) = (double len) -> true
  | _ -> false 
  

let constructInitialBoard (f:FileInput): BoardState =
  match f with
  (len, _) when checkValidBoard f -> {size = len; lines = convertToMap f; board = convertToCharListList f}
  | _  -> failwith "invalidBoard"

//test board
let testInput = (4, "98980660zab0ab0z")
let initialBoard = constructInitialBoard testInput
initialBoard.GetCost
initialBoard.GetHeuristic
initialBoard.AtGoal
