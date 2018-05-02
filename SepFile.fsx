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
// get next states for a board

//TODO: integrate file reading and randomly generated boards
//TODO: do A* and greedy best
//module PriorityQueue
open System
open Microsoft.FSharp.Core

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

  //checks if AtGoal
  member l.AtGoal = l.endPos = l.goalPos


//sets the Pos in the board to the char
let rec replacePos (c: char)(p: Pos)(old: char list list): char list list =
  match p with
  (row,col) -> replaceHelper c old row col

and replaceHelper (c:char)(old: char list list)(row: int)(col: int): char list list =
  match old with
  (x :: xs) when row = 0 -> inRow c x col :: replaceHelper c xs (row-1) col
  | (x :: xs) -> x :: replaceHelper c xs (row-1) col
  | [] -> []
and inRow (c:char)(row: char list)(col:int) =
  match row with
  (x :: _) when col = 0 && x <> '0' && x <> c -> failwith "Position occupied with a different line"
  | (_ :: xs) when col = 0 -> c :: inRow c xs (col-1)
  | (x :: xs) -> x:: inRow c xs (col-1)
  | [] -> []


//update this with functions for getting next states, getting cost and heuristic
//[<CustomComparison; StructuralEquality>]
type BoardState = {size: int; lines: Map<char,Line>; board: char list list}
with
  // interface IComparable<BoardState> with
  //   member this.CompareTo other =
  //     compare this.GetHeuristic other.GetHeuristic
  // interface IComparable with
  //   member this.CompareTo(obj: obj) =
  //     match obj with
  //     | :? BoardState -> compare this.GetHeuristic (unbox<BoardState> obj).GetHeuristic
  //     | _ -> invalidArg "obj" "Must be of type Point"
 

  //compares boards for GreedyBest
  member b.Print =
    for rows in b.board do
      printfn "%A" rows

  member b.CompareGreedyBest (otherBoard: BoardState): int =
    if b.GetHeuristic>otherBoard.GetHeuristic then 1
    else if b.GetHeuristic<otherBoard.GetHeuristic then -1
    else 0

  member b.CompareAStar (otherBoard: BoardState): int =
    if b.GetCost+b.GetHeuristic>otherBoard.GetCost+otherBoard.GetHeuristic then 1
    else if b.GetCost+b.GetHeuristic<otherBoard.GetCost+otherBoard.GetHeuristic then -1
    else 0
  member b.GetCost =
    Map.fold (fun state _ value -> state + value.length) 0 b.lines

  //h(s) = manhattan distance
  member b.GetHeuristic = 
    Map.fold (fun state _ (value:Line) -> state + value.GetDistance) 0 b.lines
  
  member b.AtGoal =
    Map.fold(fun state _ (value:Line) -> state && value.AtGoal) true b.lines
  
  //returns a board updated with the new pos of the end of the line
  member b.Update(c:char)(p: Pos): BoardState =
    let foundLine = Map.tryFind c b.lines in 
    let lines' =
      match foundLine with
      None -> failwith "Trying to move line that doesn't exist"
      | (Some l1) -> b.lines |> Map.remove c |> Map.add c (l1.UpdateEndPos p)
    in
    {size = b.size; lines = lines'; board = replacePos c p b.board}

  //gets all of the possible next states
  member b.GetNextStates : BoardState list =
    let mutable boards = [b]
    for (c,_) in Map.toList b.lines do
      boards <- (List.append boards (b.GetNextStatesOfLine c))
    List.tail boards


 //gets next states of a paritcular line by checking if can move in a particualr direction
 //TODO: fix so less stupid (hw2a might have a better way)
  member b.GetNextStatesOfLine (c:char) : BoardState list =
    match (b.CanMoveDown c), (b.CanMoveLeft c), (b.CanMoveRight c), (b.CanMoveUp c) with
    ((Some p1), (Some p2), (Some p3), (Some p4)) -> b.Update c p1 :: b.Update c p2 :: b.Update c p3 :: [b.Update c p4]                                                                      
    | ((Some p1), (Some p2), (Some p3), (None)) -> b.Update c p1 :: b.Update c p2 :: [b.Update c p3]
    | ((Some p1), (Some p2), (None), (Some p4)) -> b.Update c p1 :: b.Update c p2  :: [b.Update c p4]
    | ((Some p1), (None), (Some p3), (Some p4)) -> b.Update c p1  :: b.Update c p3 :: [b.Update c p4]
    | ((None), (Some p2), (Some p3), (Some p4)) -> b.Update c p2 :: b.Update c p3 :: [b.Update c p4]
    | ((Some p1), (Some p2), (None), (None)) -> b.Update c p1 :: [b.Update c p2]
    | ((Some p1), (None), (Some p3), (None)) -> b.Update c p1 :: [b.Update c p3]
    | ((None), (Some p2), (Some p3), (None)) -> b.Update c p2 :: [b.Update c p3]
    | ((Some p1), (None), (None), (Some p4)) -> b.Update c p1 :: [b.Update c p4]
    | ((None), (Some p2), (None), (Some p4)) -> b.Update c p2 :: [b.Update c p4]
    | ((None), (None), (Some p3), (Some p4)) -> b.Update c p3 :: [b.Update c p4]
    | ((Some p1), (None), (None), (None)) -> [b.Update c p1]
    | ((None), (Some p2), (None), (None)) -> [b.Update c p2]
    | ((None), (None), (Some p3), (None)) -> [b.Update c p3]
    | ((None), (None), (None), (Some p4)) -> [b.Update c p4]
    | ((None), (None), (None), (None)) -> []
    
//checks if line can move in a particular direction
//put a check to see if already at goal, not sure if needed
  member b.CanMoveUp (c:char) : Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (row-1>=0) && not l1.AtGoal && (b.board.[row-1].[col] = '0' || l1.goalPos = (row-1,col))-> Some (row-1,col)
                   |(_, _) -> None
      | _ -> failwith "line does not exist"

  member b.CanMoveDown (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (row+1<b.size) && not l1.AtGoal && (b.board.[row+1].[col] = '0' || l1.goalPos = (row+1,col)) -> Some (row+1, col)
                   |(_, _) -> None
      | _ -> failwith "line does not exist"

  member b.CanMoveRight (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (col+1<b.size) && not l1.AtGoal && (b.board.[row].[col+1] = '0' || l1.goalPos = (row,col+1))-> Some (row, col+1)
                   |(_, _) ->  None
      | _ -> failwith "line does not exist"

  member b.CanMoveLeft (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (col-1>=0) && not l1.AtGoal && (b.board.[row].[col-1] = '0' || l1.goalPos = (row,col-1))-> Some (row, col-1)
                   |(_, _)-> None
      | _ -> failwith "line does not exist"
    

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
  

//constructs the initial board if valid
let constructInitialBoard (f:FileInput): BoardState =
  match f with
  (len, _) when checkValidBoard f -> {size = len; lines = convertToMap f; board = convertToCharListList f}
  | _  -> failwith "invalidBoard"

//test board
let testInput = (4, "00a000b00a0000b0")
let initialBoard = constructInitialBoard testInput

let queue = new PriorityQueue<BoardState>([initialBoard])


let temp = queue.Dequeue
match temp with (a,b) 

while (not tempBoard. && queue.Size>0) do
  

let mutable frontier = [initialBoard]
let mutable tempBoard = List.head frontier

while (not tempBoard.AtGoal && frontier.Length>0) do
  frontier <- List.tail frontier
  frontier <- List.append frontier tempBoard.GetNextStates
  tempBoard <- List.head frontier

if (not tempBoard.AtGoal) then printfn "%s" "Unsolvable"
else tempBoard.Print
tempBoard.Print
