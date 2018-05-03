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

  member l.Hash = match l.endPos with
                  (r,c) -> string r + string c + string l.name + string l.length


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
[<CustomComparison; StructuralEquality>]
type BoardState = {size: int; lines: Map<char,Line>; board: char list list}
with
  
  //greedy best
  interface IComparable<BoardState> with
    member this.CompareTo other =
      compare  this.GetHeuristic other.GetHeuristic
  interface IComparable with
    member this.CompareTo(obj: obj) =
      match obj with
      | :? BoardState -> compare (unbox<BoardState> obj).GetHeuristic this.GetHeuristic
      | _ -> invalidArg "obj" "Must be of type BoardState"

   //A star
  // interface IComparable<BoardState> with
  //   member this.CompareTo other =
  //     compare  (this.GetHeuristic + this.GetCost) (other.GetHeuristic + other.GetCost)
  // interface IComparable with
  //   member this.CompareTo(obj: obj) =
  //     match obj with
  //     | :? BoardState -> compare ((unbox<BoardState> obj).GetHeuristic + (unbox<BoardState> obj).GetCost) (this.GetHeuristic + this.GetCost)
  //     | _ -> invalidArg "obj" "Must be of type BoardState"    
  member b.Print =
    for rows in b.board do
      printfn "%A" rows

  member b.Hash =
    Map.fold (fun state _ (value:Line) -> state + value.Hash) "" b.lines

  //compares boards for GreedyBest
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
                   (row, col) when (row-1>=0) && (not l1.AtGoal) && (b.board.[row-1].[col] = '0' || l1.goalPos = (row-1,col)) -> Some (row-1,col)
                   |(_, _) -> None
      | _ -> failwith "line does not exist"

  member b.CanMoveDown (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (row+1<b.size) && (not l1.AtGoal) && (b.board.[row+1].[col] = '0' || l1.goalPos = (row+1,col)) -> Some (row+1, col)
                   |(_, _) -> None
      | _ -> failwith "line does not exist"

  member b.CanMoveRight (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (col+1<b.size) && (not l1.AtGoal) && (b.board.[row].[col+1] = '0' || l1.goalPos = (row,col+1))-> Some (row, col+1)
                   |(_, _) ->  None
      | _ -> failwith "line does not exist"

  member b.CanMoveLeft (c:char): Pos option = 
    let foundLine = Map.tryFind c b.lines in
      match foundLine with
      (Some l1) -> match l1.endPos with
                   (row, col) when (col-1>=0) && (not l1.AtGoal) && (b.board.[row].[col-1] = '0' || l1.goalPos = (row,col-1))-> Some (row, col-1)
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


//Priority Queue from: https://gist.github.com/tjaskula/18a51f84ef06b4819b0952bb7e691828
let private (|Greater|_|) descendent compareResult =
    match compareResult with
    | n when n < 0 && descendent  -> None
    | n when n < 0 && not descendent  -> Some()
    | 0 -> None
    | n when n > 0 && descendent -> Some()
    | n when n > 0 && not descendent -> None
    | _ -> failwith "Impossible case for IComparable result"

let private isGreater x y descendent =
    match compare x y with
    | Greater descendent _ -> true
    | _ -> false

let private isLower x y descendent = not (isGreater x y descendent)

type PriorityQueue<'T when 'T : comparison>(values: seq<'T>, isDescending: bool) =
    let heap : System.Collections.Generic.List<'T> = System.Collections.Generic.List<'T>(values)
    
    let mutable size = heap.Count


    let parent i = (i - 1) / 2
    let leftChild i = 2 * i + 1
    let rightChild i = 2 * i + 2

    let swap i maxIndex =
        let temp = heap.[i]
        heap.[i] <- heap.[maxIndex]
        heap.[maxIndex] <- temp

    let siftUp i =
        let mutable indx = i
        while indx > 0 && isLower heap.[parent indx] heap.[indx] isDescending do
            swap (parent indx) indx
            indx <- parent indx

    let rec siftDown i =
        let l = leftChild i
        let r = rightChild i
        // maybe there is a cleaner way to express those
        let maxIndexLeft = if l < size && isGreater heap.[l] heap.[i] isDescending then l else i
        let maxIndex = if r < size && isGreater heap.[r] heap.[maxIndexLeft] isDescending then r else maxIndexLeft
        if i <> maxIndex then
            swap i maxIndex
            siftUp maxIndex
        else ()
    
    let build (unsortedValues: seq<'T>) =
        for i = size / 2 downto 0 do
            siftDown i
    
    do build heap

    new (values) = PriorityQueue<'T>(values, true)

    member this.IsEmpty = size = 0

    member this.getSize = size

    member this.Dequeue() =
        if this.IsEmpty then raise (new Exception("No more elements to dequeue"))
        let result = heap.[0]
        heap.[0] <- heap.[size - 1]
        // we limit the boundary but the last element stays in memory
        // we could use heap.Remove but it's O(n) operation so too slow
        size <- size - 1
        siftDown 0
        result

    member this.Enqueue(p: 'T) =
        if heap.Count = size then
            heap.Add(p)
        else
            heap.[size] <- p
        size <- size + 1
        siftUp (size - 1)

//test board
//let testInput = (7, "000D0000C00BE0000CA00000E000000000000A0000B000D00")
//let testInput = (5,"000RG00BG0R0000PB0YP0000Y")
//let testInput = (9, "BP00000000000000G0000R00000000000000000000000R000000000000000000PYG00B0000000Y00")
//let testInput = (7,"0000000GR000R0PG000000B0B0000YP00Y000000000000000")
//let testInput = (5, "B0YRP000000Y0000R0PG0BG00")
//let testInput = (3, "a00b000ba")
//let testInput = (5,"Y00000000000G00BGR0YR000B")
//let testInput = (5,"000RGR000000Y00000B0GBY00")
let testInput = (10, "A00000000AB00000000BC00000000CD00000000DE00000000EF00000000FG00000000GH00000000HI00000000IJ00000000J")
let initialBoard = constructInitialBoard testInput
initialBoard.lines
initialBoard.Print

let greedyQueue = new PriorityQueue<BoardState>([|initialBoard|])

let mutable tempBoard = initialBoard

let mutable beenTo = Set.empty<string>
beenTo<- beenTo.Add initialBoard.Hash
while ((not tempBoard.AtGoal) && (greedyQueue.getSize <> 0))  do
  
  //beenTo <- beenTo.Add tempBoard.Hash
  //printfn "%d" tempBoard.GetHeuristic
  printfn "%d" greedyQueue.getSize
  for i in tempBoard.GetNextStates do
    if not (beenTo.Contains i.Hash) then 
      greedyQueue.Enqueue i
      beenTo <- beenTo.Add i.Hash
  tempBoard <- greedyQueue.Dequeue() 

tempBoard.Print
beenTo.Count
