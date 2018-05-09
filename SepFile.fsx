//TODO: integrate file reading and randomly generated boards
//TODO: analysis

open System
open Microsoft.FSharp.Core
open System.IO

//What the function that reads in the puzzles from a file should return
type FileInput = int * string

//auxiliary function used in reverse
let rec append l m =
  match l with
  | [] -> m 
  | h :: t -> h :: (append t m) 
//auxiliary function used in reverse
let move l1 l2 =
    let rec reverser l1m = function
      | [] -> append l1m l2
      | x::l1 -> reverser(x::l1m) l1
    reverser [] l1

//reverse items in a list function
let reverse l = if l <> [] then move l [] else failwith "empty list"

//reads in 250 files from a puzzle
let getPuzzles: FileInput list = 

  //Retrieve directory path
  let baseDirectory = __SOURCE_DIRECTORY__
  let baseDirectory' = Directory.GetParent(baseDirectory)
  let filePath = "AI-NumberLink-Project/testCases.txt"
  let fullPath = Path.Combine(baseDirectory'.FullName, filePath)
  //Converted each line in text as elements 
  let data =  File.ReadAllLines(fullPath)

  let mutable counter = 1
  let mutable sizearr = []
  let mutable boardarr = []
  //Depending on line number, add to size array or board array
  for value in data do
    if counter%3 = 1 then 
      if value.Length > 0 then
        sizearr <- value :: sizearr
    else if counter%3 = 2 then 
      if value.Length > 0 then
        boardarr <- value :: boardarr
    counter <- counter + 1

  sizearr <- reverse sizearr
  boardarr <- reverse boardarr
  //convert corresponding elements in each array to tuple so can be converted to proper data structure 
  let mutable boardstuplearr: FileInput list = []
  for i in 0 ..(boardarr.Length - 1) do
    boardstuplearr <- ( (int)sizearr.[i], boardarr.[i]) :: boardstuplearr

  boardstuplearr <- reverse boardstuplearr
  boardstuplearr


//change this to change how comparable is implemented
let mutable heuristic = ""
let aStar = "astar"
let greedyBest = "greedy"

type Pos = int * int

//represents the a line in the puzzle
type Line = {name: char; endPos: Pos; goalPos: Pos; length: int}
with
  //updates the postion of the end of the line, adds one to length
  member l.UpdateEndPos (p: Pos) = {name = l.name; endPos = p; 
  goalPos = l.goalPos; length = l.length+1}

  //manhatan distance of the line
  member l.GetManhattanDistance = 
    match (l.endPos, l.goalPos) with
    | (x1,y1),(x2,y2) -> abs (x1-x2) + abs (y1-y2)

  //checks if AtGoal
  member l.AtGoal = l.endPos = l.goalPos

  //checks if within one of goal
  member l.WithinOne = 1 = l.GetManhattanDistance

  //uniqueness
  member l.Hash = 
   match l.endPos with
   | (r,c) -> string l.name + string r + string c  + string l.length + string l.GetManhattanDistance


//sets the Pos in the board to the char
let rec replacePos (c: char)(p: Pos)(old: char list list): char list list =
  match p with
  | (row,col) when row>=old.Length || col>=old.Length || row<0 || col<0 -> failwith "Position out of bounds"
  | (row,col) -> replaceHelper c old row col

and replaceHelper (c:char)(old: char list list)(row: int)(col: int): char list list =
  match old with
  | (x :: xs) when row = 0 -> inRow c x col :: xs
  | (x :: xs) -> x :: replaceHelper c xs (row-1) col
  | [] -> []
and inRow (c:char)(row: char list)(col:int) =
  match row with
  | (x :: _) when col = 0 && x <> '0' && x <> c -> failwith "Position occupied with a different line"
  | (_ :: xs) when col = 0 -> c :: xs
  | (x :: xs) -> x:: inRow c xs (col-1)
  | [] -> []

//update this with functions for getting next states, getting cost and heuristic
[<CustomComparison; StructuralEquality>]
type BoardState = {size: int; lines: Map<char,Line>; board: char list list}
with
  interface IComparable<BoardState> with
    member this.CompareTo other =
      if (heuristic = greedyBest) then
        compare this.GetGreedyBest other.GetGreedyBest
      else if (heuristic = aStar) then 
        compare other.GetAStar this.GetAStar
        // if this.GetAStar = other.GetAStar then
        //   compare this.GetManhattanDistance other.GetManhattanDistance
        // else
        //   compare this.GetAStar other.GetAStar
      else 0 //just treat as the same
  interface IComparable with
    member this.CompareTo(obj: obj) =
      match obj with
      | :? BoardState when (heuristic = greedyBest) -> compare (unbox<BoardState> obj).GetGreedyBest this.GetGreedyBest
      //Part 2 improvement:
        // if same heuristic tiebreaker is lower h(s)
      | :? BoardState when (heuristic = aStar) -> compare (unbox<BoardState> obj).GetAStar this.GetAStar
        //if ((unbox<BoardState> obj).GetAStar) = this.GetAStar then
          //compare this.GetManhattanDistance (unbox<BoardState> obj).GetManhattanDistance
        //else
          //compare this.GetAStar (unbox<BoardState> obj).GetAStar
      | :? BoardState -> 0 
      | _ -> invalidArg "obj" "Must be of type BoardState"

  //prints the 2d representation of the board  
  member b.Print =
    for rows in b.board do
      printfn "%A" rows

  //uniqueness
  member b.Hash =
    Map.fold (fun state _ (value:Line) -> state + value.Hash) "" b.lines

  //g(s) + h(s)
  member b.GetAStar = b.GetCost + b.GetManhattanDistance
  
  //h(s)
  member b.GetGreedyBest = b.GetManhattanDistance
  member b.GetCost =
    Map.fold (fun state _ (value:Line) -> state + value.length) 0 b.lines

  //h(s) = manhattan distance
  member b.GetManhattanDistance = 
    Map.fold (fun state _ (value:Line) -> state + value.GetManhattanDistance) 0 b.lines
  
  member b.AtGoal =
    //Part 2 improvement:
    //can't be the goal if cost not at least the manhattan distance
    if (b.GetCost<b.GetManhattanDistance) then false
    else Map.fold(fun state _ (value:Line) -> state && value.AtGoal) true b.lines
  
  //returns a board updated with the new pos of the end of the inputted line
  member b.Update(c:char)(p: Pos): BoardState =
    let foundLine = Map.tryFind c b.lines in 
    let lines' =
      match foundLine with
      | None -> failwith "Trying to move line that doesn't exist"
      | (Some l1) -> b.lines |> Map.remove c |> Map.add c (l1.UpdateEndPos p)
    in
    {size = b.size; lines = lines'; board = replacePos c p b.board}

  //gets all of the possible next states
  member b.GetNextStates : BoardState list =
    let mutable boards = []
    //Part 2 improvement:
    //if a line is not a goal, and has no next states (trapped) then this
    // path is a dead end and no need to continue
    // Also no need to get next states if already at goal or within one of goal
    let mutable stop = false
    for (k,v) in Map.toList b.lines do
      // if v.WithinOne && not stop then
      //   boards <- [b.Update v.name v.goalPos]
      //   stop <- true
      if not stop && not v.AtGoal then
        match b.GetNextStatesOfLine k with
        |[] -> boards<- []
               stop <- true
        | next -> boards <- List.append boards next
    boards


 //gets next states of a paritcular line by checking if can move in a particular direction
  member b.GetNextStatesOfLine (c:char) : BoardState list =
    let pos =  Map.tryFind c b.lines in
      match (b.CanMoveDown pos), (b.CanMoveLeft pos), (b.CanMoveRight pos), (b.CanMoveUp pos) with
      | ((Some p1), (Some p2), (Some p3), (Some p4)) -> b.Update c p1 :: b.Update c p2 :: b.Update c p3 :: [b.Update c p4]                                                                      
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
  member b.CanMoveUp (cpos:Line option) : Pos option = 
    match cpos with
    |(Some l1) -> match l1.endPos with
                  |(row, col) when (row-1>=0) && (b.board.[row-1].[col] = '0' || l1.goalPos = (row-1,col)) -> Some (row-1,col)
                  |(_, _) -> None
    | _ -> failwith "line does not exist"

  member b.CanMoveDown (cpos:Line Option): Pos option = 
    match cpos with
    |(Some l1) -> match l1.endPos with
                  |(row, col) when (row+1<b.size) && (b.board.[row+1].[col] = '0' || l1.goalPos = (row+1,col)) -> Some (row+1, col)
                  |(_, _) -> None
    | None -> failwith "line does not exist"

  member b.CanMoveRight (cpos:Line Option): Pos option = 
    match cpos with
    |(Some l1) -> match l1.endPos with
                  |(row, col) when (col+1<b.size) && (b.board.[row].[col+1] = '0' || l1.goalPos = (row,col+1))-> Some (row, col+1)
                  |(_, _) ->  None
    | None -> failwith "line does not exist"

  member b.CanMoveLeft (cpos:Line Option): Pos option = 
    match cpos with
    |(Some l1) -> match l1.endPos with
                  |(row, col) when (col-1>=0) && (b.board.[row].[col-1] = '0' || l1.goalPos = (row,col-1))-> Some (row, col-1)
                  |(_, _)-> None
    | None -> failwith "line does not exist"
    

//takes in the file representation and converts to n by n char list
let rec convertToCharListList (f: FileInput) : char list list =
  match f with
  | (len, str) -> split len (Array.toList (str.ToCharArray())) 
and split (n:int)(c: char list): char list list =
  match c with
  | [] -> []
  | _ -> firstN n c :: split n (removeN n c)
and firstN (n: int)(c: char list): char list =
  match c with
  | (x::_) when n=1 -> [x]
  | (x::xs) -> x :: firstN (n-1) xs
  | _ -> []
and removeN (n: int)(c: char list): char list =
  match c with
  | (_::xs) when n=1 -> xs
  | (_::xs) -> removeN (n-1) xs
  | _ -> []


//takes in the file input and outputs the map representation
let rec convertToMap (f: FileInput) : Map<char, Line> =
  match f with
  | (len, str)  -> addToMap len (Array.toList (str.ToCharArray()))
and addToMap (n: int)(c: char list): Map<char, Line> =
  let mutable m = Map.empty<char,Line>
  for i = 0 to c.Length-1 do
    if c.[i] <> '0' && m.ContainsKey (c.[i]) then
     let tempLine = (Map.find c.[i] m)
     m <- m.Remove(c.[i])
     m <- m.Add(c.[i], {name = tempLine.name; endPos = tempLine.endPos; goalPos = (i/n, i%n); length = 0})
    else if c.[i] <> '0' then m <- m.Add(c.[i], {name = c.[i]; endPos = (i/n, i%n); goalPos = (-1,-1); length = 0})
  m


//checks that a board is valid:
// - sqrt string.size = length
// - there are exactly 2 of each none '0' char
let rec checkValidBoard(f: FileInput): bool =
  match f with
  | (_, str) -> checkValidInputSize f && checkTwoOfEach (List.sort (Array.toList (str.ToCharArray())))
and checkTwoOfEach(c: char list) : bool = 
  match c with
  | (x :: y :: xs) when x <>'0' -> x = y && checkTwoOfEach xs
  | (x :: y :: xs) when x ='0' -> checkTwoOfEach (y::xs)
  | ([_]) -> false
  | _ -> true
and checkValidInputSize (f:FileInput): bool = 
  match f with
  | (len, str) when sqrt (double str.Length) = (double len) -> true
  | _ -> false 
  

//constructs the initial board if valid
let constructInitialBoard (f:FileInput): BoardState =
  match f with
  | (len, _) when checkValidBoard f -> {size = len; lines = convertToMap f; board = convertToCharListList f}
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


//Heuristic used by greedy best first search is h(s), which is the manhattan distance. The manhattan distance will always 
//be admissible, that is it will always underestimate or equal the actual cost to reach the goal. Iterate through the 
//board possibilities that have not been explored. Enqueue the randomly generated m by m board into the priority queue we
//are using to maintain cost of 'nodes', in this case the potential solutions. Then, enqueue board states that have not been visited
//from the dequeued board state. In order to prevent iterating through previously visited list of board states, we mantain a list 
//that has a unique has for each board state that has been visited. Timeout is after 100 seconds, which we automatically determine
//the puzzle is unsolvable. 
let GreedyBestFirst(fileState: FileInput): BoardState option =
  heuristic <- greedyBest //this changes comparable to h(s)
  let mutable tempBoard = constructInitialBoard fileState
  let queue = new PriorityQueue<BoardState>([|tempBoard|])
  let mutable beenTo = Set.empty<string>
  beenTo<- beenTo.Add tempBoard.Hash
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()

  while ((not tempBoard.AtGoal) && (queue.getSize <> 0) && (stopWatch.Elapsed.TotalMilliseconds < 100000.0)) do
    for i in tempBoard.GetNextStates do
      if not (beenTo.Contains i.Hash) then 
        queue.Enqueue i
        beenTo <- beenTo.Add i.Hash
    tempBoard <- queue.Dequeue()

  if tempBoard.AtGoal then 
    printfn "%s" ("#Expanded Nodes:" + string (queue.getSize + beenTo.Count))
    Some tempBoard
  else None  


//Heuristic used by A* search is h(s) + g(s), which is the manhattan distance + actual cost from given position to the goal state. Iterate through the 
//board possibilities that have not been explored. Enqueue the randomly generated m by m board into the priority queue we
//are using to maintain cost of 'nodes', in this case the potential solutions. Then, enqueue board states that have not been visited
//from the dequeued board state. In order to prevent iterating through previously visited list of board states, we mantain a list 
//that has a unique has for each board state that has been visited. Timeout is after 100 seconds, which we automatically determine
//the puzzle is unsolvable. 
let AStar(fileState: FileInput): BoardState option =
  heuristic <- aStar //this changes comparable to h(s) + g(s)
  let mutable tempBoard = constructInitialBoard fileState
  let queue = new PriorityQueue<BoardState>([|tempBoard|])
  let mutable beenTo = Set.empty<string>
  beenTo<- beenTo.Add tempBoard.Hash
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()

  while ((not tempBoard.AtGoal) && (queue.getSize <> 0) && (stopWatch.Elapsed.TotalMilliseconds < 100000.0)) do
    //printfn "%s" "here"
    for i in tempBoard.GetNextStates do
      //i.Print
      //printfn "%s" i.Hash
      if not (beenTo.Contains i.Hash) then 
        queue.Enqueue i
        beenTo <- beenTo.Add i.Hash
    tempBoard <- queue.Dequeue()

  if tempBoard.AtGoal then 
    printfn "%s" ("#Expanded Nodes:" + string (queue.getSize + beenTo.Count))
    Some tempBoard
  else None 

//Part 2 improvement: Use the same heuristic as A* but explore all states at the smallest depth which there could be a solution. 
//If no solution, increase the depth size and iterate through all board states again. 
//Keep repeating this until reach max depth or 10 minutues have been reached. 
let IDAStar(fileState: FileInput): BoardState option =
  heuristic <- aStar //same comparable as A*
  let mutable tempBoard = constructInitialBoard fileState
  //Part 2 improvement:
  //The minimum depth of the solution is the ManhattanDistance
  let mutable depth = tempBoard.GetManhattanDistance
  while(not tempBoard.AtGoal && depth <= tempBoard.size * tempBoard.size) do
    tempBoard <- constructInitialBoard fileState
    
    let queue = new PriorityQueue<BoardState>([|tempBoard|])
    let mutable beenTo = Set.empty<string>
    beenTo<- beenTo.Add tempBoard.Hash
    
    while ((not tempBoard.AtGoal) && (queue.getSize <> 0)) do
        if tempBoard.GetCost < depth then 
          for i in tempBoard.GetNextStates do
            if not (beenTo.Contains i.Hash) then 
              queue.Enqueue i
              beenTo <- beenTo.Add i.Hash
        tempBoard <- queue.Dequeue()
    depth <- depth + 1

  if tempBoard.AtGoal then 
    Some tempBoard
  else None  

//test boards
//let testInput = (7, "000D0000C00BE0000CA00000E000000000000A0000B000D00")
//let testInput = (5,"000RG00BG0R0000PB0YP0000Y")
//let testInput = (7,"0000000GR000R0PG000000B0B0000YP00Y000000000000000")
//let testInput = (5, "B0YRP000000Y0000R0PG0BG00")

//let testInput = (3, "ABAC0C0B0")

//let testInput = (3, "a00b000ba")
//let testInput = (4, "a000ba0b00000000") 
//let testInput = (5,"Y00000000000G00BGR0YR000B")
//let testInput = (5,"A0000B00000000000000000BA")
//let testInput = (5,"000RGR000000Y00000B0GBY00")
//let testInput = (10, "A00000000AB00000000BC00000000CD00000000DE00000000EF00000000FG00000000GH00000000HI00000000IJ00000000J")
//let testInput = (8, "0n00000n0r0z0cq0kq0v00000000000000zr00v0000000000000000000c0k000")
//let testInput = (8,"0f0000000f00000r0000000v0r00p00000000000v000000y0000y00000000p00")

//let testInput = (10, "000vz000t00j00000gv0000zt0000n000000c0000000w0h0n00000cj0000000000000w000bb000000000h0000000000000g0")

//Run Greedy
// let solutionGreedy = GreedyBestFirst testInput
// match solutionGreedy with
//   (Some sol) -> sol.Print
//   |(None) -> printfn "no solution"
// let cons = constructInitialBoard testInput
// cons.Print

//run AStar

// let solutionAStar = GreedyBestFirst testInput
// match solutionAStar with
//   (Some sol) -> sol.Print
//   |(None) -> printfn "no solution"



//IDAStar
// let solutionIDAStar = IDAStar testInput
// match solutionIDAStar with
//   (Some sol) -> sol.Print
//   |(None) -> printfn "no solution"

//TODO: add timeout and results stuff

let mutable runningTimeGreedy = 0.0
let mutable runningTimeAStar = 0.0
let mutable thetimes = []

for i in getPuzzles do
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  match GreedyBestFirst i with
    (Some sol) -> sol.Print
    |(None) -> printfn "no solution"
  stopWatch.Stop()
  runningTimeGreedy <- stopWatch.Elapsed.TotalMilliseconds
  stopWatch.Reset()

  stopWatch.Start()
  match AStar i with
    (Some sol) -> sol.Print
    |(None) -> printfn "no solution"
  stopWatch.Stop()
  runningTimeAStar <- stopWatch.Elapsed.TotalMilliseconds

  thetimes <- (runningTimeGreedy, runningTimeAStar) :: thetimes

thetimes <- reverse thetimes

