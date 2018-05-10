(*
Project 1: Numberlink Puzzle
We implemented the state of the board as n by n list of char and a map of <char,Line> where
the Line kept track of the length, the goal position in the board, and the current end of the
line position in the board. Each state gets its next state by checking if each end of line can
extend up,down,right, or left. Those next states are returned and added to the priority queue.
For greedy best first search the priority queue is ordered on the manhattan distance by implementing
comparable for the board which compare the manhattan distances.
For A* search the priority queue is ordered on the manhattan distance + the cost. The cost is the
total cost of each line in the board.

The starting configuration boards are randomly generated using a python script. These boards are 
included in file: testCases.txt which needs to be located in the same directory as this file to work.
The boards are validated when the initial representation is constructed. The validation checks if
there are the correct number (n^2, where n is board size) and if there exactly 2 of each non zero
char.

For part two we implemented A* with iterative deepening. We improved it by starting the inital max
depth to be the least possible solution of the board (the manhattan distance) and then incrementing by
1 until the solution is found or it reach the max solution (the entire board is filled: n^2).

Other improvements we made for part two:
If a line is not at the goal and if it has no more possible moves (trapped) then there are no next
states because there would be no reason to continue expanding.
Changing the beenTo set which keeps track of the boards that have already been expanded to count
some boards as identical to save time and prevent uneeded node expansion. This problem is especially
an issue with random boards where the are multiple least cost solutions going from position a to position
b that are slighlty different but for the sake of the solution are the same.
Example
With the initial board of:
a 0 0
0 0 0
0 0 a
the states:
a a 0      a 0 0 
0 a 0      a a 0
0 0 a      0 0 0
can be thought of as the same which prevents unecessary node expansion. For a simple problem like a
3 by 3 this improvement is not noticable. But it makes a lot of the larger puzzles much quicker to
solve.

Statistics:
Number of unique expanded Nodes: Size of beenTo + size of queue at end
Cost: g(s) of solution board
Depth: g(s) + 1(root) because as the depth is increased, the cost increases by one
Branching Factor: Max: 4*number of lines 

*)

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

//Retrieve directory path
let baseDirectory = __SOURCE_DIRECTORY__
let baseDirectory' = Directory.GetParent(baseDirectory)
//reads in 250 files from a puzzle
let getPuzzles: FileInput list = 
  //use to get the test cases
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
//greedyBest signifies that comparable compare h(s)
//aStar signifies that comparable compares g(s) + h(s)
let mutable heuristic = ""
let aStar = "astar"
let greedyBest = "greedy"

//position within the board (x,y)
type Pos = int * int

//Line Record: represents a line in the board
//Each line knows its current end of line position, goal position and length
//the length is needed to calculate the g(s)
//Line also has the ability to calculate its manhattan distance which is used for h(s)
//Manhattan distance = |goalPos X - endPos X| + |goalPos Y - endPos Y|
//Because the manhattan distance will always underestimate or equal the cost it is an
// admissable heuristic
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

  //prevents duplicate lines that are different but can be thought of as the same
  member l.Hash = 
   match l.endPos with
   | (r,c) -> string l.name + string r + string c  + string (l.length + l.GetManhattanDistance)


//Updates the n by n char list with the expansion of the line by one position
//Checks that the replacement position within the bounds of the board and is not
//already occupied by another line. This should never occur but just there as an extra check.
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

//BoardState Record: represents the state/node
//Each board has a size, a map of the Lines in the board, and a n by n list of the board
//The map is used to easily access postions of the lines, and the n by n list is used to
//check if a line can expand in a particular direction.
//Can get the next possible states of the current BoardState, check if puzzle is solved, 
//and calculate the g(s) and h(s) by summing the g(s) and h(s) of each line
[<CustomComparison; StructuralEquality>]
type BoardState = {size: int; lines: Map<char,Line>; board: char list list}
with
  //compares the different f(s). h(s) or g(s) + h(s)
  //This implementation allows to easily change to a or add a different heuristic
  // to order the priority queue by.
  interface IComparable<BoardState> with
    member this.CompareTo other =
      if (heuristic = greedyBest) then
        compare other.GetGreedyBest this.GetGreedyBest
      else if (heuristic = aStar) then
        compare other.GetAStar this.GetAStar
      else 0 //just treat as the same
  interface IComparable with
    member this.CompareTo(obj: obj) =
      match obj with
      | :? BoardState when (heuristic = greedyBest) -> compare (unbox<BoardState> obj).GetGreedyBest this.GetGreedyBest
      | :? BoardState when (heuristic = aStar) -> compare (unbox<BoardState> obj).GetAStar this.GetAStar
      | :? BoardState -> 0 
      | _ -> invalidArg "obj" "Must be of type BoardState"

  //prints the 2d representation of the board, used for debugging and printing solution  
  member b.Print =
    for rows in b.board do
      printfn "%A" rows

  //concatenates the hash of each line, used to create a unique representation of the BoardState
  member b.Hash =
    Map.fold (fun state _ (value:Line) -> state + value.Hash) "" b.lines

  //f(n) = g(s) + h(s)
  member b.GetAStar = b.GetCost + b.GetManhattanDistance
  
  //f(n) = h(s)
  member b.GetGreedyBest = b.GetManhattanDistance

  //Sum of the lenghs of each Line
  member b.GetCost =
    Map.fold (fun state _ (value:Line) -> state + value.length) 0 b.lines

  //Sum of the manhattan distances of each Line
  member b.GetManhattanDistance = 
    Map.fold (fun state _ (value:Line) -> state + value.GetManhattanDistance) 0 b.lines
  
  //checks if the puzzle is solved by checking if the manhattan distance of the current state is 0
  //if the manhattan distance is 0 that means that each line is 0 away from the goal (at the goal)
  member b.AtGoal =
     b.GetManhattanDistance=0


  //returns a board updated with the new pos of the end of the inputted line
  member b.Update(c:char)(p: Pos): BoardState =
    let foundLine = Map.tryFind c b.lines in 
    let lines' =
      match foundLine with
      | None -> failwith "Trying to move line that doesn't exist"
      | (Some l1) -> b.lines |> Map.remove c |> Map.add c (l1.UpdateEndPos p)
    in
    {size = b.size; lines = lines'; board = replacePos c p b.board}

  //gets all of the possible next states of the current BoardState by looping through each
  // Line and getting the expansions in the different directions
  //Will not expand a line if already at its goal, doesn't need to continue past the goal position
  member b.GetNextStates : BoardState list =
    let mutable boards = []
    //Part 2 improvement:
    //if a line is not a goal, and has no next states (trapped) then this
    // path is a dead end and no need to continue
    // Also no need to get next states if already at goal
    let mutable stop = false
    for (k,v) in Map.toList b.lines do
      if not stop && not v.AtGoal then
        match b.GetNextStatesOfLine k with
        |[] -> boards<- []
               stop <- true
        | next -> boards <- List.append boards next
    boards


 //gets next states of a paritcular line by checking if can move in a particular direction
 //There is probably a better way to do this but left it like this for simplicity and readabillity
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

let mutable greedyExpandedNodes = 0
let mutable greedyCost = 0
let mutable greedyBranching = 0
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
      //if not (beenTo.Contains i.Hash) then 
      queue.Enqueue i
        //beenTo <- beenTo.Add i.Hash
    tempBoard <- queue.Dequeue()

  if tempBoard.AtGoal then 
    greedyExpandedNodes <- queue.getSize + beenTo.Count
    greedyCost <- tempBoard.GetCost
    greedyBranching <- 4*tempBoard.lines.Count
    Some tempBoard
  else
    greedyExpandedNodes <- -69
    None  


let mutable aStarExpandedNodes = 0
let mutable aStarCost = 0
let mutable aStarBranching = 0
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
    for i in tempBoard.GetNextStates do
      if not (beenTo.Contains i.Hash) then 
        queue.Enqueue i
        beenTo <- beenTo.Add i.Hash
    tempBoard <- queue.Dequeue()

  if tempBoard.AtGoal then 
    aStarExpandedNodes <- queue.getSize + beenTo.Count
    aStarCost <- tempBoard.GetCost
    aStarBranching <- 4*tempBoard.lines.Count
    Some tempBoard
  else 
    aStarExpandedNodes <- -69
    None 

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


let mutable runningTimeGreedy = 0.0
let mutable runningTimeAStar = 0.0
let mutable thetimes = []
//iterate through all puzzles and run greedy best first search and A* on them. Collect timethey take
//and add the times as a tuple to the thetimes array.

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

  let (mValue, _) = i
  
  //printfn "%s" ("Greedy: " +  string(runningTimeGreedy) + " A*: " + string(runningTimeAStar))
  let greedy_tup = (greedyExpandedNodes, greedyCost, greedyBranching, runningTimeGreedy)
  let aStar_tup = (aStarExpandedNodes, aStarCost, aStarBranching, runningTimeAStar)
  let analysis_tup = (mValue, greedy_tup, aStar_tup)
  thetimes <- analysis_tup :: thetimes

thetimes <- reverse thetimes

let filePath = "AI-NumberLink-Project/analysis_results.txt"
let fullPath = Path.Combine(baseDirectory'.FullName, filePath)

type aggregateStat = int * int * int * int * int
let printNumbersToFile fileName =
  use file = System.IO.File.CreateText(fileName)

  let mutable solvableGreedy = 0
  let mutable solvableAStar = 0



  let mutable greedy8 = []
  let mutable greedy9 = []
  let mutable greedy10 = []
  let mutable greedy11 = []
  let mutable greedy12 = []

  let mutable aStar8 = []
  let mutable aStar9 = []
  let mutable aStar10 = []
  let mutable aStar11 = []
  let mutable aStar12 = []
   //let mutable greedy 

  for tup in thetimes do
    let (mVal, greedyTup, aStarTup) = tup
    let (_, _, _, runningTimeGreedy) = greedyTup
    let (_, _, _, runningTimeAStar) = aStarTup

    if(runningTimeGreedy < 100000.0) then
      if(mVal = 8) then
        greedy8 <- greedyTup :: greedy8
      else if(mVal = 9) then
        greedy9 <- greedyTup :: greedy9
      else if(mVal = 10) then
        greedy10 <- greedyTup :: greedy10
      else if(mVal = 11) then
        greedy11 <- greedyTup :: greedy11
      else if(mVal = 12) then
        greedy12 <- greedyTup :: greedy12

      solvableGreedy <- solvableGreedy + 1

    if(runningTimeAStar < 100000.0) then
      if(mVal = 8) then
        aStar8 <- aStarTup :: aStar8
      else if(mVal = 9) then
        aStar9 <- aStarTup :: aStar9
      else if(mVal = 10) then
        aStar10 <- aStarTup :: aStar10
      else if(mVal = 11) then
        aStar11 <- aStarTup :: aStar11
      else if(mVal = 12) then
        aStar12 <- aStarTup :: aStar12
      
      solvableAStar <- solvableAStar + 1 



  let mutable averageResultsGreedy = [] 
  let mutable averageResultsAStar = []   

  let findAverageGreedy greedyArr =
    let mutable counter = 0
    let mutable totalExpandedNodes = 0
    let mutable totalCost = 0
    let mutable totalBranching = 0
    let mutable runningTimeTotal = 0.0
    for gT in greedyArr do
      let (greedyExpandedNodes, greedyCost, greedyBranching, runningTimeGreedy) = gT
      totalExpandedNodes <- totalExpandedNodes + greedyExpandedNodes
      totalCost <- totalCost + greedyCost
      totalBranching <- totalBranching + greedyBranching
      runningTimeTotal <- runningTimeTotal + runningTimeGreedy 
      counter <- counter + 1
    averageResultsGreedy <- (totalExpandedNodes/counter, totalCost/counter, totalBranching/counter, runningTimeTotal/float(counter)) :: averageResultsGreedy
    
  let findAverageAStar aStarArr =
    let mutable counter = 0
    let mutable totalExpandedNodes = 0
    let mutable totalCost = 0
    let mutable totalBranching = 0
    let mutable runningTimeTotal = 0.0

    for gT in aStarArr do
      let (aStarExpandedNodes, aStarCost, aStarBranching, runningTimeAStar) = gT
      totalExpandedNodes <- totalExpandedNodes + aStarExpandedNodes
      totalCost <- totalCost + aStarCost
      totalBranching <- totalBranching + aStarBranching
      runningTimeTotal <- runningTimeTotal + runningTimeAStar 
      counter <- counter + 1

    averageResultsAStar <- (totalExpandedNodes/counter, totalCost/counter, totalBranching/counter, runningTimeTotal/float(counter)) :: averageResultsAStar
    
  fprintf file "%s" ("Part 1-- Solvable Greedy: " + string(solvableGreedy) + " Unsolvable/Timed Out: " + string(250-solvableGreedy))
  fprintf file "\n"
  fprintf file "%s" ("Part 1-- Solvable A*: " + string(solvableAStar) + " Unsolvable/Timed Out: " + string(250-solvableAStar))
  fprintf file "\n"

  if greedy8.Length > 0 then
    findAverageGreedy greedy8
  else 
    averageResultsGreedy <- (0, 0, 0, 0.0) :: averageResultsGreedy
  
  if greedy9.Length > 0 then
    findAverageGreedy greedy9
  else 
    averageResultsGreedy <- (0, 0, 0, 0.0) :: averageResultsGreedy
  
  if greedy10.Length > 0 then
    findAverageGreedy greedy10
  else 
    averageResultsGreedy <- (0, 0, 0, 0.0) :: averageResultsGreedy
  
  if greedy11.Length > 0 then
    findAverageGreedy greedy11
  else 
    averageResultsGreedy <- (0, 0, 0, 0.0) :: averageResultsGreedy
  
  if greedy12.Length > 0 then
    findAverageGreedy greedy12
  else 
    averageResultsGreedy <- (0, 0, 0, 0.0) :: averageResultsGreedy

  averageResultsGreedy <- reverse averageResultsGreedy
  
  printfn "%A" averageResultsGreedy
  let mutable mSize = 8
  for x in averageResultsGreedy do
    let (expandedNodes, cost, branching, runningTime) = x
    fprintf file "%s" ("Part 2--Greedy Best Result " + string(mSize) + "x" + string(mSize) + "Average Expanded Nodes: " + string(expandedNodes) + " " + " Average Cost: " + string(cost) + " Average Branching: " + string(branching) + " Average running time: " + string(runningTime))
    fprintf file "\n"
    mSize <- mSize + 1

  if aStar8.Length > 0 then
    findAverageAStar aStar8
  else 
    averageResultsAStar <- (0, 0, 0, 0.0) :: averageResultsAStar
  
  if aStar9.Length > 0 then
    findAverageAStar aStar9
  else 
    averageResultsAStar <- (0, 0, 0, 0.0) :: averageResultsAStar
  
  if aStar10.Length > 0 then
    findAverageAStar aStar10
  else 
    averageResultsAStar <- (0, 0, 0, 0.0) :: averageResultsAStar
  
  if aStar11.Length > 0 then
    findAverageAStar aStar11
  else 
    averageResultsAStar <- (0, 0, 0, 0.0) :: averageResultsAStar
  
  if aStar12.Length > 0 then
    findAverageAStar aStar12
  else 
    averageResultsAStar <- (0, 0, 0, 0.0) :: averageResultsAStar

  averageResultsAStar <- reverse averageResultsAStar

  printfn "%A" averageResultsAStar
  mSize <- 8
  for x in averageResultsAStar do
    let (expandedNodes, cost, branching, runningTime) = x
    fprintf file "%s" ("Part 2--A* " + string(mSize) + "x" + string(mSize) + "Average Expanded Nodes: " + string(expandedNodes) + " " + " Average Cost: " + string(cost) + " Average Branching: " + string(branching) + " Average running time: " + string(runningTime))
    fprintf file "\n"
    mSize <- mSize + 1


          
   
printNumbersToFile fullPath