
//Completed:
// Take the input type of int string and convert to n by n char list
// Take the input type of int string and convert to a map where the key
//   is the char and the value is Type line where line has a char name,
//   end position of the line, goal position of the line, and length of line 
// not sure if we need length, but will make calculating cost of state easier I think
// wasn't sure if should be initially 0 or 1 to begin with but shouldn't matter 
//   as long as its consistant

type pos = int * int

//represents the different lines
type line = {name: char; endPos: pos; goalPos: pos; length: int}
with
  //updates the postion of the end of the line, adds one to length
  member l.UpdateEndPos (p: pos) = {name = l.name; endPos = p; 
  goalPos = l.goalPos; length = l.length+1}


//update this with functions for getting next states, getting cost and heuristic
type boardState = {size: int; lines: Map<char,line>; board: char list list}


//What the function that reads in puzzle from a file should return
type fileInput = int * string

let checkValidInputSize (f:fileInput): bool = 
  match f with
  (len, str) when sqrt (double str.Length) = (double len) -> true
  | _ -> false 

//takes in the file representation and converts to n by n char list
//invalid check should be move elsewhere
let rec convertToCharChar (f: fileInput) : char list list =
  match f with
  (len, str) when (checkValidInputSize f) -> split len (Array.toList (str.ToCharArray())) 
  | _ -> failwith "Invalid Board"
and split (n:int)(c: char list): char list list =
  match c with
  [] -> []
  | _ -> firstN n c :: split n (removeN n c)
and firstN (n: int)(c: char list): char list =
  match c with
  (x::xs) when n=1 -> [x]
  |(x::xs) -> x :: firstN (n-1) xs
  | _ -> []
and removeN (n: int)(c: char list): char list =
  match c with
  (x::xs) when n=1 -> xs
  |(x::xs) -> removeN (n-1) xs
  | _ -> []


//takes in the file input and outputs the map representation
//should move the invalid check seperately
let rec convertToMap (f: fileInput) : Map<char, line> =
  match f with
    (len, str) when (checkValidInputSize f) -> addToMap len (Array.toList (str.ToCharArray()))
    | _ -> failwith "Invalid Board"
and addToMap (n: int)(c: char list): Map<char, line> =
  let mutable m = Map.empty<char,line>
  for i = 0 to c.Length-1 do
    if c.[i] <> '0' && m.ContainsKey (c.[i]) then
     let tempLine = (Map.find c.[i] m)
     m <- m.Remove(c.[i])
     m <- m.Add(c.[i], {name = tempLine.name; endPos = tempLine.endPos; goalPos = (i/n, i%n); length = 1})
    else if c.[i] <> '0' then m <- m.Add(c.[i], {name = c.[i]; endPos = (i/n, i%n); goalPos = (-1,-1); length = 1})
  m



//test board
let testInput = (3, "00ab0ab00")
convertToCharChar testInput
convertToMap testInput

let boardState = {size = 3; lines = convertToMap testInput; board = convertToCharChar testInput }
boardState.board
boardState.lines