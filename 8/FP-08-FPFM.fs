(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>

let rec countA t n =
  match t with
  | Leaf -> n
  | Node (left, _, right) -> countA left (countA right (n + 1))
(* //> countA t 0;;
Real: 00:00:00.004, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 4 *)

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA t 0

(* Assignment 8.2, HR 9.9 *)
//might be wrong, although it's counting the number of nodes
let countAC t n c =
  let rec countHelper t acc = 
    match t with
    | Leaf -> acc
    | Node (left, _, right) -> countHelper left (countHelper right (acc + 1))
  c (countHelper t n)
(* > countAC t 0 id;;
Real: 00:00:00.004, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 4 *)
(* Example *)
countAC t 0 id

(* Assignment 8.3, HR 9.10 *)
(* 
    bigListK is a recurive function that generates a list of size 'n', where each element of the list
    is the value '1'. The continuation 'k' is a function that takes the list generated so far and
    returns the final result. 'id' is the identity function, which returns the argument.

    However, there's a problem with the call 'bigListK 300000 id'. It generates a very large call stack.
    When bigListK is called recursively, it creates a new continuation function, which is passed as an agument
    to the next recursive call. In this particular case, 'n' is '300000' which is a big number and this in turn
    causes the call stack to grow to a very large size, eventually causing a stack overflow.

    It's worth noting that it could be avoided by rewriting the function using tail recursion.
 *)
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))

(* Assignment 8.4, HR 9.11 *)
//let rec leftTreeC n c = failwith "Not implemented"
//let leftTree n = failwith "leftTreeC ..."

let leftTree n =
  let rec leftTreeC n c acc =
    if n = 0 then acc
    else leftTreeC (n-1) (c+1) ((n+c)::acc)
  in leftTreeC n 1 []
(* Examples *)
leftTree 0
leftTree 1
leftTree 360000

//let rec rightTreeC n c = failwith "Not implemented"
//let rightTree n = failwith "rightTreeC ..."

let rightTree n =
  let rec rightTreeC n c acc =
    if n = 0 then acc
    else rightTreeC (n-1) (c+1) (acc@[n+c])
  in rightTreeC n 1 []
(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 360000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1
(* //> count t;;
Real: 00:00:00.005, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 4 *)

let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))
(* //> countC t id;;
Real: 00:00:00.008, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 4 *)

(* Assignment 8.5, HR 11.1 *)
let oddNumbers = Seq.initInfinite (fun i -> i * 2 + 1)

(* Assignment 8.6, HR 11.2 *)

let fac =
    Seq.unfold (fun state ->
        match state with
        | 0, acc -> Some(1, (1, 1))
        | n, acc -> let next = acc * (n + 1)
                    Some(next, (n + 1, next))) (0, 1)

(* let fac =
    Seq.unfold (fun state ->
        match state with
        | -1, acc -> Some(1, (0, 1))
        | n, acc -> let next = acc * (n + 1)
                    Some(next, (n + 1, next))) (-1, 1) *)

(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 3 fac
Seq.take 10 fac

(* let fac =
    Seq.unfold (fun n -> 
        let x = if n = 0 then 1 else n * fac(n-1)
        Some (x, n+1)) 0

let test1 = Seq.take 0 fac
let test2 = Seq.take 1 fac
let test3 = Seq.take 2 fac
let test4 = Seq.take 10 fac *)
