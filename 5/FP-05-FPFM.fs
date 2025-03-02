module test


type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree
// 5.1

let rec inOrder = function
    | Leaf -> []
    | Node(n,treeL,treeR) -> (inOrder treeL) @  [n] @ (inOrder treeR)

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

// 5.2

let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(n,treeL,treeR) -> 
        let left = mapInOrder f treeL
        let mappedValue = f n
        let right = mapInOrder f treeR
        Node(mappedValue, left, right)

//Can you give an example of why mapInOrder might give a result different from mapPostOrder, but the
//result tree returned in both cases is still the same.
//Answer: mapInOrder and mapPostOrder would apply the function given to all the values regardless of the order in which the tree is traversed. 
//Therefore, the result tree returned will be the same in both cases. However, if we were to print a list, we would have a different order
//because we would traverse the tree in two different ways. In mapInOrder we traverse the tree from left sub-tree, then visit node and finally
//traverse the right sub-tree. Post-order traversal does almost the same thing, but it traverses the right sub-tree before the node, and at last
//we traverse the root node. Hence, we would print values on a list in a different order by applying these 2 functions.


// 5.3

let floatBinTree = 
    Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
        Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

//Uses accumulator as a variable or a box where you can store values. Traverses the left side of the tree first, then the root node and
// finally the right side of the tree to finally compute the sum of all the values found in the tree.
let rec foldInOrder f acc = function
  | Leaf -> acc
  | Node (n, treeL, treeR) ->
      let acc' = foldInOrder f acc treeL in
      let acc'' = f n acc' in
      foldInOrder f acc'' treeR


//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm          // if-then 
    | RU of stm * bExp          //  repeat-until 

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;


let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(a1, a2) -> A a1 s = A a2 s
    | Lt(a1, a2) -> A a1 s < A a2 s
    | Neg(b) -> not(B b s)
    | Con(b1, b2) -> B b1 s && B b2 s


let rec I stm s =
    match stm with
    | Ass(x,a) -> Map.add x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b, stm) -> if B b s then I (Seq(stm, While(b, stm))) s else s
    | IT(b, stm) -> if B b s then I stm s else s
    | RU(stm, b) -> I (Seq(stm, IT(Neg b, RU(stm, b)))) s


// Example 0 // Assigns the sum of two integers/numbers (10 + 30) to the variable "res" -> results in map[("res, 40")].
let stmt0 = Ass("res",(Add(N 10, N 30)))
let state0 = Map.empty;;
// Example 1 // Sequence of two statements. First, it assigns the value 5 to the variable "x". Second, it assigns the value of "x" plus 3 to
// the variable "y". The initial state is an empty map.
let stmt1 = Seq(Ass("x", N 5), Ass("y", Add(V "x", N 3)))
let state1 = Map.empty;;
// Example 2 // If-then-else statement to assign a value to either "x" or "y", depending on whether the condition "10 < 20" is true or false
// The condition is true in this particular case, therefore "x" is assigned the value 55.
let stmt2 = ITE(Lt(N 10, N 20), Ass("x", N 55), Ass("y", N 5))
let state2 = Map.empty;;
// Example 3 // This example uses a while loop to repeatedly increment the value of the variable "x" until it reaches 10. 
// The initial value of "x" is 5.
let stmt3 = While(Lt(V "x", N 10), Ass("x", Add(V "x", N 1)))
let state3 = Map.add "x" 5 Map.empty;;
// Example 4 // Repeat-until loop. Assigns value 1 to the variable "x" until it equals 10.
let stmt4 = RU(Ass("x", N 1), Eq(V "x", N 10))
let state4 = Map.empty;;
// Example 5 // First, assigns the value "x" to 5 using the order of execution of Seq(stmt1, stmt2). Then stmt2 executes and since the first
// parameter is skip, nothing happens. It then adds the 3 to the variable "y" by using the value of variable "x", which previously was assigned 5. Therefore, it results in
// 5 + 3 = 8. It then assigns this calculation to the variable "y" as mentioned before.
let stmt5 = Seq(Ass("x", N 5), Seq(Skip, Ass("y", Add(V "x", N 3))))
let state5 = Map.empty;;

(* Exercise 5.6 Suppose that an expression of the form inc(x) is added to the abstract syntax. It adds one to the
value of x in the current state, and the value of the expression is this new value of x. The expression inc(x) should
be added to the type aExp.
How would you refine the interpreter to cope with this construct? 

Answer: In order to the add inc(x) expression to the abstract syntax and update the interpreter to handle it, we can make the following modifications:
1. Add a 'Inc' constructor to 'aExp' type to represent the increpement operation.
2. Return the updated state along with the result of the expression evaluation from the 'A' function.
3. Modify 'I' function to handle the 'Inc' expression.

For instance, adding to type aExp =
    | Inc of string

It can also be showcased with this example:
Ass("x", N(5));
Add(N(3), inc("x"))
Before the addition of inc(x), the evaluation of the second expression would simply return the value 8. However, with the addition
of inc(x), the evaluation of the second expression should increment the value of x to 6, and then return that new value 9 instead of 8.
*)

