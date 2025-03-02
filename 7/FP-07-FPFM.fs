module a8

(* Assignment 7.1, HR 9.1 *)
(* Not covered by Code Judge *)
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys

g 2

(* Draw the stack  
The first part of the evaluation of g 2 makes repeated bindings of n corresponding to the recursive function calls:
g 2
-> (g n, [n -> 2])
-> (g (2-1) + n, [n -> 2])
-> g 1 + (n, [n -> 2])
...
These bindings are implemented by 3 stacks sf1,...,sf3 pushed on top of the initial stack frame sf0 corresponding to g and it. Each of the stack
frames sf1,...,sf3 corresponds to an uncompleted evaluation of a function call in which each stack frame mentioned before creates a new cell on the
heap until it reaches the base case g 0. Therefore, for g 2 we allocate a new cell on the heap for the list 'ys', which is initialized with the value
'n' which in this case is '2'. 
The resulting stack and heap would be:

sf3 ---> n = 0, result: ?    heap: xs -> [1;2]
sf2 ---> n = 1, result: ?    heap: ys -> 1 :: g(1-1)
sf1 ---> n = 2, result: ?    heap: ys -> 2 :: g(2-1)
sf0 ---> xs = [1; 2], it = ?, g ----> closure for g

Results and evaluations: (g n, [n -> 0])... results in [1;2] and the binding of n -> 0 is hence no longer needed. The implementation releases
the memory used to implement this binding by popping the frame sf3 off the stack. The next evaluation would be: (g n, [n -> 1]) which will cons 1 to 
the previous list xs returned [1;2] resulting in [1;1;2] and then reversing it [2;1;1] so on the next step 2 would be cons to it resulting in
[2;2;1;1] and finally reversing it one last time to result in [1;1;2;2] which will be the final output: val it: int list = [1;1;2;2]
*)

(* Assignment 7.2, HR 9.3 *)

let rec sum(n,m) = 
    let rec aux n acc =
        if n > 0 then aux (n-1) (acc+m+n) else m + acc
    aux n 0


(* Example *)
sum(10,10)

(* Assignment 7.3, HR 9.4 *)
let length xs =
    let mutable count = 0
    for _ in xs do
        count <- count + 1
    count

(* let length2 xs =
    List.fold (fun acc _ -> acc + 1) 0 xs *)

(* Example *)
length [1]

(* Assignment 7.4, HR 9.6 *)
let rec facC n c =
    if n = 0 then c 1
    else facC (n - 1) (fun res -> c (n * res))

(* > facC 5 id;;
Real: 00:00:00.002, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 120 *)

let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1,n*m)
(* > factA (5,1);;
Real: 00:00:00.002, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it: int = 120 *)

  
(* Assignment 7.5, HR 8.6 *)
let fib n =
    if n <= 1 then n
    else
        let mutable fib_n_minus_2 = 0
        let mutable fib_n_minus_1 = 1
        let mutable fib_n = 1
        let mutable i = 2
        while i <= n do
            fib_n <- fib_n_minus_1 + fib_n_minus_2
            fib_n_minus_2 <- fib_n_minus_1
            fib_n_minus_1 <- fib_n
            i <- i + 1
        fib_n

(* Example *)
fib 4;;

(* Assignment 7.6, HR 9.7 *)
//First checks if the input 'n' is less than or equal to 1, in which case it returns 'n2' as the nth fibonacci number. Otherwise, it recursively
//calls itself with 'n-1' as the new input, and the current value of 'n2' and the sum of 'n1' and 'n2' as the new values of 'n1' and 'n2'.

let rec fibA n n1 n2 =
    if n = 0 then n1
    elif n = 1 then n2
    else fibA (n - 1) (n2) (n1 + n2)


(* Example *)
fibA 10 0 1;;
(* Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val fibC: n: int -> c: (int -> 'a) -> 'a
val it: int = 55 *)

let rec fibC n c =
    if n <= 1 then c n
    else fibC (n - 1) (fun fib_n_minus_1 -> 
        fibC (n - 2) (fun fib_n_minus_2 ->
            c (fib_n_minus_1 + fib_n_minus_2)))

(* Example *)
fibC 10 id
(* Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val fibA: n: int -> n1: int -> n2: int -> int
val it: int = 55 *)