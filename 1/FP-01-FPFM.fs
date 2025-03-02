module a1 =
    // 1.1 Computes the square of a number n by multiplying n by itself 
    let sqr n = n * n
    // 1.2 Computes the power of a number a, b number of times. It multiplies the base a, b number of times. For instance: 3**3 = 27
    let pow a b = System.Math.Pow(a, b)

    // 1.3 / HR 1.1 Computes an addition to n, where n can be assigned any integer. The expression adds 4 to n.
    let g n = n + 4

    // 1.4 / HR 1.2 Takes 2 float numbers and it squares them both by 2.0 for both x and y. It then adds them together and calculates
    // its squareroot
    let h(x:float, y:float) = sqrt(x**2.0) + (y**2.0)

    // 1.5 / HR 1.4 Defines a recursive function f. It sums n, n number of times. 
    // The function is composed by 2 clauses. The first clause, which is the stop clause, determines when the function should stop
    // if the condition is met. That is, n = 0. Otherwise, perform clause 2 which computes the sum of n in a recursive manner until clause 1 is met.
    let rec f = function
        | 0 -> 0
        | n -> n + f(n-1)
    f 4;; //Evaluation for f 4: 4 + f(4-1) -> 4 + f(3) -> 4 + (3 + f(3-1)) -> 4 + (3 + f(2)) -> 4 + (3 + (2 + f(2-1))) -> 4 + (3 + (2 + (1 + f(1-1)))) ->
    // -> 4 + (3 + (2 + (1 + 0))) = 10 // Final result: val it: int = 10

    // 1.6 / HR 1.5 Defines a recursive function fib, which computes the fibonacci sequence. The function has 3 clauses.
    // The first clause, which is the stop clause, determines what happens when n = 0 and it also stops the loop.
    // The second clause determines the outcome when n = 1.
    //The third clause determines the recursive outcome when n >= 2.
    let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | n -> fib(n-1) + fib(n-2)
    fib 4;; 
    //Evaluation for fib 4:
    // fib 4 =
    // f(4-1) + f(4-2) =
    // (f(3-1)) + f(3-2)) + (f(2-1) + f(2-2)) =
    // (((f(2-1) + f(2-2)) + f(1))) + (f(1) + f(0)) =
    // (((f(1) + f(0)) + f(1)) + (f(1) + f(0)) =
    // ((1 + 0) + 1)) + (1 + 0) = 
    // 2 + 1 = 3
    //The original call to fib 4 is computed as fib 3 + fib 2 = 2 + 1 = 3
    //Final result: val it: int = 3

    // 1.7 / HR 1.6 Defines a recursive sum function, which computes an addition of the form m + n, n times. 
    //The first clause, which is the stop clause, determines when the loop should stop, that is when n = 0, giving m as output and adding it to the rest of the output as a final result.
    // The second clause performs a recursive sum of m + n, n number of times until n = 0 by applying the same function recursively sum(m, n-1)
    let rec sum = function
        | (m,0) -> m
        | (m,n) -> sum(m, n-1) + (m + n)

    // 1.8 / HR 1.7
   (*  (System.MATH.PI, fact -1) -----> float * int //tuple of (float, int) but fact -1 would evaluate in infinite evaluation/non-terminating evaluation
    fact (fact 4) -----> int
    power(System.MATH.PI, fact 2) ----> float
    (power, fact) ----> (float * int -> float) * (int -> int) *)

    // 1.9 / HR 1.8
    let a = 5 // a -> 5
    let f a = a + 1  // f a takes an int as an argument and then performs the calculation a + 1
    let g b = (f b) + a // g takes an int as an argument and then performs the calculation
    // by calling the previous function f a, but now with a different name for the argument
    // so the result of applying f b = b + 1, would then be applied to + a. a is originally declared as a = 5
    // let g b = (f b = b + 1) + a // a will always be 5 in this scenario
    f 3;; // Evaluation for f 3:
    // f 3 = 3 + 1 = 4 
    //Final result: val it: int = 4
    g 3;; //Evaluation for g 3 :
    // g 3 = (f 3 = 3 + 1) + 5 -> 4 + 5 = 9
    //Final result: val it: int = 9

    //env1 = a -> 5, f -> the f function, g -> the g function // The environment is the overall result of assigning each variable a final value after computing it (if necessary)
    // 

    // 1.10 Duplicate strings: dup:string -> string
    // Duplicates the string by concatenating itself one time.
    let dup a:string = a + a

    // 1.11 Duplicate string n times.
    //Duplicates a string n times by performing an if-else statement. 
    //If the condition is (n = 0) then an empty string is given as output and text: string is resolved to its previous concatenation n number of times. Otherwise it concatenates the text by itself n times until n = 0.
    let rec dupn (text:string) n = 
        if n = 0 then "" else text + dupn text (n-1);;
    

(*     let rec dupn2 text n = 
        match text, n with
        | (text, 0) -> ""
        | (text, n) -> text + dupn2 text (n-1)
    
    dupn2 "haha" 3 *)
    

