module a2

// 2.1 time difference: 
//Computer the time difference between two given times of the day.
//Multiplies the number of hours that has passed since midgnight by 60 and adds the minutes to obtain the total of minutes.
//Finally, it substracts the result of a, which is first time of the day
//to b which is the second time of the day to evaluate the final output.
let timediff (h0, m0) (h1, m1) = 
     let a = (h0 * 60) + m0
     let b = (h1 * 60) + m1
     b - a

let minutes2 (h, m) =
     timediff (00, 00) (h, m)

// 2.2 function minutes
// Function minutes takes 2 arguments, h = hours and m = minutes. It multiplies the number of hours since midnight by 60 to obtain the total amount of minutes
// and adds the remaining minutes to evaluate the final output. 
let minutes (h, m) = 
     let a = (h * 60) + m
     a



// 2.3 / HR 2.2
// The function pow takes a recursive approach. It takes 2 arguments, s and n. Matches parameter n with n' of type int to know how many times
// string s has to concatenate itself recursively to obtain the final result. Once n = 0, the first clause is triggered and it returns an empty string.
// Afterwards, the final output is returned with string s concatenated itself n number of times.
let rec pow (s,n) = 
     match n with
     | 0 -> ""
     | n' -> s + pow(s, n' - 1)
     

// 2.4 / HR 2.8 / a = row, b = element at index b in row a 
// The function bin takes a recursive approach. It takes a tuple of two integers (a, b) as input.
// The function recursively calcualtes the number of ways to choose b elements from the set of a elements using the binomial coefficient formula.
let rec bin (a,b) = 
     if b = 0 || b = a then
          1
     else 
          bin(a - 1, b - 1) + bin(a - 1, b)

// 2.5 / HR 2.9
let rec f = function
     | (0,y) -> y
     | (x,y) -> f(x-1, x*y)

//1. The type of f is: (int * int) -> int
//2. The evaluation terminates when x = 0 regardless of the value of the argument y (clause 1) after applying the function recursively (if necessary)
//3. f(2, 3) = val it: int = 6. The steps are as follows: First step (2, 3) triggers the second clause: (2, 3)  -> f(2-1, 2*3)
//Second step triggers the second clause again because 1 > 0, therefore (1, 6) -> f(1-1, 1*6)
//Third and last step triggers the first clause because x = 0, therefore yields the final result (0, 6) -> 6
//4. In mathematics, f (x, y) is a notation used to represent a function of two variables, x and y.
//The function f, maps the input values x and y to a single output value. The function is defined by a set of rules that
//determine the relationship between the inputs and the output. In this case, the function calls itself recursively and performs
//a calculation which yields an output that is fed as input for the recursive function until a condition is met, which in this case
//is when x = 0. This triggers the first clause, also known as the stopping clause and yields the final output. 


// 2.6 / HR 2.10
let test(c,e) = 
     if c then 
          e 
     else 
          0
//1. The type of test is: c: bool * e: int -> int
//2. The result is 0. e = fact (-1) is never evaluated because the function is resolved beforehand. 
//However, fact (-1) would output a non-terminating evaluation.
//3. This would trigger fact (-1) and it would output a non-terminating evaluation.

// 2.7 / HR 2.13 Curry and Uncurry

let curry (f: 'a * 'b -> 'c) : 'a -> 'b -> 'c =
  fun x -> fun y -> f (x,y)

let uncurry (g: 'a -> 'b -> 'c) : 'a * 'b -> 'c =
  fun (x,y) -> g x y
