module a2

// Exercise 4.1 - explode - string to char list
let explode (s:string) = s.ToCharArray() |> List.ofArray
let rec explode2 (s:string) = 
    match s with
    | "" -> []
    | s -> s.Chars(0) :: (explode2 (s.Remove(0, 1)))

// Exercise 4.2 - implode - char list to string
let implode (list: char list) = List.foldBack (fun x acc -> string x + acc) list ""

let implodeRev (list: char list) = List.fold (fun acc x -> string x + acc) "" list

// Exercise 4.3 - toUpper
let toUpper (s:string) = implode (List.map System.Char.ToUpper (explode s))

// explode >> map >> implode
let toUpper1 (s:string) = s |> (explode >> List.map System.Char.ToUpper >> implode)
// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;

let toUpper2 (s: string) = s |> (implode << List.map System.Char.ToUpper << explode)

// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
let palindrome (s: string) =
    let caseInsensitive = s.ToLower()
    caseInsensitive = new string(Array.rev(caseInsensitive.ToCharArray()))

// Exercise 4.5 - ack
// ack(3, 11) = 16381
let rec ack (m, n) = 
    match (m, n) with
    | (0, _) -> n + 1
    | (m, n) when m > 0 && n = 0 -> ack(m - 1, 1)
    | (m, n) when m > 0 && n > 0 -> ack(m - 1, ack(m, n - 1))
    | (_,_) -> failwith "input is negative number"
    

// Exercise 4.6 - time
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)


let timeArg1 f a =
    let start = System.DateTime.Now
    let res = f a
    let finish = System.DateTime.Now
    (res, finish - start)

let result = timeArg1 (fun x -> ack x) (3, 11)

// Exercise 4.7 - HR 5.4 - downTo2 f n e
let rec downto1 f n e =
    match n with
    | n when n > 1 -> f n (downto1 f (n - 1) e)
    | 1 -> f n e
    | _ -> e

// factorial function using downto1 for recursion.
let fact n = downto1 (fun x acc -> x * acc) n 1

//Uses a recurive helper function to build the list. Accumulates the results from n to 1. 
//By applying fact 5 to buildList g n (buildList fact 5), we get a list of all the factorial results from 1 to 5 = [1; 2; 6; 24; 120].
let buildList g n =
  let rec buildListHelper g n acc =
    if n < 1 then acc
    else buildListHelper g (n-1) ((g n) :: acc)
  in buildListHelper g n []

