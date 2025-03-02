module a2
// Exercise 3.1 downTo + downTo2
let rec downTo n =
    if n <= 0 then
        []
    else 
        n :: downTo (n - 1)

let rec downTo2 n = 
    match n with 
    | 0 -> []
    | n' -> n' :: downTo2 (n' - 1)

// Exercise 3.2 removeOddIdx
let rec removeOddIdx xs =
    match xs with
    | [] -> xs
    | [_] -> xs
    | x0 :: _ :: xs' -> x0 :: removeOddIdx xs'

// Exercise 3.3 combinePair
let rec combinePair (xs: int list) = 
    match xs with
    | x0 :: x1 :: xs -> (x0, x1) :: combinePair xs
    | _ -> []

(* let rec combinePair2 (xs: int list) = 
    match xs with
    | [] -> []
    | [_] -> []
    | x0 :: x1 :: xs' ->
        match xs' with
        | [] -> [(x0, x1)]
        | _ -> (x0, x1) :: combinePair2 xs' *)

// Exercise 3.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) (pound1, shilling1, pence1) (pound2, shilling2, pence2) =
    let total_pence = pence1 + pence2
    let carry_shilling, remaining_pence = total_pence / 12, total_pence % 12
    let total_shilling = shilling1 + shilling2 + carry_shilling
    let carry_pound, remaining_shilling = total_shilling / 20, total_shilling % 20
    let total_pound = pound1 + pound2 + carry_pound
    (total_pound, remaining_shilling, remaining_pence)

// Money tuple subtraction
let (^-^) (pound1, shilling1, pence1) (pound2, shilling2, pence2) =
    let total_pence = pence1 - pence2
    let carry_shilling, remaining_pence = 
        if total_pence < 0 then (-1, total_pence + 12) else (0, total_pence)
    let total_shilling = shilling1 - shilling2 + carry_shilling
    let carry_pound, remaining_shilling = 
        if total_shilling < 0 then (-1, total_shilling + 20) else (0, total_shilling)
    let total_pound = pound1 - pound2 + carry_pound
    (total_pound, remaining_shilling, remaining_pence)

type Money = {pound : int; shilling : int; pence : int};;

// Money record addition
let (|+|) a b =
    let total_pence = a.pence + b.pence
    let carry_shilling, remaining_pence = total_pence / 12, total_pence % 12
    let total_shilling = a.shilling + b.shilling + carry_shilling
    let carry_pound, remaining_shilling = total_shilling / 20, total_shilling % 20
    let total_pound = a.pound + b.pound + carry_pound
    { pound = total_pound; shilling = remaining_shilling; pence = remaining_pence }
    
// Money record subtraction
let (|-|) a b =
    let total_pence = a.pence - b.pence
    let carry_shilling, remaining_pence = 
        if total_pence < 0 then (-1, total_pence + 12) else (0, total_pence)
    let total_shilling = a.shilling - b.shilling + carry_shilling
    let carry_pound, remaining_shilling = 
        if total_shilling < 0 then (-1, total_shilling + 20) else (0, total_shilling)
    let total_pound = a.pound - b.pound + carry_pound
    { pound = total_pound; shilling = remaining_shilling; pence = remaining_pence }


// Exercise 3.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d)

let ( .*) (a:float,b:float) (c:float,d:float) = (a * c - b * d, b * c + a * d)

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = (a - c, b - d)

let ( ./ ) (a,b) (c,d) =
    let denom = (c * c + d * d)
    ((a * c + b * d) / denom, (b * c - a * d) / denom)
//      3. Use 'let' expressions in division to avoid repeated evals
let (../) (a:float,b:float) (c:float,d:float) =
    let denom = (c * c + d * d)
    let real = (a * c + b * d)
    let img = (b * c - a * d)
    (real / denom, img / denom)


// Exercise 3.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
(* let rec altsum = function
| [] -> 0
| [x] -> x
| x0 :: x1 :: xs -> x0 - x1 + altsum xs *)

let rec altsum = function
    | [] -> 0
    | x :: xs -> match xs with
                | [] -> x
                | y :: ys -> x - y + altsum ys
