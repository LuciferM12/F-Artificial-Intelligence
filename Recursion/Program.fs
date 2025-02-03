//Rec example
let rec append xs ys =
    match xs with
    | [] -> ys
    | x :: xs -> x :: append xs ys

let rec sum = function 
    | [] -> 0
    | x :: xs -> x + sum xs

let rec sep z xs =
    match xs with
    | [] -> []
    | [_] -> xs
    | x :: xs -> x :: z :: sep z xs

printf "sep: %A\n" (sep -1 [2])

let rec reverse xs =
    match xs with
    | [] -> []
    | x :: xs -> reverse xs @ [x]

printf "reverse: %A" (reverse [1;2;3])