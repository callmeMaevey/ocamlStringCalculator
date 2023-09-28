
(* Trying to make a calculator that uses strings*)


let getStringHead s = 
  let open String in
  if length s > 0 
  then make 1 @@ get s 0
  else empty

let getStringTail s =
  let open String in
  if (length s) > 1
  then sub s 1 @@ len-1


let rec charList_of_string s =
  let len  = String.length s in 
  match len with
  | 0 -> [  ]
  | 1 -> [ getStringHead s ]
  | _ -> (getStringHead s)::(charList_of_string @@ getStringTail s)


let rec printcharlist = function
  | [] -> ()
  | h::t ->
      print_char h;
      print_char ' ';
      printCharList t

let rec reverseString s =
  let open String in 
  match (length s) with
  | 0 -> empty
  | 1 -> head
  | _ -> (reverseString @@ getStringTailtail s)^(getStringHead s)

let addStrings a b =
    let add x y =
      if (String.length x = 0)
      and (String.length y = 0)
      then empty
      else
        let hx = int_of_string @@ getStringHead x in
        let hy = int_of_string @@ getStringHead y in
      
        let tx = String.sub x 1 @@ (String.length x)-1 in
        let ty = String.sub x 1 @@ (String.length x)-1 in      
        let hz = hx + hy;
        (string_of_int @@ hz-10) ^ (add tx ty)
    in
    let a' = reverseString a in 
    let b' = reverseString b in
    add a' b'


let addCharLists a b =
  let intOfChar = function 
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    |  _  -> print_string "Error Converting digit to int"; 0
  in
  let rec add a b = 
    match a,b with
    | (ha::ta),(hb::tb) -> 
        let head = add [ha],[hb] in
        let tail = add ta tb in
        tail :: head
    | [a],[b] ->
        let a' = intOfChar a in
        let b' = intOfChar b in
        let c' = a' + b' in
        let c  = string_of_int z' in
        match z with
        | String.length z > 1 ->
            (*todo: pickup here sweetpea*)
    | [],[] -> []
  in 
  add a b

let () =
  let charlist = charList_of_string "777" in
  print charlist


