
(* Trying to make a calculator that uses strings*)


let getStringHead s = 
  let open String in
  if length s > 0 
  then make 1 @@ get s 0
  else empty

let getStringTail s =
  let open String in
  if (length s) > 1
  then sub s 1 @@ (length s) -1
  else empty


let rec charList_of_string s =
  let len  = String.length s in 
  match len with
  | 0 -> [  ]
  | 1 -> [ getStringHead s ]
  | _ -> (getStringHead s)::(charList_of_string @@ getStringTail s)


let rec printCharList = function
  | [] -> ()
  | h::t ->
      print_char h;
      print_char ' ';
      printCharList t

let rec reverseString s =
  let open String in 
  match (length s) with
  | 0 -> empty
  | 1 -> getStringHead s
  | _ -> (reverseString @@ getStringTail s)^(getStringHead s)

let addStrings a b =
    let rec add x y =
      match x,y with
      | x,y when (String.length x = 0) -> String.empty
      | x,y when (String.length y = 0) -> String.empty
      | _ ->
        let hx = int_of_string @@ getStringHead x in
        let hy = int_of_string @@ getStringHead y in

        let tx = String.sub x 1 @@ (String.length x)-1 in
        let ty = String.sub y 1 @@ (String.length y)-1 in
        
        let hz = hx + hy in
        match hz with
        | i when i<10 ->  (string_of_int @@ hz) ^ (add tx ty)
        | i when i>9  -> (string_of_int @@ hz-10) ^ (add tx ty)
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
    | (ha::ta),(hb::tb) -> ( add [ha] [hb] ) :: ( add ta tb)
    | [a],[b] ->
        let a' = intOfChar a in
        let b' = intOfChar b in
        let c' = a' + b' in
        let c  = string_of_int z' in
        match z with
        | s when String.length s < 1 -> empty
            (*todo: pickup here sweetpea*)
    | [],[] -> []
  in 
  add a b

let () =
  let charlist = charList_of_string "777" in
  print charlist


