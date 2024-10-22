exception Negative_Amount

let change amount =
  if amount < 0 then
    raise Negative_Amount
  else
    let denominations = [25; 10; 5; 1] in
    let rec aux remaining denominations =
      match denominations with
      | [] -> []
      | d :: ds -> (remaining / d) :: aux (remaining mod d) ds
    in
    aux amount denominations

let first_then_apply (test_list: 'a list)
                     (predicate: 'a -> bool)
                     (function_to_apply: 'a -> 'b option)
                     : 'b option = 
  match List.find_opt predicate test_list with
  | Some value -> (function_to_apply value)
  | None -> None

let powers_generator base =
  let rec generate_from power () =
    Seq.Cons (power, generate_from (power * base))
  in
  generate_from 1

let meaningful_line_count file_name = 
  let meaningful_line line =
    let trimmed = String.trim line in
    String.length trimmed > 0 && not (String.starts_with trimmed ~prefix:"#")
  in
  let file = open_in file_name in
  let finally () = close_in file in
  let rec count_lines count =
    try
      let line = input_line file in
      if meaningful_line line then
        count_lines (count + 1)
      else
        count_lines count
    with
    | End_of_file -> count
    in
    Fun.protect ~finally (fun () -> count_lines 0)

type shape =
  | Sphere of float
  | Box of float * float * float

let volume shape = 
  match shape with
  | Sphere radius -> Float.pi *. (radius ** 3.) *. 4. /. 3.
  | Box (length, width, height) -> length *. width *. height

let surface_area shape = 
  match shape with
  | Sphere radius -> 4. *. Float.pi *. (radius ** 2.)
  | Box (length, width, height) -> 
    2. *. (width *. length +. height *. length +. height *. width)

type 'a binary_search_tree =
  | Empty
  | Node of 'a binary_search_tree * 'a * 'a binary_search_tree

let rec insert value tree =
  match tree with
  | Empty -> Node (Empty, value, Empty)
  | Node (left, old_value, right) ->
    if value < old_value then
      Node (insert value left, old_value, right)
    else if value > old_value then
      Node (left, old_value, insert value right)
    else
      tree

let rec size tree =
  match tree with
  | Empty -> 0
  | Node (left, _, right) -> 1 + size left + size right

let rec contains value tree =
  match tree with
  | Empty -> false
  | Node (left, old_value, right) ->
    if value = old_value then
      true
    else if value < old_value then
      contains value left
    else
      contains value right

let rec inorder tree =
  match tree with
  | Empty -> []
  | Node (left, old_value, right) -> inorder left @ [old_value] @ inorder right
