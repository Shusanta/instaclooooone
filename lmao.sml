fun inList (item, []) = false
  | inList (item, (itemList::curItem)) =
      if item = itemList then true
      else inList (item, curItem);


(*
2. removeDuplicates - 10%
Write a function removeDuplicates that takes a list as input and removes the duplicate values from
the list. Each value should appear in the output list only once but the order does not matter. The
function should have type ''a list -> ''a list.
*)

fun removeDuplicates(value, []) = []
| removeDuplicates (value, x::y) =
  if inList(value,y) = false
  then value::removeDuplicates(value,y)
  else removeDuplicates(value,y);
