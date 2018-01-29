
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

fun removeDuplicates([]) = []
| removeDuplicates (x::y) =
  if inList(x,y) = false
  then x::removeDuplicates(y)
  else removeDuplicates(y);


fun intersection ([],[]) = []
    | intersection([],l) = []
    | intersection(l,[]) = []
    |intersection((x::xs),l) = if inList(x,l) = true then removeDuplicates(x::intersection(xs,l))
        else removeDuplicates(intersection(xs,l));
   
fun range (min, step, max) =
    if min >= max andalso step >= 0 then []
    else if min <= max andalso step <= 0 then[]
    else 
    min::range((min+step),step,max);
    
    
    
(*
fun numbersToSum (x:int ,[]:int list) = []
    | numbersToSum(x,[])= []
    | numbersToSum([],z) = []
    | numbersToSum(x,y::ys) = 
        if (x < 0) then []
        else if (x = 0) then []
        else y::numbersToSum((x-y),ys);
*)

fun replace (n, v, []) = []
    | replace(n,v,x::xs) = 
        if n - 1 = 0 
         then v::replace(n,v,xs)
        else x::replace(n,v,xs);
