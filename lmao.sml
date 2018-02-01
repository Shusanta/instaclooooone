
(*Shusanta Bhattarai 11474714*)
(*Help/idea contribution stated below*)

(*1*)

fun inList (item, []) = false
  | inList (item, (itemList::curItem)) =
      if item = itemList then true
      else inList (item, curItem);

(* Test Functions*)

inList(1, []);

inList(1, [1,2,3]);

inList([1], [[3],[5]]);


(*2*)
fun removeDuplicates([]) = []
| removeDuplicates (x::y) =
  if inList(x,y) = false
    then x::removeDuplicates(y)
  else removeDuplicates(y);

  (* Test Functions*)

  removeDuplicates ([1, 5, 1, 3, 4, 3, 5]);

  removeDuplicates([1,2,2,2,2,2,2,2]);

  removeDuplicates([1,2,3,4,4,4,5,5,7,8,5]);


(*3*)
fun listIntersect ([],[]) = []
    | listIntersect([],l) = []
    | listIntersect(l,[]) = []
    |listIntersect((x::xs),l) =
      if inList(x,l) = true
        then removeDuplicates(x::listIntersect(xs,l))
      else removeDuplicates(listIntersect(xs,l));


 (* Test Functions*)

 listIntersect ([1],[1]);

 listIntersect ([1,2,3],[1,1,2]);

 listIntersect ([[2,3],[1,2],[2,3]],[[1],[2,3]]);


(*4*)
fun range min step max = if max = min then []
  else if min > max andalso step > 0 then []
  else if min < max andalso step > 0 orelse min > max andalso step < 0
  then min::(range(min+step) step max)
  else range(min+step) step max;

 (*Test Functions*)

 range 0 5 30;

 range 5 ~1 0;

 range 1 2 10;

(*5*)
(*Help from Elliot*)

fun numbersToSum x [] = []
  | numbersToSum x (y::yz) =
      if x > y then y::(numbersToSum(x-y) yz)
      else numbersToSum(x-y)yz;

       (*Test Functions*)
       numbersToSum 100 [10, 20, 30, 40];

       numbersToSum 30 [5, 4, 6, 10, 4, 2, 1, 5];

       numbersToSum 1 [2];



(*6*)
fun replace n v [] = []
  | replace n v (x::xs) =
  if n = 0
    then v::(replace (n-1) v xs)
  else
    x::replace(n-1) v xs;


  (*Test Functions*)
  replace 3 40 [1, 2, 3, 4, 5, 6];
  replace 0 "X" ["a", "b", "c", "d"];
  replace 4 false [true, false, true, true, true];




(*7*)
(*Help From Edgar Perez*)

(*reverses*)
fun revhelper [] = []
  | revhelper (x::xz) = revhelper (xz)@[x]

fun groupLeft v [] p = [p]
    | groupLeft v (x::xz) p = if (length p) = v
        then p::(groupLeft v xz [x])
        else groupLeft v xz ([x]@p)

fun groupNLeft v [] = [[]]
    | groupNLeft v x = revhelper(groupLeft v (revhelper x) [])


fun groupRight v [] p = [p]
  | groupRight v (x::xz) p = if length p = v
      then p::(groupRight v xz [x])
      else groupRight v xz (p@[x])

fun groupNRight v [] = [[]]
  | groupNRight v x = groupRight v [] x;


    (*test functions*)

 groupNLeft 2 [1, 2, 3, 4, 5];

 groupNRight 2 [1, 2, 3, 4, 5];

 groupNLeft 3 [1, 2, 3, 4, 5];

 groupNRight 3 [1, 2, 3, 4, 5];
