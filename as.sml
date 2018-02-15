
fun fold f base [] = base
| fold f base (x::rest) = f x (fold f base rest);

fun map f [] = []
| map f (x::rest) = (f x)::(map f rest);

fun deepSum L =
  let
    fun add x y = x + y
  in
    fold add 0 (map (fold add 0) L)
  end


  fun deepSum L =
    let
      fun add (NONE) (NONE) = (NONE)
      | add (SOME(x)) (NONE) = (SOME(x))
      | add (SOME(x)) (SOME(y))= (SOME(x+y))
      | add (NONE) (SOME(y)) = (SOME(y))
    in
      fold add NONE (map (fold add NONE) L)
    end

fun unzip [] =[]
| unzip l1 =
  let
    fun f1 (x,y) = x;
    fun f2 (x,y) = y;
  in
    [map f1 l1,map f2 l1]
  end

  datatype either = ImAString of string | ImAnInt of int

  datatype eitherTree = eLEAF of either | eINTERIOR of (either*eitherTree*eitherTree)



  fun eitherSearch (eLEAF(ImAnInt x)) y = (x=y)
    | eitherSearch(eLEAF(ImAString x)) y = false
    | eitherSearch (eINTERIOR (x,l1,r1)) y = (eitherSearch l1 y)
    orelse (eitherSearch r1 y);

   fun eitherTest () =
    let
      val t1 = eINTERIOR((ImAString "one"),(eLEAF(ImAnInt 8)),(eLEAF(ImAnInt 8)));
      val t2 = eINTERIOR((ImAString "two"),(eLEAF(ImAnInt 5)),(eLEAF(ImAnInt 3)));
      val t3 = eINTERIOR((ImAString "three"),(eLEAF(ImAnInt 4)),(eLEAF(ImAnInt 20)));
      val t4 = eINTERIOR((ImAString "yo"),t3,t2);
      val root = eINTERIOR((ImAString "root"),t1,t4);
    in
      eitherSearch root 8
    end

      val lol = eitherTest()


      datatype 'a Tree = LEAF of 'a | NODE of ('a Tree) * ('a Tree)

      datatype 'a myTree = myLEAF of 'a | myNODE of 'a*'a*('a myTree)*('a myTree)

      fun findMin (NODE(l,r)) = if (findMin l < findMin r ) then (findMin l) else (findMin r)
        | findMin(LEAF(x)) = x

      fun findMax (NODE(l,r)) = if (findMax l > findMax r ) then (findMax l) else (findMax r)
        | findMax(LEAF(x)) = x




        fun minMaxTree (NODE(l,r))  = (myNODE(findMin (NODE(l,r)) ,findMax (NODE(l,r)), minMaxTree l,minMaxTree r))
          | minMaxTree (LEAF x) =  (myLEAF x)





val L1 = LEAF(1)
val L2 = LEAF(2)
val L3 = LEAF(3)
val N1 = NODE(L1,L2);
val N2 = NODE(N1,L1);
val N3 = NODE(N1,N2);
val t1 = NODE(N2,N3);

minMaxTree t1
