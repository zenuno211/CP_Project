module BTree

open Cp

// (1) Datatype definition -----------------------------------------------------

type BTree<'a> = Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inBTree x = either (konst Empty) Node x

let outBTree x =
    match x with
    | Empty -> Left ()
    | Node (a, (t1,t2)) -> Right (a, (t1,t2))

// (2) Ana + cata + hylo -------------------------------------------------------

let baseBTree f g = id -|- (f >< (g >< g))

let recBTree g x = baseBTree id g x

let rec cataBTree g x = (g << (recBTree (cataBTree g)) << outBTree) x
   
let rec anaBTree g x = (inBTree << (recBTree (anaBTree g)) << g) x

let hyloBTree h g x = (cataBTree h << anaBTree g) x

// (3) Map ---------------------------------------------------------------------

let fmap f x = cataBTree ( inBTree << baseBTree f id ) x

// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inBTree << (id -|- (id >< swap))) x

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) x

// (4.3) Serialization ---------------------------------------------------------

let inord x = 
    let join(x,(l,r)) = l @ [x] @ r
    in either nil join x

let inordt x = cataBTree inord x

let preord x = 
    let f(x,(l,r)) = x :: l @ r
    in (either nil f) x

let preordt x = cataBTree preord x

let postordt x = 
    let f(x,(l,r)) = l @ r @ [x]
    in cataBTree (either nil f) x

// (4.4) Quicksort -------------------------------------------------------------

let rec part p l = 
     match l with
      | [] -> ([],[])
      | h::t -> if p > h then let (s,l) = part p t in (h::s,l)
                else let (s,l) = part p t in (s,h::l)
                
let qsep l =
    match l with
    | [] -> Left ()
    | (h::t) -> let (s,l) = part h t
                in Right (h,(s,l))

let qSort x = (hyloBTree inord qsep) x



// (4.5) Traces ----------------------------------------------------------------

let union left right =
    List.append left right |> Seq.distinct |> List.ofSeq

let func a l = a:: l

let tunion(a,(l,r)) = union (List.map (func a) l) (List.map (func a) r) 

let traces x = cataBTree (either (konst [[]]) tunion) x


// (4.6) Towers of Hanoi -------------------------------------------------------

let present x = inord x // same as in qSort

let strategy x =
    match x with
    | (d,0) -> Left()
    | (d,n) -> Right ((n-1,d),((not d,n-1),(not d,n-1)))

let hanoi x = hyloBTree present strategy x

// (5) Depth and balancing (using mutual recursion) --------------------------

let baldepth x = let f((b1,d1),(b2,d2)) = ((b1,b2),(d1,d2))
                 let h(a,((b1,b2),(d1,d2))) = (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)
                 let g = either (konst(true,0)) (h << (id><f))
                 in cataBTree g x

let depthBTree x = (p2 << baldepth) x

let balBTree x = (p1 << baldepth) x