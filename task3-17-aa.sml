(*created 2021dec23*)

fun f3 (a1 : (int * real * int) list, a2 : real * int list)
       : real list * int =
  let
    val x = hd (tl (#2 a2))
    val xPlus7 = x + 7
    val x2Plus5 = x * x + 5
    val y3Minus3 = let val y = #3 (hd (tl (tl a1))) in y * y * y - 3 end
    val xPlus72 = xPlus7 * xPlus7
    val xy = real (x2Plus5 * x2Plus5 - y3Minus3 * y3Minus3)
  in
    ( [xy / real xPlus7 + real xPlus72 / real x2Plus5 + real y3Minus3, xy]
    , xPlus72 + y3Minus3
    )
  end

val test1 = f3 ( [(7, 9.5, 8), (5, 3.4, 6), (7, 8.8, 4), (1, 2.5, 3)]
               , (4.5, [3, 4, 5] )
               )
val test2 = f3 ( [(1, 4.5, 5), (2, 5.4, 3), (7, 3.8, 4), (1, 1.5, 3)]
               , (3.5, [6, 7, 8] )
               )
val test3 = f3 ( [(2, 3.5, 6), (1, 8.4, 2), (7, 4.8, 4), (7, 2.5, 4)]
               , (5.5, [9, 1, 2] )
               )
