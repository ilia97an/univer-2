fun f5 (x : real, n : int) : real =
  let
    val realn = real n
    fun f5Rekur ( a : real, sum : real, slagA : real, slagB : real, znak : real
                , xStep : real ) : real =
      let
        val ainc = a + 1.0
      in
        if a > realn then
          sum
        else
          f5Rekur ( ainc, sum - (slagA - znak * slagB) / ainc * xStep
                  , slagA * 5.0, slagB * 4.0, znak * ~1.0, xStep * x )
      end
  in
    f5Rekur (0.0, 0.0, 5.0, 4.0, 1.0, 1.0)
  end

fun f5Test (x : real) : real = Math.ln (1.0 - x - 20.0 * Math.pow (x, 2.0))

val test11 = f5 (0.1, 10)
val test12 = f5Test 0.1

val test21 = f5 (2.0, 3)
val test22 = f5Test 2.0

val test31 = f5 (2.0, 2)
val test32 = f5Test 2.0

val test41 = f5 (0.1, 100)
val test42 = f5Test 0.1
