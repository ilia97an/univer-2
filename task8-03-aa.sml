(*created 2021dec22*)
signature COMPLEX = sig
  (* тип данных *)
  type complex
  (* конструктор *)
  val complRect : real * real -> complex
  val complPolar : real * real -> complex
  val re : complex -> real
  val im : complex -> real
  val magnitude : complex -> real
  val angle : complex -> real
  val toString : complex -> string
  val isEq : complex * complex -> bool
  val add : complex * complex -> complex
  val mul : complex * complex -> complex
  val negate : complex -> complex
  val abs : complex -> complex
  val sign : complex -> complex
  val sub : complex * complex -> complex
  val fromReal : real -> complex
  val divide : complex * complex -> complex
  val recip : complex -> complex
end

structure Complex :> COMPLEX = struct

  datatype complex = complRect of real * real
                   | complPolar of real * real
  
  fun toString (complRect (x, y)) = 
        "(" ^ Real.toString x ^ " + "
            ^ Real.toString y ^ "i)"
    | toString (complPolar (r, A)) =
        "(" ^ Real.toString r ^ " * e ^ {i * "
            ^ Real.toString A ^ "})"
  fun re (complRect (x, y)) = x
    | re (complPolar (r, A)) = r * Math.cos A
  fun im (complRect (x, y)) = y
    | im (complPolar (r, A)) = r * Math.sin A
  
  fun magnitude (complRect (x, y)) = Math.sqrt (x * x + y * y)
    | magnitude (complPolar (r, A)) = r
  fun angle (complRect (x, y)) = Math.atan (y / x)
    | angle (complPolar (r, A)) = A
  fun isEq (compl1, compl2) = 
    let val (re1, im1) = (re compl1, im compl1)
        val (re2, im2) = (re compl2, im compl2)
    in re1 >= re2 andalso re1 <= re2 andalso im1 >= im2 andalso im1 <= im2
    end
  fun add (compl1, compl2) =
    complRect (re compl1 + re compl2, im compl1 + im compl2)
  fun mul (c1, c2) =
    complPolar (magnitude c1 * magnitude c2, angle c1 + angle c2)
  fun negate c = complRect (~1.0 * re c, ~1.0 * im c)
  fun abs c = complPolar (Real.abs (magnitude c), 0.0)
  fun sign c =
    complRect ( Real.fromInt (Real.sign (re c))
              , Real.fromInt (Real.sign (im c)) )
  fun sub (compl1, compl2) = add (compl1, negate compl2)
  fun fromReal r = complRect (r, 0.0)
  fun divide (c1, c2) = 
    complPolar (magnitude c1 / magnitude c2, angle c1 - angle c2)
  fun recip c = divide (fromReal 1.0, c)
end

type complex = Complex.complex
val complRect = Complex.complRect
val complPolar = Complex.complPolar

(*тесты*)
val compl1 = complRect (1.0, 2.0)
val compl2 = complPolar (2.0, 3.0)
val sintest1 = Math.sin (3.1415 / 2.0)
val sintest2 = Math.sin 3.5
val sintest3 = Math.sin 4.6
val sintest4 = Math.sin 3.1415
val sintest5 = Math.sin (3.1415 / 6.0)
val sintest6 = Math.sin ((7.0 / 6.0) * 3.1415)
(* x = r cos A
   y = r sin A *)
val compl3 = complRect (0.0, 3.5)
val compl4 = complRect (~1.3, 4.0)
val compl5 = complPolar (2.4, ~5.2) (* sin (6.3 - 5.2 = 1.1) > 0, cos > 0 *)
val compl6 = complPolar (2.3, ~1.2) (* sin < 0, cos > 0 *)
val compl7 = complRect (0.0, ~3.5)


val compl1toString = Complex.toString compl1
val compl2toString = Complex.toString compl2
val compl3toString = Complex.toString compl3
val compl4toString = Complex.toString compl4
val compl5toString = Complex.toString compl5
val compl6toString = Complex.toString compl6
val compl7toString = Complex.toString compl7
val compl1re = Complex.re compl1
val compl2re = Complex.re compl2
val compl3re = Complex.re compl3
val compl4re = Complex.re compl4
val compl5re = Complex.re compl5
val compl6re = Complex.re compl6
val compl1im = Complex.im compl1
val compl2im = Complex.im compl2
val compl3im = Complex.im compl3
val compl4im = Complex.im compl4
val compl5im = Complex.im compl5
val compl6im = Complex.im compl6
val compl1mag = Complex.magnitude compl1
val compl2mag = Complex.magnitude compl2
val compl3mag = Complex.magnitude compl3
val compl4mag = Complex.magnitude compl4
val compl5mag = Complex.magnitude compl5
val compl6mag = Complex.magnitude compl6
val compl1ang = Complex.angle compl1
val compl2ang = Complex.angle compl2
val compl3ang = Complex.angle compl3
val compl4ang = Complex.angle compl4
val compl5ang = Complex.angle compl5
val compl6ang = Complex.angle compl6
val isEqcompl1compl1 = Complex.isEq (compl1, compl1)
val isEqcompl5compl5 = Complex.isEq (compl5, compl5)
val isEqcompl1compl2 = Complex.isEq (compl1, compl2)
val isEqcompl2compl1 = Complex.isEq (compl2, compl1)
val isEqcompl1compl5 = Complex.isEq (compl1, compl5)
val isEqcompl5compl1 = Complex.isEq (compl5, compl1)
val addcompl1compl2 = Complex.toString (Complex.add (compl1, compl2))
val addcompl3compl4 = Complex.toString (Complex.add (compl3, compl4))
val addcompl5compl6 = Complex.toString (Complex.add (compl5, compl6))
val mulcompl1compl2 = Complex.toString (Complex.mul (compl1, compl2))
val mulcompl3compl4 = Complex.toString (Complex.mul (compl3, compl4))
val mulcompl5compl6 = Complex.toString (Complex.mul (compl5, compl6))
val compl1neg = Complex.toString (Complex.negate compl1)
val compl2neg = Complex.toString (Complex.negate compl2)
val compl3neg = Complex.toString (Complex.negate compl3)
val compl4neg = Complex.toString (Complex.negate compl4)
val compl5neg = Complex.toString (Complex.negate compl5)
val compl6neg = Complex.toString (Complex.negate compl6)
val compl1abs = Complex.toString (Complex.abs compl1)
val compl2abs = Complex.toString (Complex.abs compl2)
val compl3abs = Complex.toString (Complex.abs compl3)
val compl4abs = Complex.toString (Complex.abs compl4)
val compl5abs = Complex.toString (Complex.abs compl5)
val compl6abs = Complex.toString (Complex.abs compl6)
val compl1sign = Complex.toString (Complex.sign compl1)
val compl2sign = Complex.toString (Complex.sign compl2)
val compl3sign = Complex.toString (Complex.sign compl3)
val compl4sign = Complex.toString (Complex.sign compl4)
val compl5sign = Complex.toString (Complex.sign compl5)
val compl6sign = Complex.toString (Complex.sign compl6)
val subcompl1compl2 = Complex.toString (Complex.sub (compl1, compl2))
val subcompl3compl4 = Complex.toString (Complex.sub (compl3, compl4))
val subcompl5compl6 = Complex.toString (Complex.sub (compl5, compl6))
val fromReal__13 = Complex.toString (Complex.fromReal 1.3)
val fromReal__00 = Complex.toString (Complex.fromReal 0.0)
val fromReal_m12 = Complex.toString (Complex.fromReal ~1.2)
val divcompl1compl2 = Complex.toString (Complex.divide (compl1, compl2))
val divcompl3compl4 = Complex.toString (Complex.divide (compl3, compl4))
val divcompl5compl6 = Complex.toString (Complex.divide (compl5, compl6))
val compl1recip = Complex.toString (Complex.recip compl1)
val compl2recip = Complex.toString (Complex.recip compl2)
val compl3recip = Complex.toString (Complex.recip compl3)
val compl4recip = Complex.toString (Complex.recip compl4)
val compl5recip = Complex.toString (Complex.recip compl5)
val compl6recip = Complex.toString (Complex.recip compl6)

(*
  val re : complex -> real
  val im : complex -> real
  val magnitude : complex -> real
  val angle : complex -> real
  val toString : complex -> string
  val isEq : complex * complex -> bool
  val add : complex * complex -> complex
  val mul : complex * complex -> complex
  val negate : complex -> complex
  val abs : complex -> complex
  val sign : complex -> complex
  val sub : complex * complex -> complex
  val fromReal : real -> complex
  val divide : complex * complex -> complex
  val recip : complex -> complex
*)
