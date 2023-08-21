
module Rationallib = struct
  type r =
    { numerator: int
    ; denominator: int
    }
  type t =
    | Number of int
    | Rational of r

  let t_of_int (p : int) : t = Number p

  let times_const (c : int) (p : t) : r =
    { p with numerator = c * p.numerator }

  let times_const_t (c : int) (p : t) : t =
    match p with
    | Rational p -> { p with numerator = c * p.numerator }
    | Number p -> Number (c * p)

  let reciprocate (p : t) : r =
    match p with
    | Number p -> { numerator = 1 ; denominator = p }
    | Rational p -> { numerator = p.denominator ; denominator = p.numerator }

  let reciprocate_t : (t -> t) = fun p -> (Rational (reciprocate p))

  let times (p : t) (q : t) : t =
    match p, q with
    | Number p, Number q -> Rational { numerator = p * q ; denominator = 1 }
    | Rational p, Number q -> times_const_t q p
    | Number p, Rational q -> times_const_t p q
    | Rational p, Rational q ->
      Rational { numerator = p.numerator * q.numerator
               ; denominator = p.denominator * q.denominator }

  let div (p : t) (q : t) : r =
    match p, q with
    | Number p, Number q -> { numerator = p ; denominator = q }
    | Number p, Rational q -> times_const p (Rational (reciprocate q))
    | Rational p, Rational q ->
      { numerator = p.numerator * q.numerator
      ; denominator = p.denominator * q.denominator }
    | Rational p, Number q ->
      { numerator = p.numerator
      ; denominator = q * p.denominator }

  let sum (p : t) (q : t) : t =
    match p, q with
    | Number p, Number q -> Rational { numerator = p + q ; denominator = 1 }
    | Rational p, Number q -> times_const_t q p
    | Number p, Rational q -> times_const_t p q
    | Rational p, Rational q ->
      Rational { numerator = p.numerator * q.numerator
               ; denominator = p.denominator * q.denominator }

end
