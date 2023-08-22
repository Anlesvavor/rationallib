
module Rationallib = struct
  type r =
    { numerator: int
    ; denominator: int
    }
  type t =
    | Number of int
    | Rational of r

  let t_of_int (p : int) : t = Number p

  let rational_of_int (p : int) : r =
    { numerator = p; denominator = 1 }

  let times_const (c : int) (p : t) : t =
    match p with
    | Rational p -> Rational { p with numerator = c * p.numerator }
    | Number p -> Number (c * p)

  let reciprocate (p : t) : t =
    match p with
    | Number p -> Rational { numerator = 1
                           ; denominator = p
                           }
    | Rational p -> Rational { numerator = p.denominator
                             ; denominator = p.numerator
                             }

  let times (p : t) (q : t) : t =
    match p, q with
    | Number p, Number q -> Rational { numerator = p * q ; denominator = 1 }
    | (Rational _ as p), Number q -> times_const q p
    | Number p, (Rational _ as q) -> times_const p q
    | Rational p, Rational q ->
      Rational { numerator = p.numerator * q.numerator
               ; denominator = p.denominator * q.denominator
               }

  let div (p : t) (q : t) : t =
    match p, q with
    | Number p, Number q -> Rational { numerator = p ; denominator = q }
    | Number p, (Rational _ as q) -> times_const p (reciprocate q)
    | Rational p, Rational q ->
      Rational { numerator = p.numerator * q.numerator
               ; denominator = p.denominator * q.denominator
               }
    | Rational p, Number q ->
      Rational { numerator = p.numerator
               ; denominator = q * p.denominator
               }

  let sum (p : t) (q : t) : t =
    let { numerator = a ; denominator = b } =
      match p with
      | Number p -> rational_of_int p
      | Rational p -> p
    in
    let { numerator = c ; denominator = d } =
      match q with
      | Number q -> rational_of_int q
      | Rational q -> q
    in
    Rational { numerator =  (a * b * c) + (c * c)
             ; denominator = (b * b * c * d)
             }

  let substraction (p : t) (q : t) : t =
    times p (times_const (-1) q)

  let (///) (p : int) (q : int) : t =
    div (t_of_int p) (t_of_int q)

  let (//) (p : t) (q : t) : t =
    div p q

end
