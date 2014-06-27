

module NumInterval = NumAbstractionInterval.Make

module NumCardinal = NumAbstractionCardinal.Make

module NumSign = NumAbstractionSign.Make

(*module type NumProd = 
  sig
    include Reduceprod.Make
    val reduce : Abstraction.Num.L.t -> Abstraction.Num.L.t 
  end*)

module RedAB =
  functor (A: Abstraction.Num) -> 
    functor (B: Abstraction.Num) ->
      functor (Q: sig val reduce : A.L.t * B.L.t -> A.L.t * B.L.t end) ->
struct
  type t = A.L.t * B.L.t
  let reduce = Q.reduce
end

module RedABC =
  functor (AB: Abstraction.NumProd) -> 
    functor (C: Abstraction.Num) ->
      functor (Q: sig val reduce : AB.L.t * C.L.t -> AB.L.t * C.L.t end) ->
struct
  type t = AB.L.t * C.L.t
  let reduce = Q.reduce
end

module RedIntervalCardinal = 
  RedAB
    (NumInterval) 
    (NumCardinal)
    (struct let reduce = (fun (a,b) -> (a,b)) end)

module IntervalCardinal = 
      Reducedprod.Make (NumInterval) (NumCardinal) (RedIntervalCardinal)

module RedIntervalCardinalSign = 
  RedABC
    (IntervalCardinal)
    (NumSign)
    (struct 
      let reduce (ab,c) = 
	let a',b' = IntervalCardinal.reduce ab in
	((a',b'),c)
     end)

module IntervalCardinalSign =
  Reducedprod.Make (IntervalCardinal) (NumSign) (RedIntervalCardinalSign)

module NumInterval2 = NumAbstractionInterval.Make

module RedIntervalCardinalSignInterval2 = 
  RedABC
    (IntervalCardinalSign)
    (NumInterval2)
    (struct
      let reduce (abc,d) =
	let ((a',b'),c') = IntervalCardinalSign.reduce abc in
	((a',b'),c'),d
     end)
