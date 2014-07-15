

module NumInterval = NumAbstractionInterval.Make

module NumCardinal = NumAbstractionCardinal.Make

module NumSign = NumAbstractionSign.Make

(*module type NumProd = 
  sig
    include Reduceprod.Make
    val reduce : Abstraction.Num.L.t -> Abstraction.Num.L.t 
  end*)

module RedAB =
  functor (A: NumAbstractions.Interval ) -> 
    functor (B: NumAbstractions.Cardinal ) ->
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
    (struct
      let reduce (i,c) =
          match i with 
	    | None -> None, NumCardinal.L.bottom ()
	    | Some (NumInterval.Z a0, NumInterval.Z a1) ->
	      (match c with
		| ppset, card ->
		  if (NumCardinal.L.is_bottom c) then
		   ( (*Printf.printf "reducing to bottom. Card: %b \n" (NumCardinal.L.is_bottom c);*)
		    None, NumCardinal.L.bottom())
		  else if
		      (Z.lt 
			 (Z.succ (Z.of_string (Num.string_of_num (Num.sub_num a1 a0))))
			 card) 
		  then
		    ( (*Printf.printf "reducing card wrt interval length \n";*)
		      i, (ppset, Z.succ (Z.of_string (Num.string_of_num (Num.sub_num a1 a0))))
		    )
		  else
		    i,c
	      )
	    | _ -> (i,c)
     end)

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
