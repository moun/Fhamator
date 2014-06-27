module Make =
  functor (L1: Lattice.S) ->
    functor (L2:Lattice.S) ->
    struct

      module S = 
      struct
	
	type t = L1.t * L2.t
	    
	let order_dec (x1,y1) (x2,y2) = 
	  (L1.order_dec x1 x2) && (L2.order_dec y1 y2)

	let join (x1,y1) (x2,y2) =
	  L1.join x1 x2,L2.join y1 y2

	let meet (x1,y1) (x2,y2) =
	  L1.meet x1 x2,L2.meet y1 y2

	let widen (x1,y1) (x2,y2) =
	  L1.widen x1 x2,L2.widen y1 y2

	let narrow (x1,y1) (x2,y2) =
	  L1.narrow x1 x2,L2.narrow y1 y2

	let bottom () = 
	  L1.bottom (), L2.bottom ()

	let top () = 
	  L1.top (), L2.top ()

	let to_string (x,y) = 
	  Printf.sprintf "%s , %s" (L1.to_string x) (L2.to_string y)
	  
      end
    end
