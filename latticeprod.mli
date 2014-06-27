module Make :
  functor (L1: Lattice.S)  -> 
    functor (L2: Lattice.S) ->
      sig
	module S :
	sig
	  type t = L1.t * L2.t
            val order_dec : L1.t * L2.t -> L1.t * L2.t -> bool
            val join : L1.t * L2.t -> L1.t * L2.t -> L1.t * L2.t
            val meet : L1.t * L2.t -> L1.t * L2.t -> L1.t * L2.t
            val widen : L1.t * L2.t -> L1.t * L2.t -> L1.t * L2.t
            val narrow : L1.t * L2.t -> L1.t * L2.t -> L1.t * L2.t
            val bottom : unit -> L1.t * L2.t
            val top : unit -> L1.t * L2.t
            val to_string : L1.t * L2.t -> string
	end
      end
