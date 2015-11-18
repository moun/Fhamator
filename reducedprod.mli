module Make : 
  functor(AbNum1:Abstraction.Num) ->
    functor(AbNum2:Abstraction.Num) ->
      functor( Red12: 
	sig type t = AbNum1.L.t * AbNum2.L.t  val reduce : t -> t end) ->
      sig

	module L :
	sig
	  type t = AbNum1.L.t * AbNum2.L.t
          val order_dec : t -> AbNum1.L.t * AbNum2.L.t -> bool
          val join : t -> t -> t  
          val meet : t -> t -> t
          val widen : t-> t -> t
          val narrow : t -> t -> t
          val bottom : unit -> t
          val top : unit -> t
          val to_string : t -> string
	end

	val backward_comp : Syntax.comp -> L.t  ->  L.t -> L.t * L.t
	  
	val forward_binop : l:Syntax.label option -> Syntax.binop -> L.t  -> L.t -> L.t 
    
	val forward_comp : l:Syntax.label option -> Syntax.comp -> L.t -> L.t  -> L.t 
    
	val backward_binop : Syntax.binop -> L.t -> L.t -> L.t -> L.t * L.t
	  
	val forward_if : l:Syntax.label option -> L.t  -> Syntax.label list -> L.t  ->   L.t  -> L.t 

	val forward_loop : l:Syntax.label option -> L.t -> Syntax.label list -> L.t -> L.t											   
	val const : l:Syntax.label option -> int -> L.t 

	val initl : l:Syntax.label option -> L.t 
	  
	val inith :l:Syntax.label option -> L.t
	  
	val reduce : L.t -> L.t
      end
