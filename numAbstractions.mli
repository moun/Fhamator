module type Interval =
  sig
    type z' = Z of Num.num | Infty_pos | Infty_neg
    type interval = z' * z'
    val min : 'a * 'b -> 'a
    val max : 'a * 'b -> 'b
    val min_z' : z' -> z' -> z'
    val max_z' : z' -> z' -> z'
    val abs_z' : z' -> z'
    val le_test : z' -> z' -> bool
    module L :
      sig
        type t = (z' * z') option
        val string_of_z' : z' -> string
        val to_string : (z' * z') option -> string
        val join : (z' * z') option -> (z' * z') option -> (z' * z') option
        val meet : (z' * z') option -> (z' * z') option -> (z' * z') option
        val widen : (z' * z') option -> (z' * z') option -> (z' * z') option
        val narrow : (z' * z') option -> (z' * z') option -> (z' * z') option
        val order_dec : (z' * z') option -> (z' * z') option -> bool
        val bottom : unit -> 'a option
        val top : unit -> (z' * z') option
      end
    val sub1 : z' -> z'
    val add1 : z' -> z'
    val backward_eq :
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val backward_lt :
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val backward_neq :
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val backward_le :
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val backward_comp :
      Syntax.comp ->
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val add' : z' -> z' -> z'
    val sub' : z' -> z' -> z'
    val mult' : z' -> z' -> z'
    val forward_rem : z' * z' -> z' * z' -> z' * z'
    val forward_binop :
      l:'a ->
      Syntax.binop ->
      (z' * z') option -> (z' * z') option -> (z' * z') option
    val forward_comp :
      l:'a ->
      Syntax.comp -> (z' * z') option -> (z' * z') option -> (z' * z') option
    val forward_if :
      l:'a ->
      'b -> 'c -> (z' * z') option -> (z' * z') option -> (z' * z') option
    val forward_loop :
      l:'a ->
      'b -> 'c  -> (z' * z') option -> (z' * z') option								    
    val reduce : z' -> z' -> (z' * z') option
    val backward_binop :
      Syntax.binop ->
      (z' * z') option ->
      (z' * z') option ->
      (z' * z') option -> (z' * z') option * (z' * z') option
    val const : l:'a -> int -> (z' * z') option
    val initl : l:'a -> (z' * z') option
    val inith : l:'a -> (z' * z') option
  end

module type Cardinal =
  sig
    type label = Syntax.label
    module Label_Set :
      sig
        type elt = label
        type t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val max_elt : t -> elt
        val choose : t -> elt
        val split : elt -> t -> t * bool * t
      end
    type ppset = Top | Set of Label_Set.t
    type cardinal = Z.t
    val cardinal_top : Z.t
    val ppset_l : Label_Set.elt option -> ppset
    module L :
      sig
        type t = ppset * cardinal
        val order_dec : ppset * Z.t -> ppset * Z.t -> bool
        val join_labels : ppset -> ppset -> ppset
        val join_cardinals : Z.t -> Z.t -> Z.t
        val join : ppset * Z.t -> ppset * Z.t -> ppset * Z.t
        val meet_labels : ppset -> ppset -> ppset
        val meet_cardinals : Z.t -> Z.t -> Z.t
        val meet : ppset * Z.t -> ppset * Z.t -> ppset * Z.t
        val widen : ppset * Z.t -> ppset * Z.t -> ppset * Z.t
        val narrow : ppset * Z.t -> ppset * Z.t -> ppset * Z.t
        val bottom : unit -> ppset * Z.t
        val top : unit -> ppset * Z.t
        val is_bottom_labels : ppset -> bool
        val is_bottom_cardinals : Z.t -> bool
        val is_bottom : ppset * Z.t -> bool
        val to_string : ppset * Z.t -> string
      end
    val backward_eq : 'a -> 'b -> 'a * 'b
    val backward_neq : 'a -> 'b -> 'a * 'b
    val backward_lt : 'a -> 'b -> 'a * 'b
    val backward_le : 'a -> 'b -> 'a * 'b
    val forward_binop_cardinal :
      l:Label_Set.elt option -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_add :
      l:Label_Set.elt option -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_sub :
      l:Label_Set.elt option -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_mult :
      l:Label_Set.elt option -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_rem :
      l:Label_Set.elt option -> 'a * 'b -> 'c * 'd -> ppset * 'b
    val backward_add : 'a -> 'b -> 'c -> 'b * 'c
    val backward_sub : 'a -> 'b -> 'c -> 'b * 'c
    val backward_mult : 'a -> 'b -> 'c -> 'b * 'c
    val backward_rem : 'a -> 'b -> 'c -> 'b * 'c
    val const : l:Label_Set.elt option -> 'a -> ppset * Z.t
    val initl : l:Label_Set.elt option -> ppset * Z.t
    val inith : l:Label_Set.elt option -> ppset * Z.t
    val backward_comp : Syntax.comp -> 'a -> 'b -> 'a * 'b
    val forward_binop :
      l:Label_Set.elt option ->
      Syntax.binop -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_comp :
      l:Label_Set.elt option ->
      Syntax.comp -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val add_c :
      Label_Set.elt list -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
    val forward_if :
      l:'a ->
      'b * Z.t ->
      Label_Set.elt list -> ppset * Z.t -> ppset * Z.t -> ppset * Z.t
     val forward_loop :
      l:'a ->
      L.t ->
      Label_Set.elt list -> ppset * Z.t -> ppset * Z.t 								    
    val backward_binop : Syntax.binop -> 'a -> 'b -> 'c -> 'b * 'c
  end
