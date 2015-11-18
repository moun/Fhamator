

module Make =
  functor (AbNum1:Abstraction.Num) ->
    functor (AbNum2:Abstraction.Num  ) ->
      functor (Red12:  sig  type t = AbNum1.L.t * AbNum2.L.t val reduce: t -> t end) ->
struct
  
  module Lat = Latticeprod.Make (AbNum1.L) (AbNum2.L)

  let reduce = Red12.reduce

  module L  = Lat.S
  let backward_comp c (x1,y1) (x2,y2) = 
    let x1',x2' = AbNum1.backward_comp c x1 x2 in
    let y1',y2' = AbNum2.backward_comp c y1 y2 in
    Red12.reduce (x1',y1'), Red12.reduce (x2',y2')


  let forward_binop ~l b (x1,y1) (x2,y2) = 
    let x = AbNum1.forward_binop ~l b x1 x2 in
    let y = AbNum2.forward_binop ~l b y1 y2 in
    Red12.reduce (x,y)
    
  let forward_comp ~l c (x1,y1) (x2,y2) = 
    let x = AbNum1.forward_comp ~l c x1 x2 in
    let y = AbNum2.forward_comp ~l c y1 y2 in
    Red12.reduce (x,y)
    

  let backward_binop b (x1,y1) (x2,y2) (x3,y3) = 
    let x1',x2' = AbNum1.backward_binop b x1 x2 x3 in
    let y1',y2' = AbNum2.backward_binop b y1 y2 y3 in
    Red12.reduce (x1',y1'),Red12.reduce (x2',y2')

  let forward_if ~l (x1,y1) labels (x2,y2) (x3,y3) = 
    let x1' = AbNum1.forward_if ~l x1 labels x2 x3 in
    let y1' = AbNum2.forward_if ~l y1 labels y2 y3 in
    Red12.reduce (x1',y1')

  let forward_loop ~l (x0,y0) labels (x,y)  = 
    let x' = AbNum1.forward_loop ~l x0 labels x in
    let y' = AbNum2.forward_loop ~l y0 labels y in
    Red12.reduce (x',y')
		 
  let const ~l c = Red12.reduce (AbNum1.const ~l c, AbNum2.const ~l c)

  let initl ~l = Red12.reduce (AbNum1.initl ~l, AbNum2.initl ~l)
 
  let inith ~l = Red12.reduce (AbNum1.inith ~l, AbNum2.inith ~l)

end
