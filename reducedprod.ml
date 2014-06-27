

module Make =
  functor (AbNum1:Abstraction.Num) ->
    functor (AbNum2:Abstraction.Num  ) ->
      functor (Red12:  sig  type t val reduce: t -> t end) ->
struct
  
  module Lat = Latticeprod.Make (AbNum1.L) (AbNum2.L)

  let reduce (x,y) = (x,y)  

  module L  = Lat.S
  let backward_comp c (x1,y1) (x2,y2) = 
    let x1',x2' = AbNum1.backward_comp c x1 x2 in
    let y1',y2' = AbNum2.backward_comp c y1 y2 in
    (x1',y1'),(x2',y2')


  let forward_binop ~l b (x1,y1) (x2,y2) = 
    let x = AbNum1.forward_binop ~l b x1 x2 in
    let y = AbNum2.forward_binop ~l b y1 y2 in
    reduce (x,y)
    
  let forward_comp ~l c (x1,y1) (x2,y2) = 
    let x = AbNum1.forward_comp ~l c x1 x2 in
    let y = AbNum2.forward_comp ~l c y1 y2 in
    reduce (x,y)
    

  let backward_binop b (x1,y1) (x2,y2) (x3,y3) = 
    let x1',x2' = AbNum1.backward_binop b x1 x2 x3 in
    let y1',y2' = AbNum2.backward_binop b y1 y2 y3 in
    (x1',y1'),(x2',y2')

  let forward_if ~l (x1,y1) labels (x2,y2) (x3,y3) = 
    let x1' = AbNum1.forward_if ~l x1 labels x2 x3 in
    let y1' = AbNum2.forward_if ~l y1 labels y2 y3 in
    reduce (x1',y1')

  let const ~l c = reduce (AbNum1.const ~l c, AbNum2.const ~l c)

  let initl ~l = reduce (AbNum1.initl ~l, AbNum2.initl ~l)
 
  let inith ~l = reduce (AbNum1.inith ~l, AbNum2.inith ~l)

end
