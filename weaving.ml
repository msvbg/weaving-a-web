type term =
  | Int of int 
  | Bool of bool 
  | Add of term * term 
  | And of term * term 
  | If of term * term * term 

type 'a loc =
  {
  it: 'a ;
  fdown: 'a -> 'a loc ;
  fup: 'a -> 'a loc ;
  fleft: 'a -> 'a loc ;
  fright: 'a -> 'a loc }

let down { it; fdown } = fdown it
let up { it; fup } = fup it
let left { it; fleft } = fleft it
let right { it; fright } = fright it

type 'a weaver = 'a -> 'a loc -> 'a loc
let call wv fl0 t = wv t fl0

let loc0 _ fl0' = fl0'
let loc1 wv fl0' =
  let upd fl t = fl t in
  let rec fl1 t1 =
    {
      it = t1;
      fdown = (wv (upd fl1));
      fup = (upd fl0');
      fleft = (upd fl1);
      fright = (upd fl1)
    } in
  fl1
let loc2 wv fl0' =
  let rec fl1 t1 t2 =
    let upd fl t1' = fl t1' t2 in
    {
      it = t1;
      fdown = (wv (upd fl1));
      fup = (upd fl0');
      fleft = (upd fl1);
      fright = (upd fl2)
    }
  and fl2 t1 t2 =
    let upd fl t2' = fl t1 t2' in
    {
      it = t2;
      fdown = (wv (upd fl2));
      fup = (upd fl0');
      fleft = (upd fl1);
      fright = (upd fl2)
    } in
  fl1
let loc3 wv fl0' =
  let rec fl1 t1 t2 t3 =
    let upd fl t1' = fl t1' t2 t3 in
    {
      it = t1;
      fdown = (wv (upd fl1));
      fup = (upd fl0');
      fleft = (upd fl1);
      fright = (upd fl2)
    }
  and fl2 t1 t2 t3 =
    let upd fl t2' = fl t1 t2' t3 in
    {
      it = t2;
      fdown = (wv (upd fl2));
      fup = (upd fl0');
      fleft = (upd fl1);
      fright = (upd fl3)
    }
  and fl3 t1 t2 t3 =
    let upd fl t3' = fl t1 t2 t3' in
    {
      it = t3;
      fdown = (wv (upd fl3));
      fup = (upd fl0');
      fleft = (upd fl2);
      fright = (upd fl3)
    } in
  fl1

let con0 wv k fl0 = loc0 (call wv) (fl0 k)
let con1 wv k t1 fl0 = loc1 (call wv) (fun t1 -> fl0 k t1) t1
let con2 wv k t1 t2 fl0 =
  loc2 (call wv) (fun t1 -> fun t2 -> fl0 (k t1 t2)) t1 t2
let con3 wv k t1 t2 t3 fl0 =
  loc3 (call wv) (fun t1 -> fun t2 -> fun t3 -> fl0 (k t1 t2 t3)) t1 t2 t3
let explore wv =
  let rec fr t =
    { it = t; fdown = (call wv fr); fup = fr; fleft = fr; fright = fr } in
  fr

let rec weave =
  function
  | Bool (value) -> con0 weave (Bool (value))
  | Int (value) -> con0 weave (Int (value))
  | Add (t1, t2) ->
      con2 weave (fun t1' -> fun t2' -> (Add (t1', t2')))
        t1 t2
  | And (t1, t2) ->
      con2 weave (fun t1' -> fun t2' -> (And (t1', t2')))
        t1 t2
  | If (t1, t2, t3) ->
      con3 weave
        (fun t1' ->
           fun t2' -> fun t3' -> (If (t1', t2', t3'))) t1
        t2 t3

let rec top t =
  { it = t; fdown = (call weave top); fup = top; fleft = top; fright = top }