(* Plc Parser Aux *)
exception invalidArgC;

fun unrollTypes (args: (plcType * string) list) : plcType list =
  case args of
      x::xt => [#1x] @ unrollTypes xt
    | [] => []

(* Creat the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr =
  case xs of
    x::xt => Let(#2x, Item(n, Var "$list"), makeFunAux (n+1, xt, e))
    | [] => e

(* Create the list of arguments of a function. *)
fun makeType (args: (plcType * string) list): plcType =
  let
    val typeList = unrollTypes (args)
  in
    ListT typeList
  end

(* Create a function expression. *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType xs
        val e1' = makeFunAux (1, xs, e1)
      in
        Letrec(f, t, "$list", rt, e1', e2)
      end;

(* Create a Anonymus function expression. *)
fun makeAnon (xs:(plcType * string) list, e:expr):expr =
  case xs of
      [] => Anon(ListT [], "()", e)
    | (t,x)::[] => Anon(t,x,e)
    | _ =>
      let
        val t = makeType xs
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;