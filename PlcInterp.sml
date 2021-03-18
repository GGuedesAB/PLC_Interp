(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (ConI i) _ = IntV i
  | eval (ConB b) _ = BoolV b
  | eval (ESeq e) _ = SeqV []
  | eval (Var v) (env:plcVal env) = let in lookup env v handle SymbolNotFound => raise SymbolNotFound end
  | eval (Item (index, exp)) (env:plcVal env) =
    let
      fun getElementI (index, []) = raise Impossible
        | getElementI (index, (x::[])) = if index = 1 then x else raise Impossible
        | getElementI (index, (x::xs)) = if index = 1 then x else getElementI (index - 1, xs)
      val value = eval exp env
    in
      case value of
          ListV l => getElementI (index, l)
        | SeqV s => getElementI (index, s)
        | _ => raise Impossible
    end
  | eval (List []) (env:plcVal env) = ListV []
  | eval (List l) (env:plcVal env) = 
    let
      fun unroll (x::[]) = eval x env :: []
        | unroll (x::xs) = eval x env :: unroll xs
        | unroll _ = raise Impossible;
    in
      ListV (unroll l)
    end
  | eval (If (exp1, exp2, exp3)) (env:plcVal env) = 
    let in
      case eval exp1 env of 
          BoolV true => eval exp2 env
        | BoolV false => eval exp3 env
        | _ => raise Impossible
    end
  | eval (Match (exp1, matchList)) (env:plcVal env) = 
    let 
      val evalMatchVar = eval exp1 env 
      (* Try matches will return the "cond -> expr" for which cond matches exp1 *)
      fun tryMatches (matchVar, x::[]) env =
          let in
            case x of
                (SOME exp2, exp3) => if matchVar = eval exp2 env then exp3 else raise ValueNotFoundInMatch
              | (NONE, exp3) => exp3
          end
        | tryMatches (matchVar, x::xs) env =  let in
            case x of
                (SOME exp2, exp3) => if matchVar = eval exp2 env then exp3 else tryMatches (matchVar, xs) env
              | (NONE, exp3) => raise Impossible
          end
        | tryMatches (matchVar, _ ) env = raise Impossible
    in
      eval (tryMatches (evalMatchVar, matchList) env) env
    end
  | eval (Prim1 (oper, exp)) (env:plcVal env) =
    let
      val v = eval exp env
    in
      case v of
          IntV i => 
          let in
            case oper of
                "-" => IntV (~ i)
              | "print" => 
                let 
                  val v = IntV i
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | BoolV b =>
          let in
            case oper of
                "!" => BoolV (not b)
              | "print" => 
                let 
                  val v = BoolV b
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | SeqV s =>
          let in
            case oper of
                "hd" => let in let in hd s end handle Empty => raise HDEmptySeq end
              | "tl" => let in let in SeqV (tl s) end handle Empty => raise TLEmptySeq end
              | "ise" =>
                let in
                  case s of
                      [] => BoolV true
                    | _ => BoolV false
                end
              | "print" => 
                let 
                  val ignore = print(list2string(val2string, s) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | ListV l =>
          let in
            case oper of
                "print" => 
                let 
                  val ignore = print(list2string(val2string, l) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | _ => raise Impossible
    end
  | eval (Prim2 (oper, exp1, exp2)) (env:plcVal env) =
    if oper = ";" then
      let
        val ignore = eval exp1 env
      in
        eval exp2 env
      end
    else
      let
        val v1 = eval exp1 env
        val v2 = eval exp2 env
      in
        case (v1, v2) of
            (IntV i1, IntV i2) => 
            let in
              case oper of
                  "+" => IntV (i1 + i2)
                | "-" => IntV (i1 - i2)
                | "*" => IntV (i1 * i2)
                | "/" => IntV (i1 div i2)
                | "<" => BoolV (i1 < i2)
                | "<=" => BoolV (i1 <= i2)
                | "=" => BoolV (i1 = i2)
                | "!=" => BoolV (i1 <> i2)
                | _ => raise Impossible
            end
          | (BoolV b1, BoolV b2) => 
            let in
              case oper of
                  "&&" => BoolV (b1 andalso b2)
                | "=" => BoolV (b1 = b2)
                | "!=" => BoolV (b1 <> b2)
                | _ => raise Impossible
            end
          | (IntV i1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (IntV i1 :: s2)
                | _ => raise Impossible
            end
          | (BoolV b1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (BoolV b1 :: s2)
                | _ => raise Impossible
            end
          | (ListV l1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (ListV l1 :: s2)
                | _ => raise Impossible
            end
          | _ => raise Impossible
      end
  | eval (Let (var, exp1, exp2)) (env:plcVal env) =
    let
      val nEnv = (var, eval exp1 env) :: env
    in
      eval exp2 nEnv
    end
  | eval (Anon (typ, arg, exp)) (env:plcVal env) = Clos ("", arg, exp, env) (* We need to check if var can be found in the env of Anon *)
  | eval (Call (exp1, exp2)) (env:plcVal env) = 
    let
      fun mountArguments (List (x::[])) = [eval x env]
        | mountArguments (List (x::xs)) = [eval x env] @ mountArguments (List xs)
        | mountArguments (exp) = [eval exp env]
      val nEnv = [("$list", ListV (mountArguments exp2))] @ env
      val f = eval exp1 env
    in
      case f of
          Clos(name, var, exp, cEnv) =>
            let
              val ev = eval exp2 nEnv
              val fEnv = (var, ev)::(name, f)::cEnv
            in
              eval exp fEnv
            end
        | _ => raise NotAFunc
    end
  | eval (Letrec (fName, argTyp, arg, funTyp, exp1, exp2)) (env:plcVal env) =
    let
      val nEnv = (fName, Clos(fName, arg, exp1, env)) :: env
    in
      eval exp2 nEnv
    end
  ;