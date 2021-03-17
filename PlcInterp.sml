(* PlcInterp *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (ConI i) _ = IntV i
  | eval (ConB b) _ = BoolV b
  | eval (ESeq e) env = SeqV []
  | eval (Var v) env = (lookup env v)
  | eval (Item i) env =
    let in
      if #1 i = 1 then
        case #2 i of
            List [] => raise Impossible
          | List l => eval (hd l) env
          | _ => raise Impossible
      else
        case #2 i of
            List [] => raise Impossible
          | List l => eval (Item ((#1 i) - 1, List(tl l))) env
          | _ => raise Impossible
    end
  | eval (List []) env = ListV []
  | eval (List l) env = 
    let
      fun unroll (x::[]) = eval x env :: []
        | unroll (x::xs) = eval x env :: unroll xs
        | unroll _ = raise Impossible;
    in
      ListV (unroll l)
    end
  | eval (If (exp1, exp2, exp3)) env = 
    let in
      case eval exp1 env of 
          BoolV true => eval exp2 env
        | BoolV false => eval exp3 env
        | _ => raise Impossible
    end
  | eval (Match (exp1, matchList)) env = 
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
  | eval (Let (var, exp1, exp2)) env =
    let
      val nEnv = (var, eval exp1 env) :: env
    in
      eval exp2 nEnv
    end
  | eval (Prim1 (oper, exp)) env =
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
                "hd" => hd s
              | "tl" => SeqV (tl s)
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
        | _ => raise Impossible
    end
  | eval (Prim2 (oper, exp1, exp2)) env =
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
                | "::" => SeqV (IntV i1 :: IntV i2 :: [])
                | _ => raise Impossible
            end
          | (BoolV b1, BoolV b2) => 
            let in
              case oper of
                  "&&" => BoolV (b1 andalso b2)
                | "=" => BoolV (b1 = b2)
                | "!=" => BoolV (b1 <> b2)
                | "::" => SeqV (BoolV b1 :: BoolV b2 :: [])
                | _ => raise Impossible
            end
          | (ListV l1, ListV l2) => 
            let in
              case oper of
                  "::" => SeqV (ListV l1 :: ListV l2 :: [])
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
  | eval (Letrec lr) env = IntV 0
  | eval (Call c) env = IntV 1
  | eval (Anon a) env = IntV 2;

(* eval (fromString "15") [];
eval (fromString "true") [];
eval (fromString "()") [];
eval (fromString "(6,false)[1]") [];
eval (fromString "([Bool] [])") [];
eval (fromString "print x; true") [("x", BoolV false)];
eval (fromString "3::7::t") [("t", IntV 19)];
eval (fromString "fn (Int x) => -x end") [];
eval (fromString "var x = 9; x + 3") [];
eval (fromString "fun f(Int x) = x; f(1)") [];
eval (fromString "(x, y, z)[1]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "(x, y, z)[2]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "(x, y, z)[3]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "print 5") [];
eval (fromString "-5") [];
eval (fromString "-x") [("x", IntV 8)];
eval (fromString "true && false") [];
eval (fromString "true && true") [];
eval (fromString "5 + 5") [];
eval (fromString "print y; print x") [("x", IntV 3), ("y", IntV 9)];
eval (fromString "print x") [("x", IntV 3)];
eval (fromString "print y; x + 8") [("x", IntV 3), ("y", IntV 9)];
eval (fromString "1::2") [];
eval (fromString "1::2::3::4") [];
eval (fromString "(1,2)::(2,3)::(3,4)::(4,5)") [];
eval (fromString "x::y") [("x", IntV 10), ("y", IntV 9)];
eval (fromString "x::y::z") [("x", IntV 10), ("y", IntV 9), ("z", IntV 8)];

eval (fromString "x::y::z::w") [("x", IntV 10), ("y", IntV 9), ("z", IntV 8), ("w", IntV 7)];

eval (fromString "if 5 = 5 then 8 + 3 else 8 - 3") [];
eval (fromString "if x = z then y + 7 else w + 7") [("x", IntV 10), ("y", IntV 9), ("z", IntV 8), ("w", IntV 7)];
eval (fromString "if x = z then y + 7 else w + 7") [("x", IntV 10), ("y", IntV 9), ("z", IntV 10), ("w", IntV 7)];
eval (fromString "match x with | 0 -> 1| _ -> -1 end") [("x", IntV 3)];
eval (fromString "match x with | 0 -> 1| _ -> -1 end") [("x", IntV 0)];

eval (fromString "var x = 3; if x < 2 then x else y") [("y", IntV 9)];
eval (fromString "var x = 3; if x < 4 then x else y") [("y", IntV 9)];
eval (fromString "var x = 1::2::3::4; print x") []; *)