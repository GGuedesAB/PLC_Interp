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
  | eval (Var v) st = eval (lookup st v) st
  | eval (List l) st = 
      let
        fun unroll (x::[]) = eval x st :: []
          | unroll (x::xs) = eval x st :: unroll xs
          | unroll _ = raise Impossible;
      in
        ListV (unroll l)
      end
  | eval (ESeq e) st = SeqV []
  | eval (Prim1 exp) st =
      let
        val operator = #1 exp
        val operand = #2 exp
      in
        case operator of
            "!" => let in
              case operand of
                  ConB b => eval (ConB (not b)) st
                | _ => raise Impossible
            end
          | "-" => let in
              case operand of
                  ConI i => eval (ConI (~ i)) st
                | _ => raise Impossible
            end
          | "hd" => let in
              case operand of
                  List l => eval (hd l) st
                | _ => raise Impossible
            end
          | "tl" => let in
              case operand of
                  List l => eval (List (tl l)) st
                | _ => raise Impossible
            end
          | "ise" => let in
              case operand of
                  List [] => eval (ConB true) st
                | _ => eval (ConB false) st
            end
          | "print" => let val x = print(val2string(eval operand st) ^ "\n") in
              ListV [] 
            end
      end
  | eval (Prim2 exp) st =
      let
        val operator = #1 exp
        val operand1 = #2 exp
        val operand2 = #3 exp
      in
        case operator of
            "&&" => let in
              case (operand1, operand2) of
                  (ConB b1, ConB b2) => eval (ConB (b1 andalso b2)) st
                | _ => raise Impossible
            end
          | "+" => let in
              case (operand1, operand2) of
                  (ConI i1, ConI i2) => eval (ConI (i1 + i2)) st
                | _ => raise Impossible
            end
      end
  | eval (Item i) st = 
      if #1 i = 1 then
        case #2 i of
            List l => eval (hd l) st
          | _ => raise Impossible
      else
        case #2 i of
            List l => eval (Item ((#1 i) - 1, List(tl l))) st
          | _ => raise Impossible;
  
