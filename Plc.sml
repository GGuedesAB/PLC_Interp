(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
  let
    val expType = let in
      teval exp []
    end
    handle SymbolNotFound => let val p = print ("ERROR: One or more symbols are not defined in expression evaluation.") in raise SymbolNotFound end
          | EmptySeq => let val p =  print ("ERROR: Sequences was not declared with their sequence type. e.g. (Int [])") in raise EmptySeq end
          | NotEqTypes => let val p =  print ("ERROR: Types used in comparision are different.") in raise NotEqTypes end
          | WrongRetType => let val p =  print ("ERROR: Function return type and function body do not match.") in raise WrongRetType end
          | DiffBrTypes => let val p =  print ("ERROR: If branches have different types.") in raise DiffBrTypes end
          | IfCondNotBool => let val p =  print ("ERROR: If condition is not boolean.") in raise IfCondNotBool end
          | NoMatchResults => let val p =  print ("ERROR: Match has empty pattern match options.") in raise NoMatchResults end
          | MatchResTypeDiff => let val p =  print ("ERROR: Match has different result types.") in raise MatchResTypeDiff end
          | MatchCondTypesDiff => let val p =  print ("ERROR: Match conditions have different types than the to be matched expression.") in raise MatchCondTypesDiff end
          | CallTypeMisM => let val p =  print ("ERROR: Function real argument type is different than its formal argument types..") in raise CallTypeMisM end
          | NotFunc => let val p =  print ("ERROR: Calling variable that is not a function.") in raise NotFunc end
          | ListOutOfRange => let val p =  print ("ERROR: Accessing list beyond its limits.") in raise ListOutOfRange end
          | OpNonList => let val p =  print ("ERROR: Trying to access element of something that is not a list.") in raise OpNonList end
          | UnknownType => let val p = print ("ERROR: Sorry, the type checker is not able to figure out why the exception was raised.") in raise UnknownType end
          | _ => let val p = print ("ERROR: Sorry, the type checker is not able to figure out why the exception was raised.") in raise UnknownType end
    val expResult = let in
      eval exp []
    end
    handle SymbolNotFound => let val p = print ("ERROR: One or more symbols are not defined in expression evaluation.") in raise SymbolNotFound end
         | HDEmptySeq => let val p =  print ("ERROR: Trying to access head of empty sequence.") in raise HDEmptySeq end
         | TLEmptySeq => let val p =  print ("ERROR: Trying to access tail of empty sequence.") in raise TLEmptySeq end
         | ValueNotFoundInMatch => let val p =  print ("ERROR: Could not find a match in the match list for expression to be matched.") in raise ValueNotFoundInMatch end
         | NotAFunc => let val p =  print ("ERROR: Calling variable that is not a function.") in raise NotAFunc end
         | Impossible => let val p = print ("ERROR: Sorry, the interpreter is not able to figure out why the exception was raised.") in raise Impossible end
         | _ => let val p = print ("ERROR: Sorry, the interpreter is not able to figure out why the exception was raised.") in raise Impossible end
  in
    val2string(expResult) ^ " : " ^ type2string(expType)
  end