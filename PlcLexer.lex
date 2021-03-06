(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

fun keyword (s, lpos, rpos) =
    case s of
          "var" => T_VAR (lpos, rpos)
        | "Bool" => T_BOOL (lpos, rpos)
        | "Int" => T_INT (lpos, rpos)
        | "Nil" => T_NIL (lpos, rpos)
        | "true" => T_TRUE (lpos, rpos)
        | "false" => T_FALSE (lpos, rpos)
        | "fun" => T_FUN (lpos, rpos)
        | "fn" => T_FN (lpos, rpos)
        | "hd" => T_HD (lpos, rpos)
        | "tl" => T_TL (lpos, rpos)
        | "if" => T_IF (lpos, rpos)
        | "then" => T_THEN (lpos, rpos)
        | "else" => T_ELSE (lpos, rpos)
        | "ise" => T_ISE (lpos, rpos)
        | "print" => T_PRINT (lpos, rpos)
        | "rec" => T_REC (lpos, rpos)
        | "match" => T_MATCH (lpos, rpos)
        | "with" => T_WITH (lpos, rpos)
        | "end" => T_END (lpos, rpos)
        | "_" => T_UNDERSCORE (lpos, rpos)
        | _ => NAME (s, lpos, rpos)

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s =
    case Int.fromString s of
        SOME i => i
        | NONE => raise Fail ("Could not convert to int '" ^ s ^"'")

(* Initialize the lexer. *)
fun init() = ()
%%
%s COMMENT;
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit=[0-9];
whitespace=[\ \t];
identifier=[A-Za-z_][A-Za-z_0-9]*;
eol = ("\013\010"|"\010"|"\013");
%%
<INITIAL>\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (NAT (strToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyword(yytext, yypos, yypos));
<INITIAL>"(*" => (YYBEGIN COMMENT; lex());
<INITIAL>"->" => (T_MINUS_ARROW(yypos,yypos));
<INITIAL>"=>" => (T_EQUAL_ARROW(yypos,yypos));
<INITIAL>"<=" => (T_SMALLER_EQUAL(yypos,yypos));
<INITIAL>"!=" => (T_DIFF(yypos,yypos));
<INITIAL>"::" => (T_CONCAT_DC(yypos,yypos));
<INITIAL>"&&" => (T_AND_DCE(yypos,yypos));
<INITIAL>"+" => (T_PLUS(yypos,yypos));
<INITIAL>"-" => (T_MINUS(yypos,yypos));
<INITIAL>"*" => (T_MUL(yypos,yypos));
<INITIAL>"/" => (T_DIV(yypos,yypos));
<INITIAL>"(" => (T_OPEN_PAR(yypos,yypos));
<INITIAL>")" => (T_CLOSE_PAR(yypos,yypos));
<INITIAL>"[" => (T_OPEN_BRACES(yypos,yypos));
<INITIAL>"]" => (T_CLOSE_BRACES(yypos,yypos));
<INITIAL>"{" => (T_OPEN_KEYS(yypos,yypos));
<INITIAL>"}" => (T_CLOSE_KEYS(yypos,yypos));
<INITIAL>";" => (T_SEMICOLON(yypos,yypos));
<INITIAL>":" => (T_COLON(yypos,yypos));
<INITIAL>"=" => (T_EQUAL(yypos,yypos));
<INITIAL>"<" => (T_SMALLER(yypos,yypos));
<INITIAL>"!" => (T_EXCL(yypos,yypos));
<INITIAL>"|" => (T_PIPE(yypos, yypos));
<INITIAL>"," => (T_COMMA(yypos, yypos));
<COMMENT>"*)"=> (YYBEGIN INITIAL; lex());
<COMMENT>{eol} => (lineNumber := !lineNumber + 1; lex());
<COMMENT>. => (lex());
<INITIAL>. => (error("Lex error"); raise Fail("bad char "^yytext));