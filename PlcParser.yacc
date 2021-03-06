(* exception notList;
exception notTuple;
exception notDecl;

datatype TypeName =
    TypeNameDouble of plcType * string
    | TypeNameList of TypeName list

fun lHd l =
    case l of
        TypeNameList myL => hd myL
       | _ => raise notList

fun lTl l =
    case l of
        TypeNameList myL => hd (tl myL)
       | _ => raise notList

fun getFirst e =
    case e of
        TypeNameDouble tnd => #1 tnd
       | _ => raise notTuple

fun getSecond e =
    case e of
        TypeNameDouble tnd => #2 tnd
       | _ => raise notTuple *)

datatype DeclType =
    VarDecl of string * expr
    | FunDecl of string * expr
    | FunrecDecl of string * (plcType * string) list * plcType * expr

fun resolve (decl, prog) =
    case decl of
         VarDecl v => Let (#1v, #2v, prog)
       | FunDecl f => Let (#1f, #2f, prog)
       | FunrecDecl fr => makeFun (#1fr, #2fr, #3fr, #4fr, prog)


%%

%name PlcParser

%pos int

%term T_COLON | T_SEMICOLON | T_EQUAL |
      T_VAR | T_FUN | T_REC |
      T_IF | T_THEN | T_ELSE |
      T_MATCH | T_WITH |
      T_EXCL | T_HD | T_TL | T_ISE | T_PRINT |
      T_AND_DCE | T_PLUS | T_MINUS | T_MUL | T_DIV | T_DIFF | T_SMALLER | T_SMALLER_EQUAL | T_CONCAT_DC |
      T_OPEN_BRACES | T_CLOSE_BRACES | T_OPEN_PAR | T_CLOSE_PAR | T_OPEN_KEYS | T_CLOSE_KEYS |
      T_FN | T_EQUAL_ARROW | T_END |
      T_TRUE | T_FALSE |
      T_PIPE | T_MINUS_ARROW | T_UNDERSCORE | T_COMMA |
      T_BOOL | T_INT | T_NIL |
      NAME of string | NAT of int |
      EOF

%nonterm Prog of expr |
         Decl of DeclType |
         Expr of expr |
         App_expr of expr |
         Atomic_expr of expr |
         Match_expr of (expr option * expr) list |
         Cond_expr of expr option |
         Args of (plcType * string) list |
         Typed_var of plcType * string |
         Params of (plcType * string) list |
         Atomic_type of plcType |
         Type of plcType |
         Types of plcType list |
         Const of expr |
         Comps of expr list

%right T_SEMICOLON T_MINUS_ARROW
%nonassoc T_IF 
%left T_ELSE T_AND_DCE T_EQUAL T_DIFF T_SMALLER T_SMALLER_EQUAL
%right T_CONCAT_DC
%left T_PLUS T_MINUS T_MUL T_DIV
%nonassoc T_EXCL T_HD T_TL T_ISE T_PRINT NAME
%left T_OPEN_BRACES

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) |
       Decl T_SEMICOLON Prog (resolve (Decl, Prog))

Decl : T_VAR NAME T_EQUAL Expr (VarDecl(NAME, Expr)) |
       T_FUN NAME Args T_EQUAL Expr (FunDecl(NAME, makeAnon(Args, Expr))) |
       T_FUN T_REC NAME Args T_COLON Type T_EQUAL Expr (FunrecDecl(NAME, Args, Type, Expr))

Match_expr : T_END ([]) |
             T_PIPE Cond_expr T_MINUS_ARROW Expr Match_expr ([(Cond_expr, Expr)] @ Match_expr)

Cond_expr : Expr (SOME Expr) |
            T_UNDERSCORE (NONE)

Expr : Atomic_expr (Atomic_expr) |
       App_expr (App_expr) |
       T_IF Expr T_THEN Expr T_ELSE Expr (If (Expr1, Expr2, Expr3)) |
       T_MATCH Expr T_WITH Match_expr (Match(Expr, Match_expr)) |
       T_EXCL Expr (Prim1("!", Expr)) |
       T_MINUS Expr (Prim1("-", Expr)) |
       T_HD Expr (Prim1("hd", Expr)) |
       T_TL Expr (Prim1("tl", Expr)) |
       T_ISE Expr (Prim1("ise", Expr)) |
       T_PRINT Expr (Prim1("print", Expr)) |
       Expr T_AND_DCE Expr (Prim2("&&", Expr1, Expr2)) |
       Expr T_PLUS Expr (Prim2("+", Expr1, Expr2)) |
       Expr T_MINUS Expr (Prim2("-", Expr1, Expr2)) |
       Expr T_MUL Expr (Prim2("*", Expr1, Expr2)) |
       Expr T_DIV Expr (Prim2("/", Expr1, Expr2)) |
       Expr T_EQUAL Expr (Prim2("=", Expr1, Expr2)) |
       Expr T_DIFF Expr (Prim2("!=", Expr1, Expr2)) |
       Expr T_SMALLER Expr (Prim2("<", Expr1, Expr2)) |
       Expr T_SMALLER_EQUAL Expr (Prim2("<=", Expr1, Expr2)) |
       Expr T_CONCAT_DC Expr (Prim2("::", Expr1, Expr2)) |
       Expr T_SEMICOLON Expr (Prim2(";", Expr1, Expr2)) |
       Expr T_OPEN_BRACES NAT T_CLOSE_BRACES (Item(NAT, Expr))

App_expr : Atomic_expr Atomic_expr (Call(Atomic_expr1, Atomic_expr2)) |
           App_expr Atomic_expr (Call(App_expr, Atomic_expr))

Atomic_expr : Const (Const) |
              NAME (Var NAME) |
              T_OPEN_KEYS Prog T_CLOSE_KEYS (Prog) |
              T_OPEN_PAR Expr T_CLOSE_PAR (Expr) |
              T_OPEN_PAR Comps T_CLOSE_PAR (List Comps) |
              T_FN Args T_EQUAL_ARROW Expr T_END (makeAnon(Args, Expr))

Const : T_TRUE (ConB true) |
        T_FALSE (ConB false) |
        NAT (ConI NAT) |
        T_OPEN_PAR T_CLOSE_PAR (List []) |
        T_OPEN_PAR Type T_OPEN_BRACES T_CLOSE_BRACES T_CLOSE_PAR (ESeq Type)

Comps : Expr T_COMMA Expr ([Expr1, Expr2]) |
        Expr T_COMMA Comps ([Expr] @ Comps)

Args : T_OPEN_PAR T_CLOSE_PAR ([]) |
       T_OPEN_PAR Params T_CLOSE_PAR (Params)

Params : Typed_var ([Typed_var]) |
         Typed_var T_COMMA Params ([Typed_var] @ Params)

Typed_var : Type NAME ((Type, NAME))

Types : Type T_COMMA Type ([Type1, Type2]) |
        Type T_COMMA Types ([Type] @ Types)

Type : Atomic_type (Atomic_type) |
       T_OPEN_PAR Types T_CLOSE_PAR (ListT Types) |
       T_OPEN_BRACES Type T_CLOSE_BRACES (SeqT Type) |
       Type T_MINUS_ARROW Type (FunT (Type1, Type2))

Atomic_type : T_NIL (ListT []) |
              T_BOOL (BoolT) |
              T_INT (IntT) |
              T_OPEN_PAR Type T_CLOSE_PAR (Type)
