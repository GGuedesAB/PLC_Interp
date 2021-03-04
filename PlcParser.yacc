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
         Expr of expr |
         Atomic_expr of expr |
         App_expr of expr |
         Match_expr of expr |
         Cond_expr of expr |
         Decl of expr |
         Args of plcType |
         Typed_var of plcType * string |
         Params of (plcType * string) list |
         Atomic_type of plcType |
         Type of plcType |
         Types of plcType |
         Const of plcVal |
         Comps of expr


%right T_SEMICOLON T_MINUS_ARROW T_CONCAT_DC

%left T_ELSE T_AND_DCE T_EQUAL T_DIFF T_SMALLER T_SMALLER_EQUAL T_PLUS T_MINUS T_MUL T_DIV T_OPEN_BRACES

%nonassoc T_EXCL T_HD T_TL T_ISE T_PRINT NAME

%left

%eop EOF

%noshift EOF

%start Prog

%%

(*Prog : Expr (Expr) |
       Decl T_SEMICOLON Prog (Decl)

Decl : T_VAR NAME T_EQUAL Expr () |
       T_FUN NAME Args T_EQUAL Expr () |
       T_FUN T_REC NAME Args T_COLON Type T_EQUAL Expr ()

Expr : Atomic_expr () |
       App_expr () |
       T_IF Expr T_THEN Expr T_ELSE Expr () |
       T_MATCH Expr T_WITH Match_expr () |
       T_EXCL Expr () |
       T_MINUS Expr () |
       T_HD Expr () |
       T_TL Expr () |
       T_ISE Expr () |
       T_PRINT Expr () |
       Expr T_AND_DCE Expr () |
       Expr T_PLUS Expr () |
       Expr T_MINUS Expr () |
       Expr T_MUL Expr () |
       Expr T_DIV Expr () |
       Expr T_EQUAL Expr () |
       Expr T_DIFF Expr () |
       Expr T_SMALLER Expr () |
       Expr T_SMALLER_EQUAL Expr () |
       Expr T_CONCAT_DC Expr () |
       Expr T_SEMICOLON Expr () |
       Expr T_OPEN_BRACES NAT T_CLOSE_BRACES ()

Atomic_expr : Const () |
              NAME () |
              T_OPEN_KEYS Prog T_CLOSE_KEYS () |
              T_OPEN_PAR Expr T_CLOSE_PAR () |
              T_OPEN_PAR Comps T_CLOSE_PAR () |
              T_FN Args T_EQUAL_ARROW Expr T_END ()

App_expr : Atomic_expr Atomic_expr () |
           App_expr Atomic_expr ()

Const : T_TRUE (ConB true) |
        T_FALSE (ConB false) |
        NAT (ConI NAT) |
        T_OPEN_PAR T_CLOSE_PAR (List []) |
        T_OPEN_PAR Type T_OPEN_BRACES T_CLOSE_BRACES T_CLOSE_PAR (ESeq [SeqT Type])

Comps : Expr T_COMMA Expr (Expr1, Expr2) |
        Expr T_COMMA Comps (Expr, Comps)

Match_expr : T_END () |
             T_PIPE Cond_expr T_MINUS_ARROW Expr Match_expr (Cond_expr, Expr)

Cond_expr : Expr (Some Expr) |
            T_UNDERSCORE (None)

Args : T_OPEN_PAR T_CLOSE_PAR (List []) |
       T_OPEN_PAR Params T_CLOSE_PAR (List [Params])*)

Params : Typed_var ([Typed_var]) |
         Typed_var T_COMMA Params (Params @ [Typed_var])

Typed_var : Type NAME ((Type, NAME))

Types : Type T_COMMA Type (Type1; Type2) |
        Type T_COMMA Types (Type; Types)

Type : Atomic_type (Atomic_type) |
       T_OPEN_PAR Types T_CLOSE_PAR (ListT[Types]) |
       T_OPEN_BRACES Type T_CLOSE_BRACES (SeqT Type) |
       Type T_MINUS_ARROW Type (FunT (Type1, Type2))

Atomic_type : T_NIL (ListT []) |
              T_BOOL (BoolT) |
              T_INT (IntT) |
              T_OPEN_PAR Type T_CLOSE_PAR (Type)
