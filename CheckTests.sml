use "Plc.sml";

teval (fromString "15") [];
teval (fromString "true") [];
teval (fromString "()") [];
teval (fromString "(6,false)") [];
teval (fromString "(6,false)[1]") [];
teval (fromString "(6,false)[2]") [];
teval (fromString "(6,x)[2]") [("x", SeqT IntT)];
teval (fromString "([Bool] [])") [];
teval (fromString "print x; true") [("x", BoolT)];
teval (fromString "fn (Int x) => -x end") [];
teval (fromString "var x = 9; x + 3") [];
teval (fromString "fun f(Int x) = x; f(1)") [];
teval (fromString "(x, y, z)[1]") [("x", IntT), ("y", IntT), ("z", IntT)];
teval (fromString "(x, y, z)[2]") [("x", IntT), ("y", IntT), ("z", IntT)];
teval (fromString "(x, y, z)[3]") [("x", IntT), ("y", IntT), ("z", IntT)];
teval (fromString "var func1 = fn (Int x) => 2*x end; var func2 = fn (Int x) => 3*x end; var funcList = (func1, func2); var myF = funcList[1]; myF(5)") [];
teval (fromString "-5") [];
teval (fromString "-x") [("x", IntT)];
teval (fromString "true && false") [];
teval (fromString "true && true") [];
teval (fromString "5 + 5") [];
teval (fromString "print x") [("x", IntT)];
teval (fromString "print y; print x") [("x", IntT), ("y", IntT)];
teval (fromString "print y; x + 8") [("x", IntT), ("y", IntT)];
teval (fromString "()") [];
teval (fromString "([Int] [])") [];
teval (fromString "1::2::3::4::([Int] [])") [];
teval (fromString "(1,2)::(2,3)::(3,4)::(4,5)::([(Int,Int)] [])") [];
teval (fromString "(1,true)::(2,false)::(3,true)::(4,false)::([(Int,Bool)] [])") [];
teval (fromString "(false, 8)::(false, 9)::(true, 1)::(true, 2)::([(Bool, Int)] [])") [];
teval (fromString "x::y::z::w") [("x", IntT), ("y", IntT), ("z", IntT), ("w", SeqT IntT)];
teval (fromString "if x = z then y + 7 else w + 7") [("x", IntT), ("y", IntT), ("z", IntT), ("w", IntT)];
teval (fromString "match x with | 0 -> 1| _ -> -1 end") [("x", IntT)];
teval (fromString "match x with | 0 -> 1 end") [("x", IntT)];
teval (fromString "var x = 3; if x < 2 then x else y") [("y", IntT)];
teval (fromString "var x = 3; if x < 4 then x else y") [("y", IntT)];
teval (fromString "var x = (1,2,3,4); print x; x[2]") [];
teval (fromString "fun f(Int x) = x; print 5") [];
teval (fromString "fun f(Int x, Int y) = x + y; f(1,2)") [];
teval (fromString "var a = 35; fun f(Int x, Int y) = x + y + a; f(1,2)") [];
teval (fromString "fun f() = 1; f()") [];
teval (fromString "fun f(Int x) = x + 3; f(1)") [];
teval (fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)") [];
teval (fromString "fun rec f(Int n, Int m):Int = if n <= 0 then 0 else m + f(n-1, m); f(5, 8)") [];

let
    val test = teval (fromString "3::7::t") [("t", IntT)]
in
    print("ERROR: UnknownType exception should have been raised.\n")
end handle UnknownType => print ("INFO: Expected exception. Can't use :: without a list as initial element.\n");

let
    val test = teval (fromString "(Int [])") [];
in
    print("ERROR: EmptySeq exception should have been raised.\n")
end handle EmptySeq => print ("INFO: Expected exception. Sequences must declare their sequence type like [plcType].\n");

let
    val test = teval (fromString "match x with | 0 -> true | _ -> 1 end") [("x", IntT)]
in
    print("ERROR: MatchResTypeDiff exception should have been raised.\n")
end handle MatchResTypeDiff => print ("INFO: Expected exception. Match result types are different.\n");

let
    val test = teval (fromString "match x with | true -> 1| _ -> -1 end") [("x", IntT)]
in
    print("ERROR: MatchCondTypesDiff exception should have been raised.\n")
end handle MatchCondTypesDiff => print ("INFO: Expected exception. Match conditions types are different.\n");

let
    val test = teval (fromString "if 5 then 8 else 3") []
in
    print("ERROR: IfCondNotBool exception should have been raised.\n")
end handle IfCondNotBool => print ("INFO: Expected exception. If condition is not bool.\n");

let
    val test = teval (fromString "if true then 8 else false") []
in
    print("ERROR: DiffBrTypes exception should have been raised.\n")
end handle DiffBrTypes => print ("INFO: Expected exception. If branches have different types.\n");

let
    val test = teval (fromString "(6,false)[3]") []
in
    print("ERROR: ListOutOfRange exception should have been raised.\n")
end handle ListOutOfRange => print ("INFO: Expected exception. Trying to access 3rd element from 2 element list.\n");

let
    val test = teval (fromString "if true != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR: NotEqTypes exception should have been raised.\n")
end handle NotEqTypes => print ("INFO: Expected exception. Comparision done on different types.\n");

let
    val test = teval (fromString "fun rec f(Int n, Int m):Bool = if n != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR: WrongRetType exception should have been raised.\n")
end handle WrongRetType => print ("INFO: Expected exception. Function return type does not agree with function body.\n");

let
    val test = teval (fromString "var x = 3; x(1)") []
in
    print("ERROR: CallTypeMisM exception should have been raised.\n")
end handle CallTypeMisM => print ("INFO: Expected exception. Variable that is not a function is being called.\n");

let
    val test = teval (fromString "var x = 3; x[1]") []
in
    print("ERROR: OpNonList exception should have been raised.\n")
end handle OpNonList => print ("INFO: Expected exception. Variable that is not a list is being indexed.\n");

print("INFO: Checker testing complete!\n")