use "Plc.sml";

eval (fromString "15") [];
eval (fromString "true") [];
eval (fromString "()") [];
eval (fromString "(6,false)[1]") [];
eval (fromString "([Bool] [])") [];
eval (fromString "print x; true") [("x", BoolV false)];
eval (fromString "var x = 9; x + 3") [];
eval (fromString "var func = fn (Int x) => -x end; func(1)") [];
eval (fromString "fun f(Int x) = 10-x; f(1)") [];
eval (fromString "(x, y, z)[1]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "(x, y, z)[2]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "(x, y, z)[3]") [("x", IntV 5), ("y", IntV 10), ("z", IntV 15)];
eval (fromString "var func1 = fn (Int x) => 2*x end; var func2 = fn (Int x) => 3*x end; var funcList = (func1, func2); var myF = funcList[1]; myF(5)") [];
eval (fromString "-5") [];
eval (fromString "-x") [("x", IntV 8)];
eval (fromString "true && false") [];
eval (fromString "true && true") [];
eval (fromString "5 + 5") [];
eval (fromString "print y; print x") [("x", IntV 3), ("y", IntV 9)];
eval (fromString "print y; x + 8") [("x", IntV 3), ("y", IntV 9)];
eval (fromString "1::2::3::4::([Int] [])") [];
eval (fromString "(1,2)::(2,3)::(3,4)::(4,5)::([(Int,Int)] [])") [];
eval (fromString "(1,true)::(2,false)::(3,true)::(4,false)::([(Int,Bool)] [])") [];
eval (fromString "x::y::([Int] [])") [("x", IntV 10), ("y", IntV 9)];
eval (fromString "x::y::z::w") [("x", IntV 10), ("y", IntV 9), ("z", IntV 8), ("w", SeqV [])];
eval (fromString "if 5 = 5 then 8 + 3 else 8 - 3") [];
eval (fromString "if x = z then y + 7 else w + 7") [("x", IntV 10), ("y", IntV 9), ("z", IntV 8), ("w", IntV 7)];
eval (fromString "if x = z then y + 7 else w + 7") [("x", IntV 10), ("y", IntV 9), ("z", IntV 10), ("w", IntV 7)];
eval (fromString "if x = z then true else w + 7") [("x", IntV 10), ("y", IntV 9), ("z", IntV 10), ("w", IntV 7)];
eval (fromString "match x with | 0 -> 1| _ -> -1 end") [("x", IntV 3)];
eval (fromString "match x with | 0 -> 1| _ -> -1 end") [("x", IntV 0)];
eval (fromString "var x = 3; if x < 2 then x else y") [("y", IntV 9)];
eval (fromString "var x = 3; if x < 4 then x else y") [("y", IntV 9)];
eval (fromString "var x = (1,2,3,4); print x; x[2]") [];
eval (fromString "fun f(Int x, Int y) = x + y; f(1,2)") [];
eval (fromString "var a = 35; fun f(Int x, Int y) = x + y + a; f(1,2)") [];
eval (fromString "fun f() = 1; f()") [];
eval (fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)") [];
eval (fromString "fun rec f(Int n, Int m):Int = if n <= 0 then 0 else m + f(n-1, m); f(5, 8)") [];

let 
    val test = eval (fromString "3::7::t") [("t", IntV 19)]
in
    print("ERROR: Impossible exception should have been raised.\n")
end handle Impossible => print ("INFO: Expected exception. Can't use :: without a list as initial element.\n");

let 
    val test = eval (fromString "hd ([Int] [])") []
in
    print("ERROR: HDEmptySeq exception should have been raised.\n")
end handle HDEmptySeq => print ("INFO: Expected exception. Trying to access head of an empty sequence.\n");

let 
    val test = eval (fromString "tl ([Int] [])") []
in
    print("ERROR: TLEmptySeq exception should have been raised.\n")
end handle TLEmptySeq => print ("INFO: Expected exception. Trying to access tail of an empty sequence.\n");

let 
    val test = eval (fromString "var x = 3; x(1)") []
in
    print("ERROR: NotAFunc exception should have been raised.\n")
end handle NotAFunc => print ("INFO: Expected exception. Variable that is not a function is being called.\n");

let 
    val test = eval (fromString "match x with | 0 -> 1 end") [("x", IntV 3)]
in
    print("ERROR: ValueNotFoundInMatch exception should have been raised.\n")
end handle ValueNotFoundInMatch => print ("INFO: Expected exception. Could not find value in match.\n");

print("INFO: Interpreter testing complete!\n")