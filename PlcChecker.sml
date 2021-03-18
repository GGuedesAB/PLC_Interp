(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (ConI _) _ = IntT
  | teval (ConB _) _ = BoolT
  | teval (ESeq s) _ =
    let in
      case s of
          SeqT t => SeqT t
        | _ => raise EmptySeq
    end
  | teval (Var v) (env:plcType env) = lookup env v
  | teval (List l) (env:plcType env) =
    let
      fun checkEachElement (x::[]) = (teval x env)::[]
        | checkEachElement (x::xs) = (teval x env)::checkEachElement xs
        | checkEachElement _ = []
      val lst = checkEachElement l
    in
      ListT lst
    end
  | teval (Item (index, exp)) (env:plcType env) =
    let
      fun getElementI (i, []) = raise ListOutOfRange
        | getElementI (i, (x::[])) = if i = 1 then x else raise ListOutOfRange
        | getElementI (i, (x::xs)) = if i = 1 then x else getElementI (i - 1, xs)
      val vType = teval exp env
    in
      case vType of
          ListT l => getElementI(index, l)
        | _ => raise OpNonList
    end
  | teval (If(exp1, exp2, exp3)) (env:plcType env) =
    let
      val condType = teval exp1 env
      val exp2Type = teval exp2 env
      val exp3Type = teval exp3 env
    in
      case condType of
          BoolT => if exp2Type = exp3Type then exp2Type else raise DiffBrTypes
        | _ => raise IfCondNotBool
    end
  | teval (Prim1(oper, exp)) (env:plcType env) =
    let
      val expType = teval exp env
    in
      case oper of
          "!" => if expType = BoolT then BoolT else raise UnknownType
        | "-" => if expType = IntT then IntT else raise UnknownType
        | "hd" => let in
            case expType of
                SeqT t => t
              | _ => raise OpNonList
          end
        | "tl" => let in
            case expType of
                SeqT t => SeqT t
              | _ => raise OpNonList
          end
        | "ise" => let in
            case expType of
                SeqT t => BoolT
              | _ => raise OpNonList
          end
        | "print" => ListT []
        | _ => raise UnknownType
    end
  | teval (Prim2(oper, exp1, exp2)) (env:plcType env) =
    let
      val exp1Type = teval exp1 env
      val exp2Type = teval exp2 env
    in
      case oper of
          "&&" => if exp1Type = BoolT andalso exp2Type = BoolT then BoolT else raise UnknownType
        | "::" => let in
            case (exp1Type, exp2Type) of
                (IntT, ListT []) => SeqT IntT
              | (IntT, SeqT t2) => if t2 = IntT then SeqT t2 else raise NotEqTypes
              | (BoolT, ListT []) => SeqT BoolT
              | (BoolT, SeqT t2) => if t2 = BoolT then SeqT t2 else raise NotEqTypes
              | (ListT t, ListT []) => SeqT (ListT t)
              | (ListT t, SeqT t2) => if t2 = ListT t then SeqT t2 else raise NotEqTypes
              | _ => raise UnknownType
          end
        | "+" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
        | "-" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
        | "*" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
        | "/" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
        | "<" => if exp1Type = IntT andalso exp2Type = IntT then BoolT else raise UnknownType
        | "<=" => if exp1Type = IntT andalso exp2Type = IntT then BoolT else raise UnknownType
        | "=" => if exp1Type = exp2Type andalso (exp1Type = IntT orelse exp1Type = BoolT) then BoolT else raise NotEqTypes
        | "!=" => if exp1Type = exp2Type andalso (exp1Type = IntT orelse exp1Type = BoolT) then BoolT else raise NotEqTypes
        | ";" => exp2Type
        | _ => raise UnknownType
    end
  | teval (Let(var, exp1, exp2)) (env:plcType env) =
    let
      val exp1Type = teval exp1 env
      val nEnv = (var, exp1Type) :: env
    in
      teval exp2 nEnv
    end
  | teval (Anon(typ, arg, exp)) (env:plcType env) = 
    let
      val nEnv = (arg, typ) :: env
      val expType = teval exp nEnv
    in
      FunT (typ, expType)
    end
  | teval (Call(exp2, exp1)) (env:plcType env) =
    let
      val exp1Type = teval exp1 env
      val exp2Type = teval exp2 env
    in
      case exp2Type of
          FunT (typ1, exp2Type) => if typ1 = exp1Type then exp2Type else raise WrongRetType
        | _ => raise CallTypeMisM
    end
  | teval (Letrec(fName, argTyp, arg, funTyp, exp1, exp2)) (env:plcType env) =
    let
      val recEnv = (fName, FunT (argTyp, funTyp))
      val argEnv = (arg, argTyp)
      val exp1Type = teval exp1 (recEnv :: argEnv :: env)
      val exp2Type = teval exp2 (recEnv :: env)
    in
      if exp1Type = funTyp then exp2Type else raise WrongRetType
    end
  | teval (Match(exp1, matchList)) (env:plcType env) =
    let
      val initialCond = teval exp1 env
      val firstRes = (#2 (hd matchList))
      val firstResType = teval firstRes env
      fun searchMatch (Match(exp1, matchList)) (env:plcType env) =
          let in
            case matchList of
                x::[] => let in
                    case x of
                        (SOME exp2, exp3) => 
                          if (teval exp3 env) = firstResType then
                            if initialCond = (teval exp2 env) then 
                              teval exp3 env 
                            else raise MatchCondTypesDiff
                          else raise MatchResTypeDiff
                      | (NONE, exp3) => if (teval exp3 env) = firstResType then firstResType else raise MatchResTypeDiff
                  end
              | x::xs => let in
                    case x of
                        (SOME exp2, exp3) => 
                          if (teval exp3 env) = firstResType then
                            if initialCond = (teval exp2 env) then
                              searchMatch (Match(exp1, xs)) env 
                            else raise MatchCondTypesDiff
                          else raise MatchResTypeDiff
                      | _ => raise UnknownType
                  end
              | _ => raise UnknownType
          end
        | searchMatch _ _ = raise UnknownType
    in
      searchMatch (Match(exp1, matchList)) env
    end
