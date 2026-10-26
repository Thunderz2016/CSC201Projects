(* CSC 201, Team 2, Members: Jose Vasquez, Nitin Kowdle, Wenshen Zhong *)

(* Sample 2: PROGRAM isHarshad
    {
        num Integer;
        rem Integer;
        sum Integer;
        n Integer;
        result Boolean;

        num = 156;
        rem = 0;
        sum = 0;
        n = num;

    WHILE (num Gt 0)
    {
        rem = (num Minus 10 Times (num Div 10));
        sum = sum Plus rem;
        num = num Div 10;
    }

    IF ((n Minus sum Times (n Div sum)) Eq 0) THEN result = True;
    ELSE result = False;
    }
*)


(*<variable>*) 
datatype variable = var of string; 
(*<IntegerConstant>*)
type integerConstant = int;
(*<BooleanConstant>*)
type booleanConstant = bool; 

(*<ArithmaticOp>*)
datatype arithmaticOp = Plus| Minus| Times| Div;
(*<RelationalOp>*)
datatype relationalOp = Lt| Le | Eq | Ne | Ge | Gt; 
(*<BooleanOp>*)
datatype booleanOp = And | Or; 

(*<IntegerExpression>*)
datatype integerExpression = intExp1 of integerConstant |
                            intExp2 of variable |
                            intExp3 of (arithmaticOp * integerExpression * integerExpression);

(*<BooleanExpression*)
datatype booleanExpression = boolExp1 of booleanConstant |
                            boolExp2 of variable |
                            boolExp3 of (relationalOp * integerExpression * integerExpression) |
                            boolExp4 of (booleanOp * booleanExpression * booleanExpression);

(*<Instruction>*)
datatype instruction = Skip |
                        inst2 of (variable * integerExpression) |
                        inst3 of (variable * booleanExpression) |
                        inst4 of instruction list | (*sequence*)
                        inst5 of (instruction * instruction * booleanExpression) |  (*if/else*)
                        inst6 of (instruction * booleanExpression);  (*for while loop*)

(*<TypeName>*)
datatype typeName = TypeName1Boolean | TypeName2Integer;

(*<Declaration>*)
type declaration = variable * typeName;


(*<DeclarationList>*)
type declarationList = declaration list;

(*<Program>*)
type program = declarationList * instruction;

(* Create the variables *)
val v_num = var "num";
val v_rem = var "rem";
val v_sum = var "sum";
val v_n = var "n";
val v_result = var "result";

(* Declare them *)
val declarations : declarationList =
    [(v_num, TypeName2Integer),
     (v_rem, TypeName2Integer),
     (v_sum, TypeName2Integer),
     (v_n, TypeName2Integer),
     (v_result, TypeName1Boolean)];

(* Create the Integer Contraints and Expressions *)
val int_156 : integerExpression =
    intExp1 156;
val int_0 : integerExpression =
    intExp1 0;
val int_10 : integerExpression = 
    intExp1 10;

val exp_num : integerExpression =
    intExp2 v_num;
val exp_rem : integerExpression =
    intExp2 v_rem;
val exp_sum : integerExpression =
    intExp2 v_sum;
val exp_n : integerExpression =
    intExp2 v_n;

(* Assign the integer values *)
val num_156 : instruction =
    inst2 (v_num, intExp1 156);
val rem_0 : instruction =
    inst2 (v_rem, intExp1 0);
val sum_0 : instruction =
    inst2 (v_sum, intExp1 0);
val n_num : instruction =
    inst2 (v_n, intExp2 v_num);

(* WHILE*)
val whileLoopBoolExp : booleanExpression =
    boolExp3 (Gt, exp_num, int_0);

val numDivTen : integerExpression =
    intExp3 (Div, exp_num, int_10);
val tenTimesNumDivTen : integerExpression =
    intExp3 (Times, int_10, numDivTen);
val numMinusTenTimesNumDivTen : integerExpression =
    intExp3 (Minus, exp_num, tenTimesNumDivTen);  (*num Minus 10 Times (num Div 10)*)

val sumPlusRem : integerExpression =
    intExp3 (Plus, exp_sum, exp_rem);

val whileLoopInstList : instruction = 
    inst4 ([ inst2 (v_rem, numMinusTenTimesNumDivTen),   (*rem = (num Minus 10 Times (num Div 10))*)
             inst2 (v_sum, sumPlusRem), (*sum = sum Plus rem*)
             inst2 (v_num, numDivTen)]);   (*num = num Div 10*)
val whileLoop : instruction =
    inst6 (whileLoopInstList, whileLoopBoolExp);


(* IF/ELSE *)

(* n Div sum *)
val nDivSum : integerExpression =
    intExp3 (Div, exp_n, exp_sum);

(* sum Times (n Div sum) *)
val sumTimesNDivSum : integerExpression =
    intExp3 (Times, exp_sum, nDivSum);

(* n Minus sum Times (n Div sum) *)
val nMinusSumTimesNDivSum : integerExpression =
    intExp3 (Minus, exp_n, sumTimesNDivSum);

(* (n Minus sum Times (n Div sum)) Eq 0 *)
val ifCondition : booleanExpression =
    boolExp3 (Eq, nMinusSumTimesNDivSum, int_0);

(* THEN result = True *)
val thenInstruction : instruction =
    inst3 (v_result, boolExp1 true);

(* ELSE result = False *)
val elseInstruction : instruction =
    inst3 (v_result, boolExp1 false);

(* Complete IF/ELSE *)
val ifElseInstruction : instruction =
    inst5 (thenInstruction, elseInstruction, ifCondition);

(*Last Line: val teamTwoProgram = (declist, programbody) : program *)

val programBody : instruction = 
    inst4([num_156, rem_0, sum_0, n_num, whileLoop, ifElseInstruction]);

val teamTwoProgram =  (declarations,programBody): program;              


(*------------Step Two------------*)

(*Validity check for declaration list*)
(*1. varNotInDecList*)
val rec varNotInDecList = (fn (x : variable) =>
                                (fn ((y, z) :: decListTail) : declarationList =>
                                (x <> y) andalso varNotInDecList (x) (decListTail) |
                                ([]) => true)
                            );

(*2. validDecList*)
fun validDecList ([]) = true |
    validDecList ((x : variable, y : typeName) :: decListTail) =
        validDecList (decListTail) andalso varNotInDecList (x) (decListTail);

(*3*)
datatype typeInternal = IntRep | BoolRep | Not;

(*4*)
type SymbolTable = variable -> typeInternal

(*5*)
(* Symbol Table Initial - Nitin*)
fun SymbolTableInitial(x : variable) = NotDecRep

(*6*)
(* Symbol Table AddOne - Nitin*)
val SymbolTableAddOne =
    (fn(oldmap:SymbolTable) =>
        (fn ((x, TypeName1Boolean) : Declaration) =>
            (fn(anyvar variable) =>
                if anyvar = x then
                    BoolRep
                else
                    oldmap(anyvar)
            )|
        ((y,TypeName2Int) = Declaration) =>
            (fn(anyvar varaible) =>
                if anyvar = y then
                    BoolRep
                else
                    oldmap(anyvar)
            )
        )
    )
(*Testing for section 6*)
val MySymbolTable1 = SymbolTableAddOne(SymbolTableInitial);
MySymbolTable1(v_num);
list.map MySymbolTable1(tl declarations);
val MySymbolTable2 = SymbolTableAddOne(MySymbolTable1);
MySymbolTable2(v_num);
MySymbolTable2(v_rem);
list.map MySymbolTable1(tl(tl declarations));

(*7*)
(* Symbol Table Complete - Nitin*)
fun SymbolTableComplete([]) => SymbolTableInitial |
SymbolTableComplete((decListHead::decListTail)
    SymbolTableAddOne : DeclarationList) =
        (SymbolTableComplete(decListTail))
        (decListHead)
(*Testing for section 7*)
var MySymbolTable = SymbolTableComplete(validDecList);
list.map MySymbolTable(declarations);
MySymbolTable(notreal);

(*8*)
(* Valid Int Exp - Nitin*)
val rec ValidIntExp =
    (fn (DCIE1(x)) => (fn(typemap : SymbolTable) => true) |
        (DCIE2(y)) => (fn(typemap : SymbolTable) => (typemap(y) = IntRep)) |
        (DCIE3(arithmaticOp, iExp1, iExp2) =>
            (fn(typemap : SymbolTable) =>
                validIntExp(iExp1)(typemap) andalso validIntExp(iExp2)(typemap)
            )
        )
    )

(*9*)
(* Valid Bool Exp -Jose*)
val rec validBoolExp =
    ( fn (typemap:SymbolTable) =>
        (fn DCBE1(x) => true 
      |  DCBE2(y) => (typemap(y) = BoolRep) 
      |  DCBE3 (relationalOp , iExp1 , iExp2)=>
                        validIntExp (iExp1)(typemap) andalso
                        validIntExp (iExp2)(typemap) 
      |  DCBE4 (boolOp, boolExp1, boolExp2) =>
                        validBoolExp(typemap)(boolExp1) andalso
                        validBoolExp (typemap)(boolExp2)
                    )
                );
val testTable : SymbolTable =
    fn (var "num") => IntRep
     | (var "sum") => IntRep
     | (var "result") => BoolRep
     | _ => Not;
(*Testing for section 9*)
val test9_1 = validBoolExp testTable(boolExp1 true);
val test9_2 = validBoolExp testTable(boolExp2(var "result"));
val test9_2 = validBoolExp testTable(boolExp2(var "num"));


(*10*)
(* valid Instruction - Jose *)
val rec validInstruction =
    ( fn Skip => ( fn (typemap: SymbolTable) => true) 
        |DCI2( var, IntExp) => (fn(typemap:SymbolTable) =>
                                (typemap(var)= IntRep) andalso
                                validIntExp (IntExp)(typemap))
        |DCI3 (var, boolExp) => (fn(typemap: SymbolTable) => 
                                (typemap(var) = BoolRep) andalso
                                validBoolExp (typemap)(boolExp))
        |DCI4(intListHead :: intListTail) => ( fn( typemap: SymbolTable) => 
                                                validInstruction(intListHead)(typemap) andalso
                                                validInstruction( DCI4(intListTail))(typemap))
        | DCI4([]) => ( fn (typemap: SymbolTable) => true) 
        |DCI5 (truthbranch, falsebranch, boolExp) =>
            (fn(typemap: SymbolTable) =>
                validInstruction(truthbranch)(typemap) andalso
                validInstruction(falsebranch)(typemap) andalso
                validBoolExp (typemap)(boolExp))
        |DCI6 (loopbody, boolExp) => ( fn (typemap: SymbolTable)=> 
                                        validInstruction(loopbody)(typemap) andalso
                                        validBoolExp(typemap)(boolExp))
        );

(*Testing for section 10*)
val test10_1 = 
    validInstruction 
        (inst2(var "num", intExp1 10))
        testTable;
(*11*)
(* Exception Invalid Declist - Jose *)
Exception InvalidDecList

(*12*)
(* Exception Invalid Program - Jose *)
Exception InvalidProgram 

(*13*)
(* Valid Program *)
val validProgram =
    (fn (decList, programBody): program =>
    if validDecList(decList) <> true
    then raise InvalidDecList
    else 
        if validInstruction(programBody)(SymbolTableComplete(decList))
        then true 
        else raise InvalidProgram 
    );
