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
(*1. varNotInDecList: variable -> DeclarationList -> Bool - Wenshen*)
val rec varNotInDecList = (fn (x : variable) =>
                                (fn ((y, z) :: decListTail) : declarationList =>
                                (x <> y) andalso varNotInDecList (x) (decListTail) |
                                ([]) => true)
                            );

(*2. validDecList: DeclarationList -> Bool - Wenshen*)
fun validDecList ([]) = true |
    validDecList ((x : variable, y : typeName) :: decListTail) =
        validDecList (decListTail) andalso varNotInDecList (x) (decListTail);

(*Testing section 2*)
print("\n--------Testing section 2---------\n");

(*Good case: just the sample's dec list*)
validDecList (declarations);

(*Bad case: redundant declaration*)
val badDecList : declarationList = 
    [(v_num, TypeName2Integer),
     (v_num, TypeName2Integer),      (*redunant*)
     (v_sum, TypeName2Integer),
     (v_n, TypeName2Integer),
     (v_result, TypeName1Boolean)];
validDecList(badDecList);

print("\n--------End Testing section 2---------\n\n");

(*3 Type Internal - Wenshen*)
datatype typeInternal = IntRep | BoolRep | Not;

(*4 Symbol Table declaration - Wenshen*)
type SymbolTable = variable -> typeInternal;

(*5*)
(* Symbol Table Initial: SymbolTable - Nitin*)
fun SymbolTableInitial(x : variable) = Not;

(*6*)
(* Symbol Table AddOne: SymbolTable -> declaration -> SymbolTable - Nitin & Wenshen*)
val SymbolTableAddOne =
    (fn(oldmap : SymbolTable) =>
        (fn ((x : variable, TypeName1Boolean) : declaration) =>
            (fn(anyvar : variable) =>
                if anyvar = x then
                    BoolRep
                else
                    oldmap(anyvar)
            )|
        ((y : variable, TypeName2Integer) : declaration) =>
            (fn (anyvar : variable) =>
                if anyvar = y then
                    IntRep
                else
                    oldmap(anyvar)
            )
        )
    );

(*Testing for section 6*)
print("\n--------Testing section 6---------\n");
(*Test first declaration in sample*)
val MySymbolTable1 = SymbolTableAddOne(SymbolTableInitial) ((v_num, TypeName2Integer));
MySymbolTable1(v_num);
MySymbolTable1(v_rem);
MySymbolTable1(v_sum);
MySymbolTable1(v_n);
MySymbolTable1(v_result);

(*Test 2nd declaration in sample*)
val MySymbolTable2 = SymbolTableAddOne(MySymbolTable1) (v_rem, TypeName2Integer);
MySymbolTable2(v_num);
MySymbolTable2(v_rem);
MySymbolTable2(v_sum);
MySymbolTable2(v_n);
MySymbolTable2(v_result);
print("--------End Testing section 6---------\n\n");

(*7*)
(* Symbol Table Complete: Declarationlist -> SymbolTable - Nitin & Wenshen *)
fun SymbolTableComplete([]) = SymbolTableInitial |
    SymbolTableComplete((decListHead::decListTail) : declarationList) =
        SymbolTableAddOne (SymbolTableComplete (decListTail)) (decListHead);

(*Testing for section 7*)
print("\n--------Testing section 7---------\n");
val MySymbolTable = SymbolTableComplete(declarations);
MySymbolTable(v_num);
MySymbolTable(v_rem);
MySymbolTable(v_sum);
MySymbolTable(v_n);
MySymbolTable(v_result);
(* MySymbolTable(notreal); *)  (*var not in sample*)
print("--------End Testing section 7---------\n\n");

(*8*)
(* Valid Int Exp: IntegerExpression -> SymbolTable -> Bool - Nitin & Wenshen*)
val rec validIntExp =
    (fn (intExp1 (x)) => (fn(typemap : SymbolTable) => true) |
        (intExp2 (y)) => (fn(typemap : SymbolTable) => (typemap(y) = IntRep)) |
        (intExp3 (arithmaticOp, iExp1, iExp2)) =>
            (fn(typemap : SymbolTable) =>
                validIntExp(iExp1)(typemap) andalso validIntExp(iExp2)(typemap)
            )
    );

(*Testing section 8*)
(*Test table for both section 8 & 9*)
val testTable : SymbolTable =   
    fn (var "num") => IntRep
     | (var "rem") => IntRep
     | (var "sum") => IntRep
     | (var "n") => IntRep
     | (var "result") => BoolRep
     | (var "a") => IntRep
     | (var "b") => IntRep
     | (var "c") => IntRep
     | _ => Not;

print("\n--------Testing section 8---------\n");
(*Good constant*)
val goodIntConst : integerExpression = intExp1 69;
val test8_1_goodConst = validIntExp (goodIntConst) (testTable);

(*Good variable: existing variable*)
val test8_2_goodVar = validIntExp (intExp2 v_num) (testTable);

(*Bad variable: boolExp instead of intExp*)
val test8_3_badVar = validIntExp (intExp2 v_result) (testTable);   (*v_result is a boolExp*)

(*Good binary: 1 + 2*)
val goodIntBinary : integerExpression = intExp3 (Plus, intExp1 1, intExp1 2);
val test8_4_goodBin = validIntExp (goodIntBinary) (testTable);

(*Bad binary: intExp + boolExp*)
val badIntBinary : integerExpression = intExp3 (Plus, intExp1 1, intExp2 v_result); (*v_result is a boolExp*)
val test8_5_badBin = validIntExp (badIntBinary) (testTable);

print("--------End Testing section 8---------\n\n");

(*9*)
(* Valid Bool Exp: SymbolTable -> BoolExpression -> Bool -Jose & Wenshen*)
val rec validBoolExp =
    ( fn (typemap:SymbolTable) =>
        (fn (boolExp1(x)) => true 
        |   (boolExp2(y)) => (typemap(y) = BoolRep) 
        |   (boolExp3 (relationalOp , iExp1 , iExp2))=>
                validIntExp (iExp1)(typemap) andalso
                validIntExp (iExp2)(typemap) 
        |   (boolExp4 (boolOp, bExp1, bExp2)) =>
                validBoolExp(typemap)(bExp1) andalso
                validBoolExp (typemap)(bExp2)
        )
    );

(*Testing for section 9*)
print("\n--------Testing section 9---------\n");
(*Good constant*)
val test9_1_good = validBoolExp testTable(boolExp1 true);

(*Good variable*)
val test9_2_goodVar = validBoolExp testTable(boolExp2(var "result"));

(*Bad variable*)
val test9_3_badVar = validBoolExp testTable(boolExp2(var "num"));

(*Good binary relational OP: a < b + c*)
val test_BPlusC : integerExpression = intExp3 (Plus, intExp2 (var "b"), intExp2 (var "c"));
val test9_4_goodBinRela = validBoolExp testTable(boolExp3(Lt, intExp2 (var "a"), test_BPlusC));

(*Bad binary relational OP: b + c > result, but result is boolExp*)
val test9_5_badBinRela = validBoolExp testTable(boolExp3(Gt, test_BPlusC, intExp2 v_result));

(*Good boolean OP: a < b + c Or a > 0*)
val test_AGtZero : booleanExpression = boolExp3 (Gt, intExp2 (var "a"), intExp1 0); (*a > 0*)
val test9_6_goodBool = validBoolExp testTable(boolExp4(Or, (boolExp3(Lt, intExp2 (var "a"), test_BPlusC)), test_AGtZero));

print("--------End Testing section 9---------\n\n");


(*10*)
(* valid Instruction: Instruction -> SymbolTable -> Bool - Jose & Wenshen*)
val rec validInstruction =
    ( fn Skip => ( fn (typemap: SymbolTable) => true) 
        | (inst2(x, IntExp)) => (fn(typemap:SymbolTable) =>
                                (typemap(x) = IntRep) andalso
                                validIntExp (IntExp)(typemap))
        | (inst3(y, BoolExp)) => (fn(typemap: SymbolTable) => 
                                (typemap(y) = BoolRep) andalso
                                validBoolExp (typemap)(BoolExp))
        | (inst4(intListHead :: intListTail)) => ( fn( typemap: SymbolTable) => 
                                                validInstruction(intListHead)(typemap) andalso
                                                validInstruction( inst4(intListTail))(typemap))
        | (inst4([])) => ( fn (typemap: SymbolTable) => true) 
        | (inst5 (truthbranch, falsebranch, boolExp)) =>
            (fn(typemap: SymbolTable) =>
                validInstruction(truthbranch)(typemap) andalso
                validInstruction(falsebranch)(typemap) andalso
                validBoolExp (typemap)(boolExp))
        | (inst6 (loopbody, boolExp)) => ( fn (typemap: SymbolTable)=> 
                                        validInstruction(loopbody)(typemap) andalso
                                        validBoolExp(typemap)(boolExp))
    );

(*Testing for section 10*)
print("\n--------Testing section 10---------\n");

(*5 good assignments*)
val test10_1 = validInstruction (inst2(var "num", intExp1 156)) (testTable);    (*num = 156*)
val test10_2 = validInstruction (inst3(var "result", boolExp1 false)) (testTable);  (*result = false*)
val test10_3 = validInstruction (whileLoopInstList) (testTable);    (*inst4, while loop body in sample*)
val test10_4 = validInstruction (ifElseInstruction) (testTable);    (*inst5, if/else body in sample*)
val test10_5 = validInstruction (whileLoop) (testTable);    (*inst6, while loop conditional in sample*)

(*1 bad assignment: num = result, result is a boolean*)
val test10_6_badAssign = validInstruction (inst2(var "num", intExp2 v_result)) (testTable);

print("--------End Testing section 10---------\n\n");

(*11*)
(* Exception Invalid Declist - Jose *)
exception InvalidDecList;

(*12*)
(* Exception Invalid Program - Jose *)
exception InvalidProgram;

(*13*)
(* Valid Program: Program -> Bool - Jose & Wenshen*)
val validProgram =
    (fn (decList, programBody): program =>
    if validDecList(decList) <> true
    then raise InvalidDecList
    else 
        if validInstruction(programBody)(SymbolTableComplete(decList))
        then true 
        else raise InvalidProgram 
    );

(*Testing section 13*)
print("\n--------Testing section 13---------\n");

(*Good program*)
val test13_1_goodProg = validProgram(teamTwoProgram);

(*Bad decList: badDecList from section 2*)
val badDecListProgram = (badDecList, programBody);
(* val test13_2_badDecList = validProgram(badDecListProgram); *)

(*Bad body: bad instruction from section 10*)
val badInstList : instruction = inst4([num_156, rem_0, sum_0, n_num, whileLoop, ifElseInstruction, inst2(var "num", intExp2 v_result)]);
val badBodyProgram = (declarations, badInstList);
(* val test13_3_badBody = validProgram(badBodyProgram); *)

print("\nNote: uncomment tests to see exceptions raised from bad cases.\n");
print("--------End Testing section 13---------\n\n");
