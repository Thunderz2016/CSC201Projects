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
                            boolExp4 of (booleanOp * integerExpression * integerExpression);

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
                            


