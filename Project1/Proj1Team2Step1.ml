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
(*<IntegerConstant>*)
(*<BooleanConstant>*)

(*<ArithmaticOp>*)
(*<RelationalOp>*)
(*<BooleanOp>*)

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
                        inst1 of (variable * integerExpression) |
                        inst2 of (variable * booleanExpression) |
                        inst3 of instruction list |
                        inst4 of (instruction * instruction * booleanExpression) |
                        inst5 of (instruction * booleanExpression);

(*<TypeName>*)
datatype typeName = TypeName1Boolean | TypeName2Integer;

(*<Declaration>*)
type declaration = variable * typeName;


(*<DeclarationList>*)
(*<Program>*)
                            


