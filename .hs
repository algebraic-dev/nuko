{-# OPTIONS_GHC -w #-}
module Nuko.Syntax.Parser where

import Nuko.Syntax.Lexer.Support
import Nuko.Syntax.Lexer.Tokens
import Nuko.Syntax.Lexer
import Nuko.Syntax.Range
import Nuko.Syntax.Error
import Nuko.Syntax.Range
import Nuko.Syntax.Ast
import Nuko.Tree.TopLevel
import Nuko.Tree.Expr
import Data.Text            (Text)
import Control.Monad.Except (throwError)

import qualified Data.List.NonEmpty as NE
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t7 t8 t11 t14 t20 t27 t28 t29 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44
	= HappyTerminal (Ranged Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn5 (Name Normal)
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 (Type Normal)
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 (Pat Normal)
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 (Literal Normal)
	| HappyAbsSyn16 (Expr Normal)
	| HappyAbsSyn17 (NE.NonEmpty (Expr Normal))
	| HappyAbsSyn18 (Var Normal)
	| HappyAbsSyn19 (Block Normal)
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 (((Pat Normal, Expr Normal)))
	| HappyAbsSyn24 ((Name Normal, Type Normal))
	| HappyAbsSyn25 ((Name Normal, [Type Normal]))
	| HappyAbsSyn26 (TypeDeclArg Normal)
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 (Program Normal)
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,485) ([0,0,8192,33249,288,0,0,2304,0,0,0,0,32768,0,0,0,0,0,0,0,0,36864,0,0,0,0,2304,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,128,0,0,0,0,0,4,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,129,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,57632,8321,1,0,0,7698,4616,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7698,4616,0,0,0,33248,128,0,0,6656,2078,18,0,0,0,0,18,0,0,0,16384,0,0,0,0,512,0,0,0,2078,8,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,9216,0,0,0,0,0,0,0,0,3584,2056,0,0,0,0,0,0,0,0,2078,8,0,0,0,0,0,0,0,0,16,0,0,0,0,512,0,0,1024,0,0,0,0,36864,129,0,0,0,7680,8,0,0,0,0,0,0,0,0,24,0,0,0,32768,0,0,0,0,3584,2056,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,4,0,0,0,2062,8,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,32768,129,0,0,0,2048,0,0,0,0,33168,0,0,0,0,0,16,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,0,0,0,0,8192,33249,288,0,0,0,2073,0,0,0,0,1,0,0,0,0,96,0,0,40960,33249,288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40960,33249,288,0,0,0,0,0,0,0,0,0,0,0,0,7698,4616,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,33249,288,0,0,0,2078,8,0,0,0,256,0,0,0,0,1024,0,0,0,33152,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,8,0,0,0,0,0,0,0,0,16384,0,0,0,36864,129,0,0,0,0,0,0,0,0,33680,8,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,4096,0,0,0,0,0,0,0,0,4608,2078,18,0,0,0,32,0,0,0,0,0,0,0,0,33168,0,0,0,0,0,0,0,0,0,576,0,0,0,0,8192,1,0,0,0,1024,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,57344,32897,0,0,0,7698,4616,0,0,0,0,0,0,0,0,0,0,0,0,32768,129,0,0,0,0,32,0,0,0,0,16,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,36864,129,0,0,0,6144,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7698,4616,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExpr","%start_parseProgram","Lower","Upper","PathEnd","PathExpr","TypeAtom","TypeCon","Type","AtomPat","Pat","OptSep","Literal","Atom","Call","VarExpr","BlockExpr","End","CaseClause","ClosedExpr","Expr","ProdClause","SumClause","TypeTy","TypeDecl","Ret","LetDecl","Program","List__AtomPat__","List__Lower__","List__TypeAtom__","List1__SumClause__","List1__TypeAtom__","Optional__Ret__","Optional__sep__","Path__Upper__","PathHelper__PathEnd__","SepList__','__ProdClause__","SepList__sep__CaseClause__","PathHelper__Upper__","SepList1__','__ProdClause__","SepList1__sep__CaseClause__","type","match","with","let","if","then","else","pub","forall","int","str","lower","upper","'{'","'}'","'['","']'","'->'","'=>'","'('","')'","':'","'='","'|'","','","'\\\\'","'.'","'_'","begin","sep","end","%eof"]
        bit_start = st Prelude.* 76
        bit_end = (st Prelude.+ 1) Prelude.* 76
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..75]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (46) = happyShift action_18
action_0 (49) = happyShift action_19
action_0 (54) = happyShift action_20
action_0 (55) = happyShift action_21
action_0 (56) = happyShift action_3
action_0 (57) = happyShift action_22
action_0 (64) = happyShift action_23
action_0 (70) = happyShift action_24
action_0 (73) = happyShift action_25
action_0 (5) = happyGoto action_9
action_0 (6) = happyGoto action_10
action_0 (7) = happyGoto action_11
action_0 (8) = happyGoto action_12
action_0 (15) = happyGoto action_13
action_0 (16) = happyGoto action_14
action_0 (22) = happyGoto action_15
action_0 (23) = happyGoto action_16
action_0 (39) = happyGoto action_17
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (45) = happyShift action_7
action_1 (48) = happyShift action_8
action_1 (27) = happyGoto action_4
action_1 (29) = happyGoto action_5
action_1 (30) = happyGoto action_6
action_1 _ = happyReduce_56

action_2 (56) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (45) = happyShift action_7
action_4 (48) = happyShift action_8
action_4 (27) = happyGoto action_4
action_4 (29) = happyGoto action_5
action_4 (30) = happyGoto action_50
action_4 _ = happyReduce_56

action_5 (45) = happyShift action_7
action_5 (48) = happyShift action_8
action_5 (27) = happyGoto action_4
action_5 (29) = happyGoto action_5
action_5 (30) = happyGoto action_49
action_5 _ = happyReduce_56

action_6 (76) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (57) = happyShift action_22
action_7 (6) = happyGoto action_48
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (56) = happyShift action_3
action_8 (5) = happyGoto action_47
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (71) = happyShift action_46
action_9 _ = happyReduce_5

action_10 (71) = happyShift action_45
action_10 _ = happyReduce_4

action_11 _ = happyReduce_73

action_12 _ = happyReduce_26

action_13 _ = happyReduce_27

action_14 (54) = happyShift action_20
action_14 (55) = happyShift action_21
action_14 (56) = happyShift action_3
action_14 (57) = happyShift action_22
action_14 (64) = happyShift action_23
action_14 (5) = happyGoto action_9
action_14 (6) = happyGoto action_10
action_14 (7) = happyGoto action_11
action_14 (8) = happyGoto action_12
action_14 (15) = happyGoto action_13
action_14 (16) = happyGoto action_43
action_14 (17) = happyGoto action_44
action_14 (39) = happyGoto action_17
action_14 _ = happyReduce_43

action_15 (66) = happyShift action_42
action_15 _ = happyReduce_44

action_16 (76) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_7

action_18 (46) = happyShift action_18
action_18 (49) = happyShift action_19
action_18 (54) = happyShift action_20
action_18 (55) = happyShift action_21
action_18 (56) = happyShift action_3
action_18 (57) = happyShift action_22
action_18 (64) = happyShift action_23
action_18 (70) = happyShift action_24
action_18 (73) = happyShift action_25
action_18 (5) = happyGoto action_9
action_18 (6) = happyGoto action_10
action_18 (7) = happyGoto action_11
action_18 (8) = happyGoto action_12
action_18 (15) = happyGoto action_13
action_18 (16) = happyGoto action_14
action_18 (22) = happyGoto action_41
action_18 (39) = happyGoto action_17
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (46) = happyShift action_18
action_19 (49) = happyShift action_19
action_19 (54) = happyShift action_20
action_19 (55) = happyShift action_21
action_19 (56) = happyShift action_3
action_19 (57) = happyShift action_22
action_19 (64) = happyShift action_23
action_19 (70) = happyShift action_24
action_19 (73) = happyShift action_25
action_19 (5) = happyGoto action_9
action_19 (6) = happyGoto action_10
action_19 (7) = happyGoto action_11
action_19 (8) = happyGoto action_12
action_19 (15) = happyGoto action_13
action_19 (16) = happyGoto action_14
action_19 (22) = happyGoto action_40
action_19 (39) = happyGoto action_17
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_24

action_21 _ = happyReduce_25

action_22 _ = happyReduce_3

action_23 (46) = happyShift action_18
action_23 (49) = happyShift action_19
action_23 (54) = happyShift action_20
action_23 (55) = happyShift action_21
action_23 (56) = happyShift action_3
action_23 (57) = happyShift action_22
action_23 (64) = happyShift action_23
action_23 (70) = happyShift action_24
action_23 (73) = happyShift action_25
action_23 (5) = happyGoto action_9
action_23 (6) = happyGoto action_10
action_23 (7) = happyGoto action_11
action_23 (8) = happyGoto action_12
action_23 (15) = happyGoto action_13
action_23 (16) = happyGoto action_14
action_23 (22) = happyGoto action_15
action_23 (23) = happyGoto action_39
action_23 (39) = happyGoto action_17
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (54) = happyShift action_20
action_24 (55) = happyShift action_21
action_24 (56) = happyShift action_3
action_24 (57) = happyShift action_22
action_24 (64) = happyShift action_37
action_24 (72) = happyShift action_38
action_24 (5) = happyGoto action_30
action_24 (6) = happyGoto action_31
action_24 (12) = happyGoto action_32
action_24 (13) = happyGoto action_33
action_24 (15) = happyGoto action_34
action_24 (38) = happyGoto action_35
action_24 (42) = happyGoto action_36
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (46) = happyShift action_18
action_25 (48) = happyShift action_29
action_25 (49) = happyShift action_19
action_25 (54) = happyShift action_20
action_25 (55) = happyShift action_21
action_25 (56) = happyShift action_3
action_25 (57) = happyShift action_22
action_25 (64) = happyShift action_23
action_25 (70) = happyShift action_24
action_25 (73) = happyShift action_25
action_25 (5) = happyGoto action_9
action_25 (6) = happyGoto action_10
action_25 (7) = happyGoto action_11
action_25 (8) = happyGoto action_12
action_25 (15) = happyGoto action_13
action_25 (16) = happyGoto action_14
action_25 (18) = happyGoto action_26
action_25 (19) = happyGoto action_27
action_25 (22) = happyGoto action_15
action_25 (23) = happyGoto action_28
action_25 (39) = happyGoto action_17
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (74) = happyShift action_80
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (1) = happyShift action_78
action_27 (75) = happyShift action_79
action_27 (20) = happyGoto action_77
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (74) = happyShift action_76
action_28 _ = happyReduce_34

action_29 (54) = happyShift action_20
action_29 (55) = happyShift action_21
action_29 (56) = happyShift action_3
action_29 (57) = happyShift action_22
action_29 (64) = happyShift action_37
action_29 (72) = happyShift action_38
action_29 (5) = happyGoto action_30
action_29 (6) = happyGoto action_31
action_29 (12) = happyGoto action_32
action_29 (13) = happyGoto action_75
action_29 (15) = happyGoto action_34
action_29 (38) = happyGoto action_35
action_29 (42) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_17

action_31 (71) = happyShift action_74
action_31 _ = happyReduce_79

action_32 _ = happyReduce_22

action_33 (63) = happyShift action_72
action_33 (66) = happyShift action_73
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_18

action_35 (54) = happyShift action_20
action_35 (55) = happyShift action_21
action_35 (56) = happyShift action_3
action_35 (64) = happyShift action_37
action_35 (72) = happyShift action_38
action_35 (5) = happyGoto action_30
action_35 (12) = happyGoto action_53
action_35 (15) = happyGoto action_34
action_35 (31) = happyGoto action_71
action_35 _ = happyReduce_58

action_36 _ = happyReduce_71

action_37 (54) = happyShift action_20
action_37 (55) = happyShift action_21
action_37 (56) = happyShift action_3
action_37 (57) = happyShift action_22
action_37 (64) = happyShift action_37
action_37 (72) = happyShift action_38
action_37 (5) = happyGoto action_30
action_37 (6) = happyGoto action_31
action_37 (12) = happyGoto action_32
action_37 (13) = happyGoto action_70
action_37 (15) = happyGoto action_34
action_37 (38) = happyGoto action_35
action_37 (42) = happyGoto action_36
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_16

action_39 (65) = happyShift action_69
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (74) = happyShift action_68
action_40 (14) = happyGoto action_66
action_40 (37) = happyGoto action_67
action_40 _ = happyReduce_70

action_41 (47) = happyShift action_65
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (53) = happyShift action_63
action_42 (56) = happyShift action_3
action_42 (57) = happyShift action_22
action_42 (64) = happyShift action_64
action_42 (5) = happyGoto action_58
action_42 (6) = happyGoto action_31
action_42 (9) = happyGoto action_59
action_42 (10) = happyGoto action_60
action_42 (11) = happyGoto action_61
action_42 (38) = happyGoto action_62
action_42 (42) = happyGoto action_36
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (54) = happyShift action_20
action_43 (55) = happyShift action_21
action_43 (56) = happyShift action_3
action_43 (57) = happyShift action_22
action_43 (64) = happyShift action_23
action_43 (5) = happyGoto action_9
action_43 (6) = happyGoto action_10
action_43 (7) = happyGoto action_11
action_43 (8) = happyGoto action_12
action_43 (15) = happyGoto action_13
action_43 (16) = happyGoto action_43
action_43 (17) = happyGoto action_57
action_43 (39) = happyGoto action_17
action_43 _ = happyReduce_30

action_44 _ = happyReduce_39

action_45 (56) = happyShift action_3
action_45 (57) = happyShift action_22
action_45 (5) = happyGoto action_9
action_45 (6) = happyGoto action_10
action_45 (7) = happyGoto action_11
action_45 (39) = happyGoto action_56
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (56) = happyShift action_3
action_46 (5) = happyGoto action_55
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (54) = happyShift action_20
action_47 (55) = happyShift action_21
action_47 (56) = happyShift action_3
action_47 (64) = happyShift action_37
action_47 (72) = happyShift action_38
action_47 (5) = happyGoto action_30
action_47 (12) = happyGoto action_53
action_47 (15) = happyGoto action_34
action_47 (31) = happyGoto action_54
action_47 _ = happyReduce_58

action_48 (56) = happyShift action_3
action_48 (5) = happyGoto action_51
action_48 (32) = happyGoto action_52
action_48 _ = happyReduce_60

action_49 _ = happyReduce_54

action_50 _ = happyReduce_55

action_51 (56) = happyShift action_3
action_51 (5) = happyGoto action_51
action_51 (32) = happyGoto action_101
action_51 _ = happyReduce_60

action_52 (67) = happyShift action_100
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (54) = happyShift action_20
action_53 (55) = happyShift action_21
action_53 (56) = happyShift action_3
action_53 (64) = happyShift action_37
action_53 (72) = happyShift action_38
action_53 (5) = happyGoto action_30
action_53 (12) = happyGoto action_53
action_53 (15) = happyGoto action_34
action_53 (31) = happyGoto action_99
action_53 _ = happyReduce_58

action_54 (66) = happyShift action_98
action_54 (28) = happyGoto action_96
action_54 (36) = happyGoto action_97
action_54 _ = happyReduce_68

action_55 _ = happyReduce_6

action_56 _ = happyReduce_72

action_57 _ = happyReduce_29

action_58 _ = happyReduce_8

action_59 _ = happyReduce_13

action_60 (62) = happyShift action_95
action_60 _ = happyReduce_15

action_61 _ = happyReduce_45

action_62 (56) = happyShift action_3
action_62 (57) = happyShift action_22
action_62 (64) = happyShift action_64
action_62 (5) = happyGoto action_58
action_62 (6) = happyGoto action_31
action_62 (9) = happyGoto action_92
action_62 (35) = happyGoto action_93
action_62 (38) = happyGoto action_94
action_62 (42) = happyGoto action_36
action_62 _ = happyReduce_9

action_63 (56) = happyShift action_3
action_63 (5) = happyGoto action_91
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (53) = happyShift action_63
action_64 (56) = happyShift action_3
action_64 (57) = happyShift action_22
action_64 (64) = happyShift action_64
action_64 (5) = happyGoto action_58
action_64 (6) = happyGoto action_31
action_64 (9) = happyGoto action_59
action_64 (10) = happyGoto action_60
action_64 (11) = happyGoto action_90
action_64 (38) = happyGoto action_62
action_64 (42) = happyGoto action_36
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (73) = happyShift action_89
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (50) = happyShift action_88
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_23

action_68 _ = happyReduce_69

action_69 _ = happyReduce_28

action_70 (65) = happyShift action_87
action_70 (66) = happyShift action_73
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_20

action_72 (46) = happyShift action_18
action_72 (49) = happyShift action_19
action_72 (54) = happyShift action_20
action_72 (55) = happyShift action_21
action_72 (56) = happyShift action_3
action_72 (57) = happyShift action_22
action_72 (64) = happyShift action_23
action_72 (70) = happyShift action_24
action_72 (73) = happyShift action_25
action_72 (5) = happyGoto action_9
action_72 (6) = happyGoto action_10
action_72 (7) = happyGoto action_11
action_72 (8) = happyGoto action_12
action_72 (15) = happyGoto action_13
action_72 (16) = happyGoto action_14
action_72 (22) = happyGoto action_86
action_72 (39) = happyGoto action_17
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (53) = happyShift action_63
action_73 (56) = happyShift action_3
action_73 (57) = happyShift action_22
action_73 (64) = happyShift action_64
action_73 (5) = happyGoto action_58
action_73 (6) = happyGoto action_31
action_73 (9) = happyGoto action_59
action_73 (10) = happyGoto action_60
action_73 (11) = happyGoto action_85
action_73 (38) = happyGoto action_62
action_73 (42) = happyGoto action_36
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (57) = happyShift action_22
action_74 (6) = happyGoto action_31
action_74 (42) = happyGoto action_84
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (66) = happyShift action_73
action_75 (67) = happyShift action_83
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (46) = happyShift action_18
action_76 (48) = happyShift action_29
action_76 (49) = happyShift action_19
action_76 (54) = happyShift action_20
action_76 (55) = happyShift action_21
action_76 (56) = happyShift action_3
action_76 (57) = happyShift action_22
action_76 (64) = happyShift action_23
action_76 (70) = happyShift action_24
action_76 (73) = happyShift action_25
action_76 (5) = happyGoto action_9
action_76 (6) = happyGoto action_10
action_76 (7) = happyGoto action_11
action_76 (8) = happyGoto action_12
action_76 (15) = happyGoto action_13
action_76 (16) = happyGoto action_14
action_76 (18) = happyGoto action_26
action_76 (19) = happyGoto action_82
action_76 (22) = happyGoto action_15
action_76 (23) = happyGoto action_28
action_76 (39) = happyGoto action_17
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_41

action_78 _ = happyReduce_36

action_79 _ = happyReduce_35

action_80 (46) = happyShift action_18
action_80 (48) = happyShift action_29
action_80 (49) = happyShift action_19
action_80 (54) = happyShift action_20
action_80 (55) = happyShift action_21
action_80 (56) = happyShift action_3
action_80 (57) = happyShift action_22
action_80 (64) = happyShift action_23
action_80 (70) = happyShift action_24
action_80 (73) = happyShift action_25
action_80 (5) = happyGoto action_9
action_80 (6) = happyGoto action_10
action_80 (7) = happyGoto action_11
action_80 (8) = happyGoto action_12
action_80 (15) = happyGoto action_13
action_80 (16) = happyGoto action_14
action_80 (18) = happyGoto action_26
action_80 (19) = happyGoto action_81
action_80 (22) = happyGoto action_15
action_80 (23) = happyGoto action_28
action_80 (39) = happyGoto action_17
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_33

action_82 _ = happyReduce_32

action_83 (46) = happyShift action_18
action_83 (49) = happyShift action_19
action_83 (54) = happyShift action_20
action_83 (55) = happyShift action_21
action_83 (56) = happyShift action_3
action_83 (57) = happyShift action_22
action_83 (64) = happyShift action_23
action_83 (70) = happyShift action_24
action_83 (73) = happyShift action_25
action_83 (5) = happyGoto action_9
action_83 (6) = happyGoto action_10
action_83 (7) = happyGoto action_11
action_83 (8) = happyGoto action_12
action_83 (15) = happyGoto action_13
action_83 (16) = happyGoto action_14
action_83 (22) = happyGoto action_15
action_83 (23) = happyGoto action_119
action_83 (39) = happyGoto action_17
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_78

action_85 _ = happyReduce_21

action_86 _ = happyReduce_40

action_87 _ = happyReduce_19

action_88 (46) = happyShift action_18
action_88 (49) = happyShift action_19
action_88 (54) = happyShift action_20
action_88 (55) = happyShift action_21
action_88 (56) = happyShift action_3
action_88 (57) = happyShift action_22
action_88 (64) = happyShift action_23
action_88 (70) = happyShift action_24
action_88 (73) = happyShift action_25
action_88 (5) = happyGoto action_9
action_88 (6) = happyGoto action_10
action_88 (7) = happyGoto action_11
action_88 (8) = happyGoto action_12
action_88 (15) = happyGoto action_13
action_88 (16) = happyGoto action_14
action_88 (22) = happyGoto action_118
action_88 (39) = happyGoto action_17
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (54) = happyShift action_20
action_89 (55) = happyShift action_21
action_89 (56) = happyShift action_3
action_89 (57) = happyShift action_22
action_89 (64) = happyShift action_37
action_89 (72) = happyShift action_38
action_89 (5) = happyGoto action_30
action_89 (6) = happyGoto action_31
action_89 (12) = happyGoto action_32
action_89 (13) = happyGoto action_114
action_89 (15) = happyGoto action_34
action_89 (21) = happyGoto action_115
action_89 (38) = happyGoto action_35
action_89 (41) = happyGoto action_116
action_89 (42) = happyGoto action_36
action_89 (44) = happyGoto action_117
action_89 _ = happyReduce_77

action_90 (65) = happyShift action_113
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (71) = happyShift action_112
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (56) = happyShift action_3
action_92 (57) = happyShift action_22
action_92 (64) = happyShift action_64
action_92 (5) = happyGoto action_58
action_92 (6) = happyGoto action_31
action_92 (9) = happyGoto action_92
action_92 (35) = happyGoto action_111
action_92 (38) = happyGoto action_94
action_92 (42) = happyGoto action_36
action_92 _ = happyReduce_66

action_93 _ = happyReduce_11

action_94 _ = happyReduce_9

action_95 (56) = happyShift action_3
action_95 (57) = happyShift action_22
action_95 (64) = happyShift action_64
action_95 (5) = happyGoto action_58
action_95 (6) = happyGoto action_31
action_95 (9) = happyGoto action_59
action_95 (10) = happyGoto action_110
action_95 (38) = happyGoto action_62
action_95 (42) = happyGoto action_36
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_67

action_97 (67) = happyShift action_109
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (53) = happyShift action_63
action_98 (56) = happyShift action_3
action_98 (57) = happyShift action_22
action_98 (64) = happyShift action_64
action_98 (5) = happyGoto action_58
action_98 (6) = happyGoto action_31
action_98 (9) = happyGoto action_59
action_98 (10) = happyGoto action_60
action_98 (11) = happyGoto action_108
action_98 (38) = happyGoto action_62
action_98 (42) = happyGoto action_36
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_57

action_100 (53) = happyShift action_63
action_100 (56) = happyShift action_3
action_100 (57) = happyShift action_22
action_100 (58) = happyShift action_106
action_100 (64) = happyShift action_64
action_100 (68) = happyShift action_107
action_100 (5) = happyGoto action_58
action_100 (6) = happyGoto action_31
action_100 (9) = happyGoto action_59
action_100 (10) = happyGoto action_60
action_100 (11) = happyGoto action_102
action_100 (25) = happyGoto action_103
action_100 (26) = happyGoto action_104
action_100 (34) = happyGoto action_105
action_100 (38) = happyGoto action_62
action_100 (42) = happyGoto action_36
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_59

action_102 _ = happyReduce_50

action_103 (68) = happyShift action_107
action_103 (25) = happyGoto action_103
action_103 (34) = happyGoto action_131
action_103 _ = happyReduce_64

action_104 _ = happyReduce_51

action_105 _ = happyReduce_49

action_106 (56) = happyShift action_3
action_106 (5) = happyGoto action_127
action_106 (24) = happyGoto action_128
action_106 (40) = happyGoto action_129
action_106 (43) = happyGoto action_130
action_106 _ = happyReduce_75

action_107 (57) = happyShift action_22
action_107 (6) = happyGoto action_126
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_52

action_109 (46) = happyShift action_18
action_109 (49) = happyShift action_19
action_109 (54) = happyShift action_20
action_109 (55) = happyShift action_21
action_109 (56) = happyShift action_3
action_109 (57) = happyShift action_22
action_109 (64) = happyShift action_23
action_109 (70) = happyShift action_24
action_109 (73) = happyShift action_25
action_109 (5) = happyGoto action_9
action_109 (6) = happyGoto action_10
action_109 (7) = happyGoto action_11
action_109 (8) = happyGoto action_12
action_109 (15) = happyGoto action_13
action_109 (16) = happyGoto action_14
action_109 (22) = happyGoto action_15
action_109 (23) = happyGoto action_125
action_109 (39) = happyGoto action_17
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (62) = happyShift action_95
action_110 _ = happyReduce_12

action_111 _ = happyReduce_65

action_112 (53) = happyShift action_63
action_112 (56) = happyShift action_3
action_112 (57) = happyShift action_22
action_112 (64) = happyShift action_64
action_112 (5) = happyGoto action_58
action_112 (6) = happyGoto action_31
action_112 (9) = happyGoto action_59
action_112 (10) = happyGoto action_60
action_112 (11) = happyGoto action_124
action_112 (38) = happyGoto action_62
action_112 (42) = happyGoto action_36
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_10

action_114 (63) = happyShift action_123
action_114 (66) = happyShift action_73
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (74) = happyShift action_122
action_115 _ = happyReduce_83

action_116 (1) = happyShift action_78
action_116 (75) = happyShift action_79
action_116 (20) = happyGoto action_121
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_76

action_118 (74) = happyShift action_68
action_118 (14) = happyGoto action_120
action_118 (37) = happyGoto action_67
action_118 _ = happyReduce_70

action_119 _ = happyReduce_31

action_120 (51) = happyShift action_139
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_42

action_122 (54) = happyShift action_20
action_122 (55) = happyShift action_21
action_122 (56) = happyShift action_3
action_122 (57) = happyShift action_22
action_122 (64) = happyShift action_37
action_122 (72) = happyShift action_38
action_122 (5) = happyGoto action_30
action_122 (6) = happyGoto action_31
action_122 (12) = happyGoto action_32
action_122 (13) = happyGoto action_114
action_122 (15) = happyGoto action_34
action_122 (21) = happyGoto action_115
action_122 (38) = happyGoto action_35
action_122 (42) = happyGoto action_36
action_122 (44) = happyGoto action_138
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (46) = happyShift action_18
action_123 (49) = happyShift action_19
action_123 (54) = happyShift action_20
action_123 (55) = happyShift action_21
action_123 (56) = happyShift action_3
action_123 (57) = happyShift action_22
action_123 (64) = happyShift action_23
action_123 (70) = happyShift action_24
action_123 (73) = happyShift action_25
action_123 (5) = happyGoto action_9
action_123 (6) = happyGoto action_10
action_123 (7) = happyGoto action_11
action_123 (8) = happyGoto action_12
action_123 (15) = happyGoto action_13
action_123 (16) = happyGoto action_14
action_123 (22) = happyGoto action_15
action_123 (23) = happyGoto action_137
action_123 (39) = happyGoto action_17
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_14

action_125 _ = happyReduce_53

action_126 (56) = happyShift action_3
action_126 (57) = happyShift action_22
action_126 (64) = happyShift action_64
action_126 (5) = happyGoto action_58
action_126 (6) = happyGoto action_31
action_126 (9) = happyGoto action_135
action_126 (33) = happyGoto action_136
action_126 (38) = happyGoto action_94
action_126 (42) = happyGoto action_36
action_126 _ = happyReduce_62

action_127 (66) = happyShift action_134
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (69) = happyShift action_133
action_128 _ = happyReduce_81

action_129 (59) = happyShift action_132
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_74

action_131 _ = happyReduce_63

action_132 _ = happyReduce_48

action_133 (56) = happyShift action_3
action_133 (5) = happyGoto action_127
action_133 (24) = happyGoto action_128
action_133 (43) = happyGoto action_143
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (53) = happyShift action_63
action_134 (56) = happyShift action_3
action_134 (57) = happyShift action_22
action_134 (64) = happyShift action_64
action_134 (5) = happyGoto action_58
action_134 (6) = happyGoto action_31
action_134 (9) = happyGoto action_59
action_134 (10) = happyGoto action_60
action_134 (11) = happyGoto action_142
action_134 (38) = happyGoto action_62
action_134 (42) = happyGoto action_36
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (56) = happyShift action_3
action_135 (57) = happyShift action_22
action_135 (64) = happyShift action_64
action_135 (5) = happyGoto action_58
action_135 (6) = happyGoto action_31
action_135 (9) = happyGoto action_135
action_135 (33) = happyGoto action_141
action_135 (38) = happyGoto action_94
action_135 (42) = happyGoto action_36
action_135 _ = happyReduce_62

action_136 _ = happyReduce_47

action_137 _ = happyReduce_37

action_138 _ = happyReduce_82

action_139 (46) = happyShift action_18
action_139 (49) = happyShift action_19
action_139 (54) = happyShift action_20
action_139 (55) = happyShift action_21
action_139 (56) = happyShift action_3
action_139 (57) = happyShift action_22
action_139 (64) = happyShift action_23
action_139 (70) = happyShift action_24
action_139 (73) = happyShift action_25
action_139 (5) = happyGoto action_9
action_139 (6) = happyGoto action_10
action_139 (7) = happyGoto action_11
action_139 (8) = happyGoto action_12
action_139 (15) = happyGoto action_13
action_139 (16) = happyGoto action_14
action_139 (22) = happyGoto action_140
action_139 (39) = happyGoto action_17
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_38

action_141 _ = happyReduce_61

action_142 _ = happyReduce_46

action_143 _ = happyReduce_80

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (Name (getData happy_var_1) happy_var_1.position
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (Name (getData happy_var_1) happy_var_1.position
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (\p -> withPosListR p happy_var_1 (Lower (Path p happy_var_1))
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (\p -> withPosListR p happy_var_1 (Upper (Path p happy_var_1))
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (\p -> withPosListR p happy_var_3 (Accessor (withPosListR p happy_var_1 $ Lower (Path p happy_var_1)) happy_var_3)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn8
		 (let (p , f) = happy_var_1 in f p
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 (TPoly happy_var_1 NoExt
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn9
		 (TId happy_var_1 NoExt
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn9
		 (withPos happy_var_1 happy_var_2 $ TCons happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (withPos happy_var_1 happy_var_3 $ TArrow happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 11 happyReduction_14
happyReduction_14 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (withPos happy_var_1 happy_var_4 $ TForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (PWild happy_var_1.position
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 (PId happy_var_1 NoExt
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 (PLit happy_var_1 NoExt
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn12
		 (withPosList happy_var_1 happy_var_2 $ PCons happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (withPos happy_var_1 happy_var_3     $ PAnn happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn14
		 (()
	)

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (LInt (getInt happy_var_1) happy_var_1.position
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (LStr (getData happy_var_1) happy_var_1.position
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Lit happy_var_1 NoExt
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  17 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 NE.:| []
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 18 happyReduction_31
happyReduction_31 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (withPos happy_var_1 happy_var_4 $ Var happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn19
		 (BlBind happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 (BlVar happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn19
		 (BlEnd happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  20 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn20
		 (()
	)

happyReduce_36 = happyMonadReduce 1 20 happyReduction_36
happyReduction_36 (_ `HappyStk`
	happyRest) tk
	 = happyThen ((( popLayout))
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_37 = happySpecReduce_3  21 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 8 22 happyReduction_38
happyReduction_38 ((HappyAbsSyn16  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (withPos happy_var_1 happy_var_8 $ If happy_var_2 happy_var_5 (Just happy_var_8)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_2  22 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (withPos happy_var_1 happy_var_2 $ Call happy_var_1 happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 22 happyReduction_40
happyReduction_40 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (withPos happy_var_1 happy_var_4 $ Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  22 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (case happy_var_2 of { BlEnd x -> x; _ -> withPos happy_var_1 happy_var_2 $ Block happy_var_2}
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 6 22 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (withPos happy_var_1 happy_var_5 $ Case happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  22 happyReduction_43
happyReduction_43 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  23 happyReduction_44
happyReduction_44 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  23 happyReduction_45
happyReduction_45 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (withPos happy_var_1 happy_var_3 $ Ann happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  24 happyReduction_46
happyReduction_46 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn24
		 ((happy_var_1, happy_var_3)
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 (HappyAbsSyn33  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn25
		 ((happy_var_2, happy_var_3)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  26 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (TypeProd happy_var_2
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  26 happyReduction_49
happyReduction_49 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (TypeSum happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn26
		 (TypeSym happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happyReduce 5 27 happyReduction_51
happyReduction_51 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (TypeDecl happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_2  28 happyReduction_52
happyReduction_52 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 6 29 happyReduction_53
happyReduction_53 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (LetDecl happy_var_2 happy_var_3 happy_var_6 NoExt
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_2  30 happyReduction_54
happyReduction_54 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_2 { letDecls = happy_var_1 : happy_var_2.letDecls }
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  30 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_2 { tyDecls = happy_var_1  : happy_var_2.tyDecls }
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_0  30 happyReduction_56
happyReduction_56  =  HappyAbsSyn30
		 (Program [] [] NoExt
	)

happyReduce_57 = happySpecReduce_2  31 happyReduction_57
happyReduction_57 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  31 happyReduction_58
happyReduction_58  =  HappyAbsSyn31
		 ([]
	)

happyReduce_59 = happySpecReduce_2  32 happyReduction_59
happyReduction_59 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  32 happyReduction_60
happyReduction_60  =  HappyAbsSyn32
		 ([]
	)

happyReduce_61 = happySpecReduce_2  33 happyReduction_61
happyReduction_61 (HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  33 happyReduction_62
happyReduction_62  =  HappyAbsSyn33
		 ([]
	)

happyReduce_63 = happySpecReduce_2  34 happyReduction_63
happyReduction_63 (HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  34 happyReduction_64
happyReduction_64 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn34
		 (NE.singleton happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  35 happyReduction_65
happyReduction_65 (HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  35 happyReduction_66
happyReduction_66 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn35
		 (NE.singleton happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  36 happyReduction_67
happyReduction_67 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn36
		 (Just happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  36 happyReduction_68
happyReduction_68  =  HappyAbsSyn36
		 (Nothing
	)

happyReduce_69 = happySpecReduce_1  37 happyReduction_69
happyReduction_69 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (Just happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  37 happyReduction_70
happyReduction_70  =  HappyAbsSyn37
		 (Nothing
	)

happyReduce_71 = happySpecReduce_1  38 happyReduction_71
happyReduction_71 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn38
		 (let (p , f) = happy_var_1 in Path p f
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  39 happyReduction_72
happyReduction_72 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn39
		 (let (p, f) = happy_var_3 in (happy_var_1 : p, f)
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  39 happyReduction_73
happyReduction_73 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn39
		 (([], happy_var_1)
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  40 happyReduction_74
happyReduction_74 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  40 happyReduction_75
happyReduction_75  =  HappyAbsSyn40
		 ([]
	)

happyReduce_76 = happySpecReduce_1  41 happyReduction_76
happyReduction_76 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  41 happyReduction_77
happyReduction_77  =  HappyAbsSyn41
		 ([]
	)

happyReduce_78 = happySpecReduce_3  42 happyReduction_78
happyReduction_78 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn42
		 (let (p, f) = happy_var_3 in (happy_var_1 : p, f)
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  42 happyReduction_79
happyReduction_79 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn42
		 (([], happy_var_1)
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  43 happyReduction_80
happyReduction_80 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 : happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  43 happyReduction_81
happyReduction_81 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn43
		 ([happy_var_1]
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  44 happyReduction_82
happyReduction_82 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1 : happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  44 happyReduction_83
happyReduction_83 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Ranged TcEOF _ -> action 76 76 tk (HappyState action) sts stk;
	Ranged TcType _ -> cont 45;
	Ranged TcMatch _ -> cont 46;
	Ranged TcWith _ -> cont 47;
	Ranged TcLet _ -> cont 48;
	Ranged TcIf _ -> cont 49;
	Ranged TcThen _ -> cont 50;
	Ranged TcElse _ -> cont 51;
	Ranged TcPub _ -> cont 52;
	Ranged TcForall _ -> cont 53;
	Ranged (TcInt _) _ -> cont 54;
	Ranged (TcStr _) _ -> cont 55;
	Ranged (TcLowerId _) _ -> cont 56;
	Ranged (TcUpperId _) _ -> cont 57;
	Ranged TcLBracket _ -> cont 58;
	Ranged TcRBracket _ -> cont 59;
	Ranged TcLBrace _ -> cont 60;
	Ranged TcRBrace _ -> cont 61;
	Ranged TcArrow _ -> cont 62;
	Ranged TcDoubleArrow _ -> cont 63;
	Ranged TcLPar _ -> cont 64;
	Ranged TcRPar _ -> cont 65;
	Ranged TcColon _ -> cont 66;
	Ranged TcEqual _ -> cont 67;
	Ranged TcPipe _ -> cont 68;
	Ranged TcComma _ -> cont 69;
	Ranged TcSlash _ -> cont 70;
	Ranged TcDot _ -> cont 71;
	Ranged TcWild _ -> cont 72;
	Ranged TcBegin _ -> cont 73;
	Ranged TcSep _ -> cont 74;
	Ranged TcEnd _ -> cont 75;
	_ -> happyError' (tk, [])
	})

happyError_ explist 76 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Lexer a
happyReturn = (Prelude.return)
happyThen1 :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen1 = happyThen
happyReturn1 :: () => a -> Lexer a
happyReturn1 = happyReturn
happyError' :: () => ((Ranged Token), [Prelude.String]) -> Lexer a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

parseProgram = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


withPosList :: (HasPosition a, HasPosition b) => a -> [b] -> (Range -> c) -> c
withPosList p [] fn = fn (getPos p)
withPosList p xs fn = fn (getPos p <> getPos xs)

withPosListR :: (HasPosition a, HasPosition b) => [b] -> a -> (Range -> c) -> c
withPosListR []     p fn = fn (getPos p)
withPosListR (x: _) p fn = fn (getPos x <> getPos p)


withPos :: (HasPosition a, HasPosition b) => a -> b -> (Range -> c) -> c
withPos p p1 fn = fn (mixRange (getPos p) (getPos p1))

getData :: Ranged Token -> Text
getData = \case
    (Ranged (TcStr s) _)     -> s
    (Ranged (TcLowerId s) _) -> s
    (Ranged (TcUpperId s) _) -> s
    _ -> error "Chiyoku.. you have to be more careful when you try to use this function!"

getInt :: Ranged Token -> Int
getInt = \case
    (Ranged (TcInt s) _) -> s
    _ -> error "Chiyoku.. you have to be more careful when you try to use this function!"

lexer      = (scan >>=)
parseError = throwError . UnexpectedToken
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
