{-# OPTIONS_GHC -w #-}
module Syntax.Parser where

import Syntax.Lexer.Support (Token(..), Lexer)
import Syntax.Lexer (scan)

import Control.Monad.Except
import Data.Text (Text, append)
import Syntax.Bounds
import Data.Function (on)

import Syntax.Parser.Expr
import Data.Text.Read

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Data.Function (on)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29
	= HappyTerminal (WithBounds Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 ((Pat, Expr))
	| HappyAbsSyn19 ([(Pat, Expr)])
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,345) ([0,0,6,0,0,8192,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,8192,0,0,0,16384,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40960,1025,16,0,8192,0,0,0,0,512,0,0,24576,5120,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,40960,1025,0,0,24576,1024,0,0,0,0,0,0,24576,1024,0,0,0,0,0,0,0,0,2,0,24576,1024,0,0,0,2048,16,0,40960,1025,0,0,57344,1049,4,0,0,0,2,0,0,0,0,0,0,0,0,0,8192,8192,0,0,16384,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,8192,32,0,0,0,0,0,0,0,8,0,24576,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,32704,0,0,0,0,0,0,0,0,0,57344,1025,0,0,0,0,0,0,0,16384,0,0,57344,1049,4,0,57344,1049,4,0,40960,1025,0,0,0,2048,0,0,0,0,0,0,24576,1024,0,0,0,2048,2,0,0,0,0,0,0,0,0,0,24576,1024,0,0,0,0,0,0,0,2048,2,0,0,0,2,0,0,2048,16,0,0,2048,0,0,0,32,0,0,57344,1049,4,0,0,0,0,0,0,0,0,0,0,0,32704,0,57344,1049,4,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1025,0,0,57344,1049,4,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,24576,1024,0,0,24576,1024,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31744,0,0,0,31744,0,0,0,32512,0,0,0,32512,0,0,32768,0,0,0,0,1,0,0,16384,0,0,0,0,0,0,57344,1049,4,0,0,0,0,0,0,0,0,0,40960,1025,0,0,0,0,0,0,57344,1049,4,0,0,0,0,0,0,0,2,0,0,0,0,0,0,32768,1,0,40960,1025,0,0,0,0,0,0,57344,1049,4,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseDecls","LowerId","UpperId","Lit","RawType","Types","Type","Pat","Ann","Atom","Assign","Expr","Call","Sttms","Args","CaseClause","CaseClauses","Case","TypesGen","SumClause","ProductClause","SumClauses","ProductClauses","TypeDeclBody","OptRet","Decl","Decls","lower","upper","str","num","let","type","do","case","of","if","then","else","'='","'('","')'","'{'","'}'","open","end","close","'->'","'\\\\'","'|'","':'","','","'+'","'-'","'*'","'/'","'>'","'<'","'>='","'<='","'=='","%eof"]
        bit_start = st Prelude.* 64
        bit_end = (st Prelude.+ 1) Prelude.* 64
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..63]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (34) = happyShift action_5
action_0 (35) = happyShift action_6
action_0 (28) = happyGoto action_3
action_0 (29) = happyGoto action_4
action_0 _ = happyReduce_64

action_1 (30) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (34) = happyShift action_5
action_3 (35) = happyShift action_6
action_3 (28) = happyGoto action_3
action_3 (29) = happyGoto action_10
action_3 _ = happyReduce_64

action_4 (64) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (30) = happyShift action_2
action_5 (4) = happyGoto action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (31) = happyShift action_8
action_6 (5) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (30) = happyShift action_2
action_7 (4) = happyGoto action_12
action_7 (21) = happyGoto action_13
action_7 _ = happyReduce_48

action_8 _ = happyReduce_2

action_9 (17) = happyGoto action_11
action_9 _ = happyReduce_42

action_10 _ = happyReduce_65

action_11 (30) = happyShift action_2
action_11 (32) = happyShift action_21
action_11 (33) = happyShift action_22
action_11 (43) = happyShift action_23
action_11 (53) = happyShift action_24
action_11 (4) = happyGoto action_16
action_11 (6) = happyGoto action_17
action_11 (10) = happyGoto action_18
action_11 (11) = happyGoto action_19
action_11 (27) = happyGoto action_20
action_11 _ = happyReduce_60

action_12 (30) = happyShift action_2
action_12 (4) = happyGoto action_12
action_12 (21) = happyGoto action_15
action_12 _ = happyReduce_48

action_13 (42) = happyShift action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (30) = happyShift action_2
action_14 (31) = happyShift action_8
action_14 (43) = happyShift action_29
action_14 (45) = happyShift action_36
action_14 (52) = happyShift action_37
action_14 (4) = happyGoto action_25
action_14 (5) = happyGoto action_26
action_14 (7) = happyGoto action_27
action_14 (9) = happyGoto action_33
action_14 (22) = happyGoto action_34
action_14 (26) = happyGoto action_35
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_49

action_16 _ = happyReduce_13

action_17 _ = happyReduce_14

action_18 _ = happyReduce_16

action_19 _ = happyReduce_43

action_20 (42) = happyShift action_32
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_3

action_22 _ = happyReduce_4

action_23 (30) = happyShift action_2
action_23 (32) = happyShift action_21
action_23 (33) = happyShift action_22
action_23 (43) = happyShift action_31
action_23 (4) = happyGoto action_16
action_23 (6) = happyGoto action_17
action_23 (10) = happyGoto action_30
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (30) = happyShift action_2
action_24 (31) = happyShift action_8
action_24 (43) = happyShift action_29
action_24 (4) = happyGoto action_25
action_24 (5) = happyGoto action_26
action_24 (7) = happyGoto action_27
action_24 (9) = happyGoto action_28
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_5

action_26 (30) = happyShift action_2
action_26 (31) = happyShift action_8
action_26 (43) = happyShift action_29
action_26 (4) = happyGoto action_25
action_26 (5) = happyGoto action_62
action_26 (7) = happyGoto action_63
action_26 _ = happyReduce_6

action_27 _ = happyReduce_10

action_28 (50) = happyShift action_44
action_28 _ = happyReduce_61

action_29 (30) = happyShift action_2
action_29 (31) = happyShift action_8
action_29 (43) = happyShift action_29
action_29 (4) = happyGoto action_25
action_29 (5) = happyGoto action_26
action_29 (7) = happyGoto action_27
action_29 (9) = happyGoto action_61
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (44) = happyShift action_59
action_30 (53) = happyShift action_60
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (30) = happyShift action_2
action_31 (32) = happyShift action_21
action_31 (33) = happyShift action_22
action_31 (43) = happyShift action_31
action_31 (4) = happyGoto action_16
action_31 (6) = happyGoto action_17
action_31 (10) = happyGoto action_58
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (30) = happyShift action_2
action_32 (31) = happyShift action_8
action_32 (32) = happyShift action_21
action_32 (33) = happyShift action_22
action_32 (36) = happyShift action_54
action_32 (37) = happyShift action_55
action_32 (43) = happyShift action_56
action_32 (51) = happyShift action_57
action_32 (4) = happyGoto action_45
action_32 (5) = happyGoto action_46
action_32 (6) = happyGoto action_47
action_32 (10) = happyGoto action_18
action_32 (11) = happyGoto action_48
action_32 (12) = happyGoto action_49
action_32 (13) = happyGoto action_50
action_32 (14) = happyGoto action_51
action_32 (15) = happyGoto action_52
action_32 (20) = happyGoto action_53
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (50) = happyShift action_44
action_33 _ = happyReduce_59

action_34 (24) = happyGoto action_43
action_34 _ = happyReduce_52

action_35 _ = happyReduce_62

action_36 (30) = happyShift action_2
action_36 (46) = happyShift action_42
action_36 (4) = happyGoto action_39
action_36 (23) = happyGoto action_40
action_36 (25) = happyGoto action_41
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (31) = happyShift action_8
action_37 (5) = happyGoto action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (8) = happyGoto action_91
action_38 _ = happyReduce_8

action_39 (53) = happyShift action_90
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_54

action_41 (46) = happyShift action_88
action_41 (54) = happyShift action_89
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_58

action_43 (52) = happyShift action_37
action_43 (22) = happyGoto action_87
action_43 _ = happyReduce_56

action_44 (30) = happyShift action_2
action_44 (31) = happyShift action_8
action_44 (43) = happyShift action_29
action_44 (4) = happyGoto action_25
action_44 (5) = happyGoto action_62
action_44 (7) = happyGoto action_86
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (42) = happyReduce_13
action_45 (44) = happyReduce_19
action_45 (53) = happyReduce_13
action_45 _ = happyReduce_19

action_46 _ = happyReduce_20

action_47 (42) = happyReduce_14
action_47 (44) = happyReduce_18
action_47 (53) = happyReduce_14
action_47 _ = happyReduce_18

action_48 (42) = happyShift action_85
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (55) = happyShift action_76
action_49 (56) = happyShift action_77
action_49 (57) = happyShift action_78
action_49 (58) = happyShift action_79
action_49 (59) = happyShift action_80
action_49 (60) = happyShift action_81
action_49 (61) = happyShift action_82
action_49 (62) = happyShift action_83
action_49 (63) = happyShift action_84
action_49 _ = happyReduce_38

action_50 _ = happyReduce_35

action_51 _ = happyReduce_63

action_52 (30) = happyShift action_2
action_52 (31) = happyShift action_8
action_52 (32) = happyShift action_21
action_52 (33) = happyShift action_22
action_52 (43) = happyShift action_75
action_52 (4) = happyGoto action_72
action_52 (5) = happyGoto action_46
action_52 (6) = happyGoto action_73
action_52 (12) = happyGoto action_74
action_52 _ = happyReduce_33

action_53 _ = happyReduce_36

action_54 (47) = happyShift action_71
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (30) = happyShift action_2
action_55 (31) = happyShift action_8
action_55 (32) = happyShift action_21
action_55 (33) = happyShift action_22
action_55 (36) = happyShift action_54
action_55 (37) = happyShift action_55
action_55 (43) = happyShift action_56
action_55 (51) = happyShift action_57
action_55 (4) = happyGoto action_45
action_55 (5) = happyGoto action_46
action_55 (6) = happyGoto action_47
action_55 (10) = happyGoto action_18
action_55 (11) = happyGoto action_48
action_55 (12) = happyGoto action_49
action_55 (13) = happyGoto action_50
action_55 (14) = happyGoto action_70
action_55 (15) = happyGoto action_52
action_55 (20) = happyGoto action_53
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (30) = happyShift action_2
action_56 (31) = happyShift action_8
action_56 (32) = happyShift action_21
action_56 (33) = happyShift action_22
action_56 (36) = happyShift action_54
action_56 (37) = happyShift action_55
action_56 (43) = happyShift action_56
action_56 (51) = happyShift action_57
action_56 (4) = happyGoto action_45
action_56 (5) = happyGoto action_46
action_56 (6) = happyGoto action_47
action_56 (10) = happyGoto action_68
action_56 (11) = happyGoto action_48
action_56 (12) = happyGoto action_49
action_56 (13) = happyGoto action_50
action_56 (14) = happyGoto action_69
action_56 (15) = happyGoto action_52
action_56 (20) = happyGoto action_53
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (30) = happyShift action_2
action_57 (32) = happyShift action_21
action_57 (33) = happyShift action_22
action_57 (43) = happyShift action_23
action_57 (4) = happyGoto action_16
action_57 (6) = happyGoto action_17
action_57 (10) = happyGoto action_18
action_57 (11) = happyGoto action_67
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (44) = happyShift action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_15

action_60 (30) = happyShift action_2
action_60 (31) = happyShift action_8
action_60 (43) = happyShift action_29
action_60 (4) = happyGoto action_25
action_60 (5) = happyGoto action_26
action_60 (7) = happyGoto action_27
action_60 (9) = happyGoto action_66
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (44) = happyShift action_65
action_61 (50) = happyShift action_44
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_6

action_63 (8) = happyGoto action_64
action_63 _ = happyReduce_8

action_64 (30) = happyShift action_2
action_64 (31) = happyShift action_8
action_64 (43) = happyShift action_29
action_64 (4) = happyGoto action_25
action_64 (5) = happyGoto action_62
action_64 (7) = happyGoto action_92
action_64 _ = happyReduce_11

action_65 _ = happyReduce_7

action_66 (44) = happyShift action_110
action_66 (50) = happyShift action_44
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (50) = happyShift action_109
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (44) = happyShift action_59
action_68 (53) = happyShift action_60
action_68 _ = happyReduce_16

action_69 (44) = happyShift action_108
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (38) = happyShift action_107
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (30) = happyShift action_2
action_71 (31) = happyShift action_8
action_71 (32) = happyShift action_21
action_71 (33) = happyShift action_22
action_71 (36) = happyShift action_54
action_71 (37) = happyShift action_55
action_71 (43) = happyShift action_56
action_71 (51) = happyShift action_57
action_71 (4) = happyGoto action_45
action_71 (5) = happyGoto action_46
action_71 (6) = happyGoto action_47
action_71 (10) = happyGoto action_18
action_71 (11) = happyGoto action_48
action_71 (12) = happyGoto action_49
action_71 (13) = happyGoto action_50
action_71 (14) = happyGoto action_105
action_71 (15) = happyGoto action_52
action_71 (16) = happyGoto action_106
action_71 (20) = happyGoto action_53
action_71 _ = happyReduce_39

action_72 _ = happyReduce_19

action_73 _ = happyReduce_18

action_74 (55) = happyShift action_76
action_74 (56) = happyShift action_77
action_74 (57) = happyShift action_78
action_74 (58) = happyShift action_79
action_74 (59) = happyShift action_80
action_74 (60) = happyShift action_81
action_74 (61) = happyShift action_82
action_74 (62) = happyShift action_83
action_74 (63) = happyShift action_84
action_74 _ = happyReduce_37

action_75 (30) = happyShift action_2
action_75 (31) = happyShift action_8
action_75 (32) = happyShift action_21
action_75 (33) = happyShift action_22
action_75 (36) = happyShift action_54
action_75 (37) = happyShift action_55
action_75 (43) = happyShift action_56
action_75 (51) = happyShift action_57
action_75 (4) = happyGoto action_45
action_75 (5) = happyGoto action_46
action_75 (6) = happyGoto action_47
action_75 (10) = happyGoto action_18
action_75 (11) = happyGoto action_48
action_75 (12) = happyGoto action_49
action_75 (13) = happyGoto action_50
action_75 (14) = happyGoto action_69
action_75 (15) = happyGoto action_52
action_75 (20) = happyGoto action_53
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (30) = happyShift action_2
action_76 (31) = happyShift action_8
action_76 (32) = happyShift action_21
action_76 (33) = happyShift action_22
action_76 (43) = happyShift action_75
action_76 (4) = happyGoto action_72
action_76 (5) = happyGoto action_46
action_76 (6) = happyGoto action_73
action_76 (12) = happyGoto action_104
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (30) = happyShift action_2
action_77 (31) = happyShift action_8
action_77 (32) = happyShift action_21
action_77 (33) = happyShift action_22
action_77 (43) = happyShift action_75
action_77 (4) = happyGoto action_72
action_77 (5) = happyGoto action_46
action_77 (6) = happyGoto action_73
action_77 (12) = happyGoto action_103
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (30) = happyShift action_2
action_78 (31) = happyShift action_8
action_78 (32) = happyShift action_21
action_78 (33) = happyShift action_22
action_78 (43) = happyShift action_75
action_78 (4) = happyGoto action_72
action_78 (5) = happyGoto action_46
action_78 (6) = happyGoto action_73
action_78 (12) = happyGoto action_102
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (30) = happyShift action_2
action_79 (31) = happyShift action_8
action_79 (32) = happyShift action_21
action_79 (33) = happyShift action_22
action_79 (43) = happyShift action_75
action_79 (4) = happyGoto action_72
action_79 (5) = happyGoto action_46
action_79 (6) = happyGoto action_73
action_79 (12) = happyGoto action_101
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (30) = happyShift action_2
action_80 (31) = happyShift action_8
action_80 (32) = happyShift action_21
action_80 (33) = happyShift action_22
action_80 (43) = happyShift action_75
action_80 (4) = happyGoto action_72
action_80 (5) = happyGoto action_46
action_80 (6) = happyGoto action_73
action_80 (12) = happyGoto action_100
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (30) = happyShift action_2
action_81 (31) = happyShift action_8
action_81 (32) = happyShift action_21
action_81 (33) = happyShift action_22
action_81 (43) = happyShift action_75
action_81 (4) = happyGoto action_72
action_81 (5) = happyGoto action_46
action_81 (6) = happyGoto action_73
action_81 (12) = happyGoto action_99
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (30) = happyShift action_2
action_82 (31) = happyShift action_8
action_82 (32) = happyShift action_21
action_82 (33) = happyShift action_22
action_82 (43) = happyShift action_75
action_82 (4) = happyGoto action_72
action_82 (5) = happyGoto action_46
action_82 (6) = happyGoto action_73
action_82 (12) = happyGoto action_98
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (30) = happyShift action_2
action_83 (31) = happyShift action_8
action_83 (32) = happyShift action_21
action_83 (33) = happyShift action_22
action_83 (43) = happyShift action_75
action_83 (4) = happyGoto action_72
action_83 (5) = happyGoto action_46
action_83 (6) = happyGoto action_73
action_83 (12) = happyGoto action_97
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (30) = happyShift action_2
action_84 (31) = happyShift action_8
action_84 (32) = happyShift action_21
action_84 (33) = happyShift action_22
action_84 (43) = happyShift action_75
action_84 (4) = happyGoto action_72
action_84 (5) = happyGoto action_46
action_84 (6) = happyGoto action_73
action_84 (12) = happyGoto action_96
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (30) = happyShift action_2
action_85 (31) = happyShift action_8
action_85 (32) = happyShift action_21
action_85 (33) = happyShift action_22
action_85 (36) = happyShift action_54
action_85 (37) = happyShift action_55
action_85 (43) = happyShift action_56
action_85 (51) = happyShift action_57
action_85 (4) = happyGoto action_45
action_85 (5) = happyGoto action_46
action_85 (6) = happyGoto action_47
action_85 (10) = happyGoto action_18
action_85 (11) = happyGoto action_48
action_85 (12) = happyGoto action_49
action_85 (13) = happyGoto action_50
action_85 (14) = happyGoto action_95
action_85 (15) = happyGoto action_52
action_85 (20) = happyGoto action_53
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_12

action_87 _ = happyReduce_53

action_88 _ = happyReduce_57

action_89 (30) = happyShift action_2
action_89 (4) = happyGoto action_39
action_89 (23) = happyGoto action_94
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (30) = happyShift action_2
action_90 (31) = happyShift action_8
action_90 (43) = happyShift action_29
action_90 (4) = happyGoto action_25
action_90 (5) = happyGoto action_26
action_90 (7) = happyGoto action_27
action_90 (9) = happyGoto action_93
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (30) = happyShift action_2
action_91 (31) = happyShift action_8
action_91 (43) = happyShift action_29
action_91 (4) = happyGoto action_25
action_91 (5) = happyGoto action_62
action_91 (7) = happyGoto action_92
action_91 _ = happyReduce_50

action_92 _ = happyReduce_9

action_93 (50) = happyShift action_44
action_93 _ = happyReduce_51

action_94 _ = happyReduce_55

action_95 _ = happyReduce_31

action_96 (59) = happyFail []
action_96 (60) = happyFail []
action_96 (61) = happyFail []
action_96 (62) = happyFail []
action_96 (63) = happyFail []
action_96 _ = happyReduce_30

action_97 (59) = happyFail []
action_97 (60) = happyFail []
action_97 (61) = happyFail []
action_97 (62) = happyFail []
action_97 (63) = happyFail []
action_97 _ = happyReduce_29

action_98 (59) = happyFail []
action_98 (60) = happyFail []
action_98 (61) = happyFail []
action_98 (62) = happyFail []
action_98 (63) = happyFail []
action_98 _ = happyReduce_28

action_99 (59) = happyFail []
action_99 (60) = happyFail []
action_99 (61) = happyFail []
action_99 (62) = happyFail []
action_99 (63) = happyFail []
action_99 _ = happyReduce_27

action_100 (59) = happyFail []
action_100 (60) = happyFail []
action_100 (61) = happyFail []
action_100 (62) = happyFail []
action_100 (63) = happyFail []
action_100 _ = happyReduce_26

action_101 (59) = happyShift action_80
action_101 (60) = happyShift action_81
action_101 (61) = happyShift action_82
action_101 (62) = happyShift action_83
action_101 (63) = happyShift action_84
action_101 _ = happyReduce_25

action_102 (59) = happyShift action_80
action_102 (60) = happyShift action_81
action_102 (61) = happyShift action_82
action_102 (62) = happyShift action_83
action_102 (63) = happyShift action_84
action_102 _ = happyReduce_24

action_103 (57) = happyShift action_78
action_103 (58) = happyShift action_79
action_103 (59) = happyShift action_80
action_103 (60) = happyShift action_81
action_103 (61) = happyShift action_82
action_103 (62) = happyShift action_83
action_103 (63) = happyShift action_84
action_103 _ = happyReduce_23

action_104 (57) = happyShift action_78
action_104 (58) = happyShift action_79
action_104 (59) = happyShift action_80
action_104 (60) = happyShift action_81
action_104 (61) = happyShift action_82
action_104 (62) = happyShift action_83
action_104 (63) = happyShift action_84
action_104 _ = happyReduce_22

action_105 (48) = happyShift action_114
action_105 _ = happyReduce_40

action_106 (49) = happyShift action_113
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (47) = happyShift action_112
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_21

action_109 (30) = happyShift action_2
action_109 (31) = happyShift action_8
action_109 (32) = happyShift action_21
action_109 (33) = happyShift action_22
action_109 (36) = happyShift action_54
action_109 (37) = happyShift action_55
action_109 (43) = happyShift action_56
action_109 (51) = happyShift action_57
action_109 (4) = happyGoto action_45
action_109 (5) = happyGoto action_46
action_109 (6) = happyGoto action_47
action_109 (10) = happyGoto action_18
action_109 (11) = happyGoto action_48
action_109 (12) = happyGoto action_49
action_109 (13) = happyGoto action_50
action_109 (14) = happyGoto action_111
action_109 (15) = happyGoto action_52
action_109 (20) = happyGoto action_53
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_17

action_111 _ = happyReduce_32

action_112 (30) = happyShift action_2
action_112 (32) = happyShift action_21
action_112 (33) = happyShift action_22
action_112 (43) = happyShift action_31
action_112 (4) = happyGoto action_16
action_112 (6) = happyGoto action_17
action_112 (10) = happyGoto action_116
action_112 (18) = happyGoto action_117
action_112 (19) = happyGoto action_118
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_34

action_114 (30) = happyShift action_2
action_114 (31) = happyShift action_8
action_114 (32) = happyShift action_21
action_114 (33) = happyShift action_22
action_114 (36) = happyShift action_54
action_114 (37) = happyShift action_55
action_114 (43) = happyShift action_56
action_114 (51) = happyShift action_57
action_114 (4) = happyGoto action_45
action_114 (5) = happyGoto action_46
action_114 (6) = happyGoto action_47
action_114 (10) = happyGoto action_18
action_114 (11) = happyGoto action_48
action_114 (12) = happyGoto action_49
action_114 (13) = happyGoto action_50
action_114 (14) = happyGoto action_105
action_114 (15) = happyGoto action_52
action_114 (16) = happyGoto action_115
action_114 (20) = happyGoto action_53
action_114 _ = happyReduce_39

action_115 _ = happyReduce_41

action_116 (50) = happyShift action_121
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_45

action_118 (48) = happyShift action_119
action_118 (49) = happyShift action_120
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (30) = happyShift action_2
action_119 (32) = happyShift action_21
action_119 (33) = happyShift action_22
action_119 (43) = happyShift action_31
action_119 (4) = happyGoto action_16
action_119 (6) = happyGoto action_17
action_119 (10) = happyGoto action_116
action_119 (18) = happyGoto action_123
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_47

action_121 (30) = happyShift action_2
action_121 (31) = happyShift action_8
action_121 (32) = happyShift action_21
action_121 (33) = happyShift action_22
action_121 (36) = happyShift action_54
action_121 (37) = happyShift action_55
action_121 (43) = happyShift action_56
action_121 (51) = happyShift action_57
action_121 (4) = happyGoto action_45
action_121 (5) = happyGoto action_46
action_121 (6) = happyGoto action_47
action_121 (10) = happyGoto action_18
action_121 (11) = happyGoto action_48
action_121 (12) = happyGoto action_49
action_121 (13) = happyGoto action_50
action_121 (14) = happyGoto action_122
action_121 (15) = happyGoto action_52
action_121 (20) = happyGoto action_53
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_44

action_123 _ = happyReduce_46

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (WithBounds (getText happy_var_1) (bounds happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (WithBounds (getText happy_var_1) (bounds happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (LString (bounds happy_var_1) (getText happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (LNumber (bounds happy_var_1) (unpack . decimal . getText $ happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (TGen happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (TSimple happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 ([]
	)

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 (TCons (mixNonEmpty (TSimple happy_var_1 :| happy_var_2 : happy_var_3)) happy_var_1 (happy_var_2 :| happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (TArrow (mixBounds (getPos happy_var_1) (getPos happy_var_3)) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn10
		 (PId happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn10
		 (PLit happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (Raw happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Ann (mixBounds (getPos happy_var_1) (getPos happy_var_4)) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 (ELit happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (EId happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 (EId happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EBinary (mixPos happy_var_1 happy_var_3) (symbolToId happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (EAssign (mixBounds (getPos happy_var_1) (getPos happy_var_3)) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 14 happyReduction_32
happyReduction_32 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (ELambda (mixBounds (getPos happy_var_1) (getPos happy_var_4)) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 14 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (EBlock (mixBounds (getPos happy_var_1) (headOr happy_var_3 (getPos happy_var_1))) (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  14 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  14 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  15 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (ECall (mixPos happy_var_1 happy_var_2) happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  15 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  16 happyReduction_39
happyReduction_39  =  HappyAbsSyn16
		 ([]
	)

happyReduce_40 = happySpecReduce_1  16 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  16 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  17 happyReduction_42
happyReduction_42  =  HappyAbsSyn17
		 ([]
	)

happyReduce_43 = happySpecReduce_2  17 happyReduction_43
happyReduction_43 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_2 : happy_var_1
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, happy_var_3)
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  19 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  19 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_3 : happy_var_1
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 6 20 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (ECase (mixPos happy_var_2 (snd $ last happy_var_5)) happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_0  21 happyReduction_48
happyReduction_48  =  HappyAbsSyn21
		 ([]
	)

happyReduce_49 = happySpecReduce_2  21 happyReduction_49
happyReduction_49 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  22 happyReduction_50
happyReduction_50 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn22
		 ((happy_var_2, reverse happy_var_3)
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  23 happyReduction_51
happyReduction_51 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_1, happy_var_3)
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  24 happyReduction_52
happyReduction_52  =  HappyAbsSyn24
		 ([]
	)

happyReduce_53 = happySpecReduce_2  24 happyReduction_53
happyReduction_53 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_2 : happy_var_1
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  25 happyReduction_55
happyReduction_55 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_3 : happy_var_1
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  26 happyReduction_56
happyReduction_56 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn26
		 (TDSumType (happy_var_1 :| reverse happy_var_2)
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  26 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (TDProductType (reverse happy_var_2)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  26 happyReduction_58
happyReduction_58 _
	_
	 =  HappyAbsSyn26
		 (TDProductType []
	)

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn26
		 (TDSynonym happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  27 happyReduction_60
happyReduction_60  =  HappyAbsSyn27
		 (Nothing
	)

happyReduce_61 = happySpecReduce_2  27 happyReduction_61
happyReduction_61 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Just happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happyReduce 5 28 happyReduction_62
happyReduction_62 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (DType happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 6 28 happyReduction_63
happyReduction_63 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (DLet happy_var_2 (reverse happy_var_3) happy_var_6 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_0  29 happyReduction_64
happyReduction_64  =  HappyAbsSyn29
		 ([]
	)

happyReduce_65 = happySpecReduce_2  29 happyReduction_65
happyReduction_65 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 : happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	WithBounds TknEOF _ -> action 64 64 tk (HappyState action) sts stk;
	WithBounds (TknLowerId _) _ -> cont 30;
	WithBounds (TknUpperId _) _ -> cont 31;
	WithBounds (TknLStr _) _ -> cont 32;
	WithBounds (TknNumber _) _ -> cont 33;
	WithBounds TknKwLet _ -> cont 34;
	WithBounds TknKwType _ -> cont 35;
	WithBounds TknKwDo _ -> cont 36;
	WithBounds TknKwCase _ -> cont 37;
	WithBounds TknKwOf _ -> cont 38;
	WithBounds TknKwIf _ -> cont 39;
	WithBounds TknKwThen _ -> cont 40;
	WithBounds TknKwElse _ -> cont 41;
	WithBounds TknEq _ -> cont 42;
	WithBounds TknLPar _ -> cont 43;
	WithBounds TknRPar _ -> cont 44;
	WithBounds TknLBrace _ -> cont 45;
	WithBounds TknRBrace _ -> cont 46;
	WithBounds TknOpen _ -> cont 47;
	WithBounds TknEnd _ -> cont 48;
	WithBounds TknClose _ -> cont 49;
	WithBounds TknRArrow _ -> cont 50;
	WithBounds TknSlash _ -> cont 51;
	WithBounds TknPipe _ -> cont 52;
	WithBounds TknColon _ -> cont 53;
	WithBounds TknComma _ -> cont 54;
	WithBounds (TknSymbol "+") _ -> cont 55;
	WithBounds (TknSymbol "-") _ -> cont 56;
	WithBounds (TknSymbol "*") _ -> cont 57;
	WithBounds (TknSymbol "/") _ -> cont 58;
	WithBounds (TknSymbol ">") _ -> cont 59;
	WithBounds (TknSymbol "<") _ -> cont 60;
	WithBounds (TknSymbol ">=") _ -> cont 61;
	WithBounds (TknSymbol "<=") _ -> cont 62;
	WithBounds (TknSymbol "==") _ -> cont 63;
	_ -> happyError' (tk, [])
	})

happyError_ explist 64 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Lexer a
happyReturn = (Prelude.return)
happyThen1 :: () => Lexer a -> (a -> Lexer b) -> Lexer b
happyThen1 = happyThen
happyReturn1 :: () => a -> Lexer a
happyReturn1 = happyReturn
happyError' :: () => ((WithBounds Token), [Prelude.String]) -> Lexer a
happyError' tk = parseError tk
parseDecls = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Partial functions that i use because

symbolToId (WithBounds (TknSymbol n) b) = WithBounds n b

headOr :: HasPosition a => [a] -> Bounds -> Bounds
headOr [] other   = other 
headOr (x : xs) _ = getPos x

mixNEBounds ne = mixBounds (NE.head ne) (NE.last ne)

mixPos :: HasPosition a => a -> a -> Bounds
mixPos = mixBounds `on` getPos

mixNonEmpty ne = mixBounds (getPos $ NE.head ne) (getPos $ NE.last ne)

unpack (Right (r, _)) = r

getText :: WithBounds Token -> Text
getText (WithBounds (TknLowerId tx) _) = tx
getText (WithBounds (TknUpperId tx) _) = tx
getText (WithBounds (TknLStr tx) _)    = tx
getText (WithBounds (TknNumber tx) _) = tx

-- Happy primitives

lexer = (scan >>=)
parseError = throwError . show
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
