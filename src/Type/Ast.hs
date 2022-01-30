module Type.Ast where 

import Syntax.Expr 
import Data.List.NonEmpty ()
import Data.Void (Void)
import Data.Text (Text)

data Typed

type instance XName Typed = ((), Text)

type instance XTSimple Typed = NoExt
type instance XTPoly Typed   = NoExt
type instance XTArrow Typed  = ()
type instance XTCons Typed   = ()
type instance XTExt Typed    = Void

type instance XPWild Typed = ()
type instance XPCons Typed = ()
type instance XPLit Typed  = NoExt
type instance XPId Typed   = NoExt
type instance XPExt Typed  = Void

type instance XLChar Typed   = ()
type instance XLString Typed = ()
type instance XLInt Typed    = ()
type instance XLDouble Typed = ()
type instance XLExt Typed    = Void

type instance XLam Typed    = ()
type instance XApp Typed    = ()
type instance XVar Typed    = NoExt
type instance XLit Typed    = NoExt
type instance XMatch Typed  = ()
type instance XAssign Typed =  ()
type instance XBinary Typed =  ()
type instance XBlock Typed  = ()
type instance XIf Typed     = ()
type instance XExt Typed    = Void

type instance XTcSum Typed    = ()
type instance XTcRecord Typed = ()
type instance XTcSyn Typed    = NoExt
type instance XTcExt Typed    = Void

type instance XProg Typed     = NoExt
type instance XLet Typed      = NoExt
type instance XExternal Typed = NoExt
type instance XType Typed     = NoExt
type instance XExternal Typed = NoExt

type instance XBTyped Typed = ()
type instance XBRaw Typed   = NoExt