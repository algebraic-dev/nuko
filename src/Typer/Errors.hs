module Typer.Errors ( 
  TypeError(..)
) where

import Typer.Types
import Syntax.Range (Range)
import Data.Text (Text)
import Control.Exception (Exception)
import Error.Message
import Control.Applicative ( Alternative((<|>)) )
import Data.IORef (readIORef)
import GHC.IO (unsafePerformIO)
import Debug.Trace (traceShow)
import Syntax.Parser.Ast (Normal)

import qualified Syntax.Parser.Ast as Ast
import qualified Data.Text         as Text
import qualified Data.Maybe        as Maybe
import qualified Syntax.Range      as Range
import qualified Syntax.Expr       as Expr

-- Types

data TypeError
  = CantUnifyKind (Expr.Typer Normal) (Kind 'Eval) (Kind 'Eval)
  | CantUnifyType (TType 'Eval) (TType 'Eval)
  | OccoursCheckKind (Kind 'Eval) (Kind 'Eval)
  | OccoursCheckType (TType 'Eval) (TType 'Eval)
  | CantFindVar Range Text
  | CantFindKind Range Text
  | NotATypeConstructor (Kind 'Eval)
  | NotEnoughArgs (Kind 'Eval)
  | NotAFunction (TType 'Eval)
  deriving (Show)

instance Exception TypeError where

countArgs :: Kind a -> Int
countArgs = \case
  KFun _ _ kind' -> 1 + countArgs kind'
  KHole _ hole -> do 
    case unsafePerformIO (readIORef hole) of 
      Empty _   -> 0
      Filled ki -> countArgs ki
  _ -> 0

instance ErrorReport TypeError where
    toErrMessage (NotEnoughArgs ki) = 
      let title = [ Normal "Not enough arguments to this type constructor, was expecting "
                  , Normal $ Text.pack $ show (countArgs ki)
                  , Normal " more argument(s)."] 
          pos = orZero $ getPosKind ki
      in
      ErrMessage (Just $ Range.start pos) title
        $ Maybe.catMaybes [ code Red ("Not enough arguments (this type has kind '" <> Text.pack (show ki) <> "')") <$> getPosKind ki]

    toErrMessage (NotATypeConstructor ki) = 
        let title = [ Normal "You cant use an argument in this type because it's not a type constructor!" ] 
            pos = orZero $ getPosKind ki
        in
        ErrMessage (Just $ Range.start pos) title
          $ Maybe.catMaybes [ code Red ("Not a type constructor (this type has kind '" <> Text.pack (show ki) <> "')") <$> getPosKind ki
                            , Just (Desc "Note: try to remove the argument of this type!") ]

    toErrMessage (CantUnifyType ki ki') =
        let title =
              [ Normal "I expected "
              , Colored Red (Text.pack $ show ki)
              , Normal " but instead got ", Colored Blue (Text.pack $ show ki')
              ] 
            pos = orZero $ getPos ki <|> getPos ki'
        in
        ErrMessage (Just $ Range.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The kind here is " <> Text.pack (show ki)) <$> getPos ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPos ki'
            ]

    toErrMessage (OccoursCheckType ki ki') =
        let title =
              [ Normal "Cannot unify the type "
              , Colored Red (Text.pack $ show ki)
              , Normal " with ", Colored Blue (Text.pack $ show ki')
              , Normal " it would cause a infinite type!"
              ] 
            pos = orZero $ getPos ki <|> getPos ki'
        in
        ErrMessage (Just $ Range.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The type here is " <> Text.pack (show ki)) <$> getPos ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPos ki'
            ]

    toErrMessage (CantUnifyKind expr ki ki') =
        let title =
              [ Normal "I expected "
              , Colored Red (Text.pack $ show ki)
              , Normal " but instead got ", Colored Blue (Text.pack $ show ki')
              ] 
            pos = Ast.getPos expr
        in
        ErrMessage (Just $ Range.start pos) title
          [ code Red ("The kind here should be " <> Text.pack (show ki)) (Ast.getPos expr)
          ]

    toErrMessage (OccoursCheckKind ki ki') =
        let title =
              [ Normal "Cannot unify the kind "
              , Colored Red (Text.pack $ show ki)
              , Normal " with ", Colored Blue (Text.pack $ show ki')
              , Normal " it would cause a infinite kind!" ] 
            pos = orZero $ getPosKind ki <|> getPosKind ki'
        in
        ErrMessage (Just $ Range.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The kind here is " <> Text.pack (show ki)) <$> getPosKind ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPosKind ki' ]

    toErrMessage (CantFindVar pos txt) = 
      boundsError pos [Normal "Can't find type of ", Colored Red ("'" <> txt <> "'")]
    
    toErrMessage (CantFindKind pos txt) =
      boundsError pos [Normal "Can't find kind of ", Colored Red ("'" <> txt <> "'")]
    
    toErrMessage (NotAFunction typ) =
        traceShow typ $
        boundsError (orZero $ getPos typ) [Normal "You cannot apply a value of type ", Colored Red (Text.pack $ show typ), Normal " as a function!"]
