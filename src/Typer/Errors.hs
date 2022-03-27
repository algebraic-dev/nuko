module Typer.Errors where

import Typer.Types
import Syntax.Bounds (Bounds)
import Data.Text (Text)
import Data.Data (Typeable)
import Control.Exception (Exception)
import Error.Message
import Control.Applicative
import Data.IORef (readIORef)

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Syntax.Bounds as Bounds
import GHC.IO (unsafePerformIO)

-- Utils

-- Types

data TypeError
  = CantUnifyKind (Kind 'Eval) (Kind 'Eval)
  | CantUnifyType (Type 'Eval) (Type 'Eval)
  | OccoursCheckKind (Kind 'Eval) (Kind 'Eval)
  | OccoursCheckType (Type 'Eval) (Type 'Eval)
  | CantFindVar Bounds Text
  | CantFindKind Bounds Text
  | NotATypeConstructor (Kind 'Eval)
  | NotEnoughArgs (Kind 'Eval)
  deriving (Show, Typeable)

instance Exception TypeError where

countArgs :: Kind a -> Int
countArgs = \case
  KFun _ kind' -> 1 + countArgs kind'
  KLoc _ kind' -> countArgs kind'
  KHole hole -> do 
    case unsafePerformIO (readIORef hole) of 
      Empty _   -> 0
      Filled ki -> countArgs ki
  _ -> 0

instance ErrorReport TypeError where
    toErrMessage = \case
      NotEnoughArgs ki -> 
          let title = [ Normal "Not enough arguments to this type constructor, was expecting "
                      , Normal $ Text.pack $ show (countArgs ki)
                      , Normal " more argument(s)."] 
              pos = orZero $ getPosKind ki
          in
          ErrMessage (Just $ Bounds.start pos) title
            $ Maybe.catMaybes [ code Red ("Not enough arguments (this type has kind '" <> Text.pack (show ki) <> "')") <$> getPosKind ki]

      NotATypeConstructor ki -> 
        let title = [ Normal "You cant use an argument in this type because it's not a type constructor!" ] 
            pos = orZero $ getPosKind ki
        in
        ErrMessage (Just $ Bounds.start pos) title
          $ Maybe.catMaybes [ code Red ("Not a type constructor (this type has kind '" <> Text.pack (show ki) <> "')") <$> getPosKind ki
                            , Just (Desc "Note: try to remove the argument of this type!") ]

      CantUnifyType ki ki' -> 
        let title =
              [ Normal "I expected "
              , Colored Red (Text.pack $ show ki)
              , Normal " but instead got ", Colored Blue (Text.pack $ show ki')
              ] 
            pos = orZero $ getPos ki <|> getPos ki'
        in
        ErrMessage (Just $ Bounds.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The kind here is " <> Text.pack (show ki)) <$> getPos ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPos ki'
            ]

      OccoursCheckType ki ki' ->
        let title =
              [ Normal "Cannot unify the type "
              , Colored Red (Text.pack $ show ki)
              , Normal " with ", Colored Blue (Text.pack $ show ki')
              , Normal " it would cause a infinite type!"
              ] 
            pos = orZero $ getPos ki <|> getPos ki'
        in
        ErrMessage (Just $ Bounds.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The type here is " <> Text.pack (show ki)) <$> getPos ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPos ki'
            ]

      CantUnifyKind ki ki' -> 
        let title =
              [ Normal "I expected "
              , Colored Red (Text.pack $ show ki)
              , Normal " but instead got ", Colored Blue (Text.pack $ show ki')
              ] 
            pos = orZero $ getPosKind ki <|> getPosKind ki'
        in
        ErrMessage (Just $ Bounds.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The kind here is " <> Text.pack (show ki)) <$> getPosKind ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPosKind ki'
            ]

      OccoursCheckKind ki ki' ->
        let title =
              [ Normal "Cannot unify the kind "
              , Colored Red (Text.pack $ show ki)
              , Normal " with ", Colored Blue (Text.pack $ show ki')
              , Normal " it would cause a infinite kind!" ] 
            pos = orZero $ getPosKind ki <|> getPosKind ki'
        in
        ErrMessage (Just $ Bounds.start pos) title
          $ Maybe.catMaybes
            [ code Red ("The kind here is " <> Text.pack (show ki)) <$> getPosKind ki
            , code Blue ("And here is " <> Text.pack (show ki')) <$> getPosKind ki' ]

      CantFindVar pos txt -> boundsError pos [Normal "Can't find type of ", Colored Red ("'" <> txt <> "'")]
      CantFindKind pos txt -> boundsError pos [Normal "Can't find kind of ", Colored Red ("'" <> txt <> "'")]