module Main (main) where

import Relude
import Nuko.Driver            (compile, mainMod)
import Data.Aeson.Text        (encodeToLazyText)
import Data.Aeson             (ToJSON)
import Data.These             (These(These, That, This) )
import Nuko.Report.Render     (renderDiagnostic)

main :: MonadIO m => m ()
main = do
  content <- readFileBS "./examples/normal.nk"
  let res    = compile mainMod "examples/normal.nk" content
  let source = lines (decodeUtf8 content)
  case res of
    That _    -> putTextLn   "✓"
    These e _ -> putTextLn $ "•\n"  <> unlines (map (renderDiagnostic source) e)
    This e    -> putTextLn $ "✗\n"  <> unlines (map (renderDiagnostic source) e)