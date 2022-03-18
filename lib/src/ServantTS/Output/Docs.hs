module ServantTS.Output.Docs where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Servant.API
import Servant.Foreign
  ( Foreign,
    GenerateList,
    HasForeign,
    HasForeignType,
    Req,
    listFromAPI,
    typeFor,
    _reqReturnType,
  )
import ServantTS.Convert
import ServantTS.Output.TSFunctions
import Typescript

apiToTSFuncs ::
  (IsForeignType (TSIntermediate flavor)) =>
  [Req (TSIntermediate flavor)] ->
  (Req (TSIntermediate flavor) -> TSFunctionConfig) ->
  FilePath ->
  IO ()
apiToTSFuncs
  apiReqs
  reqToTSFunction'
  funcFileLoc' = do
    writeFile funcFileLoc' $ show $ apiToFunctionDoc apiReqs reqToTSFunction'

tsTypes :: TypescriptType hsType => FilePath -> [Proxy hsType] -> IO ()
tsTypes path types =
  writeFile path . unpack . Data.Text.unlines $ Prelude.map (mkTypescriptDeclaration (Proxy :: Proxy Vanilla)) types

-- | Utility methods for writing the function file
apiToFunctionDoc ::
  (IsForeignType (TSIntermediate flavor)) =>
  [Req (TSIntermediate flavor)] ->
  (Req (TSIntermediate flavor) -> TSFunctionConfig) ->
  Doc ann
apiToFunctionDoc apiReqs reqToTSFunction' =
  mkFunctionDoc $ fmap reqToTSFunction' apiReqs

-- | Utility methods for writing the TYPE DECLARATION file
mkFunctionDoc ::
  [TSFunctionConfig] ->
  Doc ann
mkFunctionDoc tsFunctions = vsep $ fmap (pretty . printTSFunction) tsFunctions

allTypes ::
  [Req a] ->
  [a]
allTypes asTS = fromMaybe [] $ sequence $ _reqReturnType <$> asTS

apiToTypeDeclarationDoc ::
  IsForeignType t =>
  [Req t] ->
  Doc ann
apiToTypeDeclarationDoc asTS =
  vsep $
    fmap (pretty . declaration . toForeignType) (allTypes asTS)
