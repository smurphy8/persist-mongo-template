{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}

module Persist.Mongo.Lens where


import Control.Lens
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import qualified Data.Foldable as F
import ContentCfgTypes
import Data.Char (toLower,toUpper)



-- |  Lens naming section ================================

mLowerName :: String -> Maybe String
mLowerName (c:cs) = Just ('l':'e':'n':'s':(toUpper c):cs)
mLowerName _ = Nothing


-- | Default 'LensRules'.
persistDefaultRules :: LensRules
persistDefaultRules = LensRules mLowerName fld (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass, CreateInstance, BuildTraversals, GenerateSignatures]
  where
    fld cs = mLowerName cs


-- | Rules for making lenses and traversals that precompose another 'Lens'. that won't interfere with Yesod Scaffold
persistClassyRules :: LensRules
persistClassyRules = persistDefaultRules
  & lensIso .~ const Nothing
  & handleSingletons .~ False
  & lensClass .~ classy
  & classRequired .~ True
  & partialLenses .~ False
  & buildTraversals .~ True
  where
    classy :: String -> Maybe (String, String)
    classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
    classy _ = Nothing

persistMakeClassy :: Name -> Q [Dec]
persistMakeClassy = makeLensesWith persistClassyRules





