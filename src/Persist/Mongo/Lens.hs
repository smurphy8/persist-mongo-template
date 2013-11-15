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
--import Persist.Mongo.Settings
import ContentCfgTypes
import Data.Char (toLower)
-- import Data.Text



-- |  Lens naming section ================================

mLowerName :: String -> Maybe String
mLowerName (c:cs) = Just ('l':'e':'n':'s':c:cs)
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



{-|
Dashboard
  owner UserId Maybe
  gId GroupId Maybe
  group Group Maybe
  name Text Maybe default="Stupid"
  notes Text Maybe
  default Bool Maybe
  header HeaderWidget Maybe 
  panels [MenuPanel] Maybe

|-}

-- dashboardLensNames :: [(String,String)]
-- dashboardLensNames = [ 
-- ("owner"    , "lensDashboardOwner"   )
-- ("gId"      , "lensDashboardGid"     )
-- ("group"    , "lensDashboardGroup"   )
-- ("name"     , "lensDashboardName"    )
-- ("notes"    , "lensDashboardNotes"   )
-- ("default"  , "lensDashboardDefault" )
-- ("header"   , "lensDashboardHeader"  )
-- ("panels"   , "lensDashboardPanels")
-- ]  



{-|         
MenuPanel
  bidx BranchIndex 
  open Bool 
  mconfig MConfigObject 
  sub SubObject Maybe
  carr ContentArray Maybe
  name Text Maybe         
|-}







-- ("bidx"     , "bidx")    
-- ("open"     , "open")    
-- ("mconfig"  , "mconfig") 
-- ("sub"      , "sub")     
-- ("carr"     , "carr")    
-- ("name"     , "name")    

