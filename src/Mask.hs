{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveGeneric,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Mask where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Monad
import Control.Lens
-- import Control.Lens.At
import GHC.Generics
-- import qualified Data.Maybe as MB
import qualified Data.HashMap.Strict as M
-- import Language.StructuredScript
import Language.StructuredScript.Parsers 
import Mask.Types
import Mask.BuiltIns
import Data.SafeCopy
import Persist.Mongo.Settings
import Data.Serialize hiding (get)
import SafeCopy () 
import qualified Data.Aeson as A
import Text.Read (readMaybe )
import Control.Applicative hiding (Const)
-- import Import
import qualified Data.Set as S
import qualified Data.List as L
import Yesod hiding (runDB)
import qualified Data.Traversable as T
import Data.Traversable.Compat
import Data.Text hiding (zip)
import Data.Text.Read

testCompile :: String
testCompile = "tst"
 
-- | Add your builtIn Functions here, BuiltIns and in Types

maskLookup :: MaskData  -> IO MaskFcn
maskLookup (MaskData DivByTen  _) =  return $ OneVar $ divBy10
maskLookup (MaskData UserDefined  um) =  UserDef <$> (tagMapTransform um)
-- maskLookup (MaskData _  um ) =  return $ OneVar $ (\x -> Right x)
    
maskPullOut :: MaskFcn -> Const -> Either String Const
maskPullOut (OneVar f ) = f
maskPullOut (UserDef f) = f

exampleUserMask :: Either String UserMask
exampleUserMask = UserMask <$>  sstStmtTree <*>  sstILookup
   where
     sstStmtTree = sstParse testString
     sstILookup =  Right sampleTagIMap

-- | triggerTransform allows even single Variable functions to be defined with DB paramters

triggerTransform :: UserMask ->
                    (VarTable -> Const -> Either String Const) ->  IO ( Const -> Either String Const)
triggerTransform (UserMask _ tm) fcn = do
  vtReady <- M.traverseWithKey keyTraverseFcn tm
  return $ fcn (VT vtReady)


keyTraverseFcn :: Text -> (TagTarget Int) -> IO Const
keyTraverseFcn _  (TagTarget i _ ) = do
  let eTextToDouble :: Entity OnpingTagCombined  -> Maybe Double
      eTextToDouble = (onpingTagCombinedResult.entityVal) >=> (readMaybe.unpack)
  v <- runDB $ selectFirst [OnpingTagCombinedPid ==. (Just i)] []
  case (v >>= eTextToDouble) of
    Just opv -> return (ConstDouble opv)
    Nothing  -> fail "No value found"


tagMapTransform :: UserMask -> IO (Const -> Either String Const)
tagMapTransform (UserMask stmt tm)  = do
  vtReady <- M.traverseWithKey keyTraverseFcn tm
  return $ runner vtReady
    where
      runner vt x = let vtNew = VT (M.insert "input1" x vt)
                    in (sstEval vtNew stmt ) >>= sstLookupOutput

insertPidKeys :: UserMask -> [Int] -> UserMask
insertPidKeys (UserMask stmt _) pList = do 
  let tFcn x = TagTarget x TagCombined
      iFcn x = append "input" (pack.show $ x)
  UserMask stmt (M.fromList [(iFcn i, tFcn p) | (i,p) <- (zip ( [1 ..]::[Int]) pList) ] )


mkUserMask :: Stmt -> UserMask 
mkUserMask stmts = emptyUserMask { getStmtTree = stmts}

mkMaskDataStore :: BuiltInId -> UserMask -> MaskDataStore 
mkMaskDataStore b u = maskDataEncode $ MaskData b u

maskDataDecode :: MaskDataStore -> Either String MaskData
maskDataDecode (MaskDataStore maskBS) = runGet safeGet maskBS

maskDataEncode :: MaskData -> MaskDataStore 
maskDataEncode md = MaskDataStore $ runPut $ safePut $ md

mkStmt :: SSTConfig -> Either String Stmt 
mkStmt (SSTConfig _ _ wc) = sstParse (unpack wc)

-- |pKey is always the first Int value of [Int]

makeMaskTypeIdFromJSON :: Text -> Maybe MaskTypeId
makeMaskTypeIdFromJSON = cnvServe.cnv
    where 
      cnv :: Text -> A.Value 
      cnv = A.toJSON.unpack
      cnvServe :: (Value -> Maybe MaskTypeId)
      cnvServe t = case A.fromJSON t of 
                     (A.Success l) -> Just l
                     (A.Error _) -> Nothing


data SSTConfig = SSTConfig{
                         inputValues :: [Integer], -- Test values used in mask creation
                         inputNames  :: [Text],    -- Names of various input labels in order
                         workingCode ::  Text --structured script code body
} deriving (Show, Eq, Generic)

instance A.ToJSON SSTConfig
instance A.FromJSON SSTConfig


testStructuredScript :: SSTConfig
testStructuredScript = SSTConfig [] [] ""




decodeMaskAssignConfig :: A.Value -> A.Result MaskAssignConfig
decodeMaskAssignConfig = A.fromJSON
     
data MaskAssignConfig = MaskAssignConfig { 
  macMaskTypeId :: Maybe MaskTypeId,
  macKeys       :: [Int],
  macBuiltIn    :: BuiltInId,
  macDefault    :: Bool,
  macName       :: Maybe Text
  }
   deriving (Show,Eq,Generic)


instance ToJSON MaskAssignConfig where 
instance FromJSON MaskAssignConfig where 

maskAssignConfigToMaskTypeJoin :: MaskAssignConfig -> IO (Either Text MaskTypeJoin) 
maskAssignConfigToMaskTypeJoin (MaskAssignConfig _ [] _ _ _) = do
  let err :: Text  
      err = "no keys present"
  return.Left $ err  
maskAssignConfigToMaskTypeJoin (MaskAssignConfig mmtid keys@(primaryKey:_) bid dflt mname) = do
  _ <- resetMTJDefault primaryKey dflt
  case mmtid of
    Nothing -> do
      ey <- (makeBuiltInDataStore keys bid)
      return $ ey >>= (\y -> return $ MaskTypeJoin y mmtid primaryKey dflt mname)
    (Just mtid) -> do 
      ex <- (makeUserDefDataStore keys mtid) 
      return $ ex >>= (\x -> return $ MaskTypeJoin x mmtid primaryKey dflt mname)

makeBuiltInDataStore :: [Int] -> BuiltInId -> IO (Either Text MaskDataStore)
makeBuiltInDataStore keys binId = do
  let bium = insertPidKeys emptyUserMask keys
  return (return $ mkMaskDataStore binId bium)
  
makeUserDefDataStore :: [Int] -> MaskTypeId -> IO (Either Text MaskDataStore)
makeUserDefDataStore keys mtid = do  
  mmsktype <- runDB $ get mtid
  case mmsktype of 
    (Just msktype) -> do
                  let mskdDataStore = maskTypeValue msktype
                      edecodedMDS = maskDataDecode mskdDataStore
                      edecodeMaskData = userMask <$> edecodedMDS
                      edecodeBuiltInId = getBuiltInId <$> edecodedMDS
                      einsertedKeys = (flip insertPidKeys keys) <$> edecodeMaskData
                  return.eStringToEText $ mkMaskDataStore <$> edecodeBuiltInId <*> einsertedKeys
    Nothing -> return $ Left err
        where 
          err :: Text 
          err = "makeUserDefDataStore failed to encode a MaskDataStore"


  
resetMTJDefault :: Int -> Bool -> IO ()
resetMTJDefault primaryKey dflt = do
  case dflt of
    True -> runDB $ updateWhere [(MaskTypeJoinPKey ==. primaryKey),(MaskTypeJoinDefaultSelect ==. True)] [MaskTypeJoinDefaultSelect =. False]
    False -> return ()

getMaskFunctionDefault :: Int -> IO (Either Text (Const -> Either String Const)) 
getMaskFunctionDefault pid = do
  mtje <- runDB $ selectFirst [(MaskTypeJoinPKey ==. pid),(MaskTypeJoinDefaultSelect ==. True)] []
  case mtje of
    Nothing -> do
      let err :: Text
          err = "No defaultMaskTypeJoin found"
      return $ Left err
    (Just mtj) -> do 
      let mds = maskTypeJoinValue.entityVal $ mtj
          edecodeMDS = maskDataDecode mds
          decodedMDS = (eStringToEText edecodeMDS) :: Either Text MaskData     
      edecodeUM <- T.traverse maskLookup decodedMDS 
      return $ maskPullOut <$>  edecodeUM



onpingTagCombinedDefaultTransform :: OnpingTagCombined -> IO OnpingTagCombined
onpingTagCombinedDefaultTransform otc = do      
  let (Just pid) = onpingTagCombinedPid otc
      result = onpingTagCombinedResult otc
  fcn <- returnDefaultMaskFunction pid
  let constresult = ((textToConst >=> fcn) <$> result) >>= etom
      etom (Left _) = Nothing  
      etom (Right x) = Just x
  let newresult = constToText <$> constresult
  return $ (otc {onpingTagCombinedResult = newresult})
    
returnDefaultMaskFunction :: Int -> IO (Const -> Either String Const)   
returnDefaultMaskFunction pid = do
  etcesc <- getMaskFunctionDefault pid
  case etcesc of
    Left _ -> return (\ x -> Right $ x)  
    Right cescFcn -> return cescFcn  
    
textToConst :: Text -> Either String Const
textToConst txt = do
  let doubledtxt = double txt
  case doubledtxt of
    Left e -> Left e
    Right dbl -> return $ ConstDouble (fst dbl)  
    
constToText :: Const -> Text
constToText cr = let esot = constToEitherDouble cr
                 in case esot of
                   Left e -> pack.show $ e   
                   Right txt -> pack.show $ txt

constToEitherDouble :: Const -> Either String Double
constToEitherDouble (ConstDouble x) = Right x
constToEitherDouble (_) = Left "Expected a Double, but got something else"

-- | Takes a list of PIDs and a historical list
onpingTagHWrapper :: (Const -> Either String Const) -> OnpingTagHistory -> OnpingTagHistory
onpingTagHWrapper _ oth@(OnpingTagHistory _ _  Nothing ) = oth
onpingTagHWrapper cFcn oth@(OnpingTagHistory _ _  (Just v)) = oth {onpingTagHistoryVal = (etom $ (doubleToConst v) >>= cFcn >>= constToEitherDouble)}
  where
     etom (Left _) = Nothing  
     etom (Right x) = Just x

onpingTagHistoryDefaultTransform :: [Int] -> [OnpingTagHistory] -> IO [OnpingTagHistory]
onpingTagHistoryDefaultTransform a b
  | (L.null a) || (L.null b) = return []
onpingTagHistoryDefaultTransform pids oth = do
  let uniquePids = S.toList.S.fromList $  pids  
  uniqueMaskFcns <- extracted   uniquePids
  let pairPandF = zip uniquePids uniqueMaskFcns
  return [ f o | o@(OnpingTagHistory ph _ vh) <- oth , (p , f) <- pairPandF , ph == (Just p), vh /= Nothing]

extracted :: Traversable f =>  f Int -> IO (f (OnpingTagHistory -> OnpingTagHistory))
extracted uniquePids = do
   uniqueCFcns <- T.traverse returnDefaultMaskFunction uniquePids
   
   return $ onpingTagHWrapper <$> uniqueCFcns


             

doubleToConst :: Double -> Either String Const
doubleToConst dbl = do
  return $ ConstDouble dbl





-- |utility function

eStringToEText :: Either String a -> Either Text a
eStringToEText (Left x ) = Left (pack x) 
eStringToEText (Right x) = Right x
         
