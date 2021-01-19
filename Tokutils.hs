{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Tokutils where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.IO ( hGetContents)
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (ToJSON, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

data PolicyScript = PolicyScript
  { 
    keyHash :: String
  , keyType    :: String
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = \f -> if f == "keyType" then "type" else f} ''PolicyScript)

data Policy = Policy
  { 
    policyScript :: FilePath
  , policyVkey   :: FilePath
  , policySkey   :: FilePath
  , policyId     :: String
  }
  deriving Show

buildPolicyPath :: String -> String -> Policy
buildPolicyPath policyName policyPath = Policy 
  (policyPath ++ "policy.script")
  (policyPath ++ "policy.vkey")
  (policyPath ++ "policy.skey")
  ""

createPolicy :: String -> String -> IO (Maybe Policy)
createPolicy policyName policyPath = do
  let policy = buildPolicyPath policyName policyPath
  -- check if policy script exists
  -- if so returns existing Policy
  bool <- doesFileExist (policyScript policy)
  if not bool then do
    createDirectoryIfMissing True policyPath
    -- create policy key files
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" ["address", "key-gen", "--verification-key-file", policyVkey policy, "--signing-key-file", policySkey policy]){ std_out = CreatePipe }
    r <- waitForProcess ph
    -- create hash
    (_, Just hout, _, _) <- createProcess (proc "cardano-cli" ["address", "key-hash", "--payment-verification-key-file", policyVkey policy]){ std_out = CreatePipe }
    keyh <- hGetContents hout
    let keyhash = filter (/= '\n') keyh
    -- create policy script
    let myPolicyScript = PolicyScript { keyHash = keyhash, keyType = "sig"}
    B.writeFile (policyScript policy) (encode myPolicyScript)
  else do
    putStrLn $ "Policy exists : no policy created for " ++ policyName
  -- retrieve policy script
  (_, Just hout, _, _) <- createProcess (proc "cardano-cli" ["transaction", "policyid", "--script-file", policyScript policy]){ std_out = CreatePipe }
  pId <- hGetContents hout 
  return (Just policy { policyId = filter (/= '\n') pId} )

getPolicyPath:: FilePath -> String -> String -> FilePath -> FilePath
getPolicyPath addressPath ownerName policyName policiesFolder = (getAddressPath addressPath ownerName) ++ policiesFolder ++ policyName ++ "/"

getAddressPath:: FilePath -> String -> FilePath
getAddressPath addressPath ownerName = addressPath ++ ownerName ++ "/"

getPolicyId:: Policy -> String
getPolicyId = policyId

