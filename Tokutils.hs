{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Tokutils ( createKeypair, createPolicy, Address, AddressType(Payment, Stake), BlockchainNetwork (BlockchainNetwork, network, networkMagic, networkEra, networkEnv), getPolicyPath, getPolicyId, getProtocolKeyDeposit, saveProtocolParameters, getAddress, getAddressFile, getSkeyFile, getVkeyFile ) where

import System.Directory ( createDirectoryIfMissing, doesFileExist)
import System.FilePath ( takeDirectory )
import System.IO ( hGetContents, readFile)
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (decode, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

-- policy helpers ---------------------------------------------------------
data PolicyScript = PolicyScript
  { 
    keyHash :: String
  , keyType    :: String
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = \f -> if f == "keyType" then "type" else f} ''PolicyScript)

-- Cardano Policy
data Policy = Policy
  { 
    policyScript :: FilePath
  , policyVkey   :: FilePath
  , policySkey   :: FilePath
  , policyId     :: String
  }
  deriving Show

-- build Policy full file names
buildPolicyPath :: String -> String -> Policy
buildPolicyPath policyName policyPath = Policy 
  (policyPath ++ "policy.script")
  (policyPath ++ "policy.vkey")
  (policyPath ++ "policy.skey")
  ""

-- create a Cardano Policy
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

-- get Policy Folder
getPolicyPath:: FilePath -> String -> String -> FilePath -> FilePath
getPolicyPath addressPath ownerName policyName policiesFolder = getAddressPath addressPath ownerName ++ policiesFolder ++ policyName ++ "/"

-- get Policy Id
getPolicyId:: Policy -> String
getPolicyId = policyId

-- protocols helpers ------------------------------------------------------

data BlockchainNetwork = BlockchainNetwork 
  {
    network :: String
  , networkMagic :: Int
  , networkEra :: String
  , networkEnv :: String
  }
  deriving Show

-- get keyDeposit parameter from protocol
getProtocolKeyDeposit :: BlockchainNetwork -> IO (Maybe Int)
getProtocolKeyDeposit bNetwork = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = networkEnv bNetwork
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" ["query", "protocol-parameters", netName, show netMagic, netEra]){ std_out = CreatePipe }
  r <- waitForProcess ph
  jsonFile <- hGetContents rc
  js <- B.readFile jsonFile
  let jsa = decode js
  print jsa
  -- TODO Retrieve keyDeposit from file
  -- return int(json_loads(rc.stdout)['keyDeposit'])
  return jsa

-- get protocol parameters
saveProtocolParameters :: BlockchainNetwork -> FilePath -> IO Bool
saveProtocolParameters bNetwork protocolParams = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = networkEnv bNetwork
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" ["query", "protocol-parameters", netName, show netMagic, netEra, "--out-file", protocolParams]){ std_out = CreatePipe }
  r <- waitForProcess ph
  return True

-- create keypair based on address_name
createKeypair :: AddressType -> FilePath -> String -> IO Bool
createKeypair addressType addressesPath ownerName = do
  let vkeyFile = getVkeyFile addressesPath addressType ownerName
  let skeyFile = getSkeyFile addressesPath addressType ownerName
  bool <- doesFileExist vkeyFile
  if bool then do
    putStrLn $ "key pair already exists for " ++ ownerName
    return False
  else do
    createDirectoryIfMissing True (takeDirectory vkeyFile)
    let saddressType = if addressType == Payment then "address" else "stake-address"
    
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" [saddressType, "key-gen", "--verification-key-file", vkeyFile, "--signing-key-file", skeyFile]){ std_out = CreatePipe }
    r <- waitForProcess ph
    return True

-- calculate_tokens_balance

-- address helpers ---------------------------------------------------------------------

-- data AdressOrKey = address | signing_key | verification_key deriving (Read, Show, Eq)
type Address = String
data AddressType = Payment | Stake deriving (Read, Show, Eq)

-- TODO GetAddress
getAddress :: FilePath -> IO (Maybe Address)
getAddress addressFileName = do
  bool <- doesFileExist addressFileName
  if not bool then do
    putStrLn $ "file not found : " ++ addressFileName
    return Nothing
  else do 
    addr <- readFile addressFileName
    return (Just addr)

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> String -> FilePath
getAddressPath addressesPath ownerName = addressesPath ++ ownerName ++ "/"

-- give file name for name type address or key
getAddressKeyFile :: FilePath -> AddressType -> String -> String -> FilePath
getAddressKeyFile addressesPath addressType addressKey name = do
  let saddressType = if addressType == Payment then "payment" else "stake"
  let extmap = [ ("address", ".addr"), ("signing_key", ".skey"), ("verification_key", ".vkey")]
  let extm = lookup addressKey extmap
  case extm of
    Just ext -> getAddressPath addressesPath name ++ saddressType ++ ext
    _ -> ""

-- give file name for name type address
getAddressFile :: FilePath -> AddressType -> String -> FilePath
getAddressFile addressesPath addressType = getAddressKeyFile addressesPath addressType "address"

-- give file name for name type signing key
getSkeyFile :: FilePath -> AddressType -> String -> FilePath
getSkeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "signing_key"

-- give file name for name type verification key
getVkeyFile :: FilePath -> AddressType -> String -> FilePath
getVkeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "verification_key"
