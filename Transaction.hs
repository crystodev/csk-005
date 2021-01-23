module Transaction ( createAddress, getUtxoFromWallet, Utxo(Utxo, raw, utxos, nbUtxos, tokens) ) where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.IO ( hGetContents)
import System.Process ( createProcess, env, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Maybe ( )
import Data.List ( delete, intercalate )
import Data.List.Split ( splitOn )

import Tokutils ( Address, AddressType(Payment, Stake), BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv),
  getAddress, getAddressFile, getVkeyFile, getProtocolKeyDeposit )

data Utxo = Utxo {
    raw :: String
  , utxos :: [String]
  , nbUtxos :: Int
  , tokens :: [(String, Int)]
}

createAddress :: BlockchainNetwork -> AddressType -> FilePath -> String -> IO (Maybe Address) 
createAddress bNetwork addressType addressesPath ownerName = do
  let vkFile = getVkeyFile addressesPath addressType ownerName
  boolvk <- doesFileExist vkFile
  if not boolvk then do
    putStrLn $ "verification key missing for " ++ ownerName
    return Nothing
  else do
    let addrFile = getAddressFile addressesPath addressType ownerName
    boolad <- doesFileExist addrFile
    if boolad then do
      putStrLn $ "address already exists for " ++ ownerName
      getAddress addrFile
    else do
      let netName = network bNetwork
      let netMagic = networkMagic bNetwork
{-      
      let netEra = networkEra bNetwork
      let envParam = networkEnv bNetwork
-}
      let saddressType = if addressType == Payment then "address" else "stake-address"
      let saddressPrefix = if addressType == Payment then "payment" else "stake"
      let runParams = [saddressType, "build", netName, show netMagic, "--" ++ saddressPrefix ++ "-verification-key-file", vkFile, "--out-file", addrFile]
      (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      getAddress addrFile

-- get utxo from wallet
getUtxoFromWallet :: BlockchainNetwork -> Address -> IO Utxo
getUtxoFromWallet bNetwork address = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = Just [("CARDANO_NODE_SOCKET_PATH", networkEnv bNetwork)]
  let runParams = ["query", "utxo", netName, show netMagic, netEra, "--address", address]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams ) { env = envParam } {std_out = CreatePipe }
  r <- waitForProcess ph
  raw <- hGetContents hout

  -- split lines and remove first to lines
  let txList = drop 2  . lines $ raw
  -- get transaction id and index from start of lists and join with #
  let rawUtxos = [ take 2 (words tx) | tx <- txList]
  let utxos = fmap (intercalate "#" ) rawUtxos
-- get tokens from end of lists and build tuples (token, amount)
  let rawTokens = [ drop 2 (words tx) | tx <- txList]
  let ltokens = fmap (filter (/= "+")) rawTokens
  let tokens = concatMap parseTokens ltokens
  return Utxo {raw=raw, utxos=utxos, nbUtxos= length utxos, tokens=tokens}

-- parse transactions list
parseTokens :: [String] -> [(String, Int)]
parseTokens [] = []
parseTokens [x] = []
parseTokens (x:y:xs) = (y,read x::Int):parseTokens xs
 
  
{-
def get_utxo_from_wallet(network, address):
  if address is None:
    print("address empty")
    return
  network_name = network['network']
  network_magic = str(network['network_magic'])
  network_era = network['network_era']

  env_param = network['env']
  utxo = {}
  tx_disp = subprocess_run(['cardano-cli', 'query', 'utxo', network_name, network_magic, network_era, '--address', address], \
    capture_output=True, text=True, env=env_param)
  tx_list = tx_disp.stdout.split('\n')
  utxo['raw'] = tx_list
  tx_list = [tx.split() for tx in tx_list[2:] if tx != ""]
  t_list = [["--tx-in",tx[0]+'#'+tx[1]] for tx in tx_list]
  # flatten list
  utxo['in_utxo'] = [y for x in t_list for y in x]
  utxo['count_utxo'] = len(tx_list)
  tx_list = [tx[2:] for tx in tx_list]

  tx_list = [split_list(tx) for tx in tx_list]
  # flatten list
  t_list = [y for x in tx_list for y in x]
  utxo['tokens'] = [ (token[1],int(token[0])) for token in t_list]
  return utxo
-}
