module Transaction ( createAddress ) where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.IO ( hGetContents)
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )

import Tokutils ( Address, AddressType(Payment, Stake), BlockchainNetwork(BlockchainNetwork, network, networkMagic), getAddress, getAddressFile, getVkeyFile, getProtocolKeyDeposit )

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
      -- print runParams
      (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      getAddress addrFile
{-
def create_address(network, address_type, addresses_path, address_prefix, name):
  """
  create address based on name
  """
  vkey_file = get_vkey_file(addresses_path, address_prefix, name)
  if not path.exists(vkey_file) :
    print(address_prefix, "verification key missing for", name)
    return None
  addr_file = get_address_file(addresses_path, address_prefix, name)
  if path.exists(addr_file) :
    print(address_prefix, "address already exists for", name)
    return get_address(addr_file)
  network_name = network['network']
  network_magic = str(network['network_magic'])

  run_params = ['cardano-cli', address_type, 'build', network_name, network_magic, \
    '--'+address_prefix+'-verification-key-file', vkey_file, '--out-file', addr_file]
  subprocess_run(run_params, capture_output=False, text=True)
 
  return get_address(addr_file)
-}