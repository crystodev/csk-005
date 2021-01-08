import argparse
import os
import subprocess
import json
from collections import defaultdict

def split_list(tx):
  """
  split list
  """
  idx_list = [idx for idx, val in enumerate(tx) if val == '+']
  idx_list1 = [idx+1 for idx, val in enumerate(tx) if val == '+']
  size = len(tx)
  tx = [tx[i: j] for i, j in
      zip([0] + idx_list1, idx_list + 
      ([size] if idx_list[-1] != size else []))]
  return tx

def getAddress(address_file):
  """
  get address from file
  """
  addr_file = open(address_file,'r')
  address = addr_file.readlines()
  return address[0]

def createPolicy(token_name):
  """
  create policy for token
  """
  os.makedirs(token_name, mode=0o777, exist_ok=True)
  policy_script=token_name+'/policy.script'
  policy_vkey=token_name+'/policy.vkey'
  policy_skey=token_name+'/policy.skey'
  rc = subprocess.run(['cardano-cli', 'address', 'key-gen', '--verification-key-file', policy_vkey, '--signing-key-file', policy_skey], capture_output=False)

  "create policy script"
  keyhash = subprocess.run(['cardano-cli', 'address', 'key-hash', '--payment-verification-key-file', policy_vkey], capture_output=True, text=True)
  data = {}
  data['keyHash'] = keyhash.stdout.replace('\n', '')
  data['type'] = 'sig'
  with open(policy_script, 'w') as outfile:
    json.dump(data, outfile)

  "get policy id"
  policy_id = subprocess.run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True, text=True)

  policy = {}
  policy['policy_id'] = policy_id.stdout.replace('\n', '')
  policy['policy_script'] = policy_script
  policy['policy_vkey'] = policy_vkey
  policy['policy_skey'] = policy_skey
  return policy

def get_protocol_parameters(network, protparams_file):
  """
  get protocol parameters
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  env_param = network['env']
  subprocess.run(['cardano-cli', 'query', 'protocol-parameters', network_name, network_magic, '--mary-era', '--out-file', protparams_file], \
    capture_output=False, text=True, env=env_param)
  return 

def get_utxo_from_wallet(network, address):
  """
  get utxo from wallet
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  env_param = network['env']
  utxo = {}
  tx_disp = subprocess.run(['cardano-cli', 'query', 'utxo', network_name, network_magic, '--mary-era', '--address', address], \
    capture_output=True, text=True, env=env_param)
  tx_list = tx_disp.stdout.split('\n')
  tx_list = [tx.split() for tx in tx_list[2:] if tx != ""]
  utxo['in_utxo'] = " ".join(["--tx-in "+tx[0]+'#'+tx[1] for tx in tx_list])
  utxo['count_utxo'] = len(tx_list)
  tx_list = [tx[2:] for tx in tx_list]

  tx_list = [split_list(tx) for tx in tx_list]
  t_list = [y for x in tx_list for y in x]
  utxo['tokens'] = [ (token[1],int(token[0])) for token in t_list]
  return utxo

def calculate_tokens_balance(tokens):
  """
  compute total balance for each list of tokens (token,amount)
  return a dict with total amount for each token
  """
  result = defaultdict(int)
  for key, value in tokens:
    result[key] += value
  return result

def build_mint_transaction(address, token, amount, policy_id, utxo, fee, out_file):
  """
  build mint transaction for token
  """
  balances = utxo['balances']
  balances['lovelace'] = balances['lovelace']-fee
  tx_out=address
  for key, value in balances.items():
    tx_out=tx_out+'+'+str(value)+' '+key
  tx_out = tx_out+' +'+str(amount)+' '+policy_id+'.'+token
  mint = str(amount)+' '+policy_id+'.'+token
  subprocess.run(['cardano-cli', 'transaction', 'build-raw', '--mary-era', '--fee', str(fee), utxo['in_utxo'].split(' ')[0], \
    utxo['in_utxo'].split(' ')[1], '--tx-out', tx_out, '--mint', mint, '--out-file', out_file], \
    capture_output=False, text=True)
  return

def calculate_fees(address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for on chain transaction
  """
  draft_file = '/tmp/'+token+'.txbody-draft'
  build_mint_transaction(address, token, amount, policy_id, utxo, 0, draft_file)

  rc = subprocess.run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def sign_transaction(network, skey, policy, ok_fee_file, sign_file):
  """
  sign transaction
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  rc = subprocess.run(['cardano-cli', 'transaction', 'sign', network_name, network_magic, '--signing-key-file', skey, '--signing-key-file', policy['policy_skey'], \
    '--script-file', policy['policy_script'], '--tx-body-file', ok_fee_file, '--out-file', sign_file], \
    capture_output=False, text=True)
  return

def submit_transaction(network, sign_file):
  """
  submit signed transaction on the network
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  env_param = network['env']
  rc = subprocess.run(['cardano-cli', 'transaction', 'submit', network_name, network_magic, '--tx-file', sign_file], \
    capture_output=False, text=True, env=env_param)
  return

def mint(network, address, skey, token, amount):
  """
  mint amount of token for address on given network
  """
  source_address = address
  destination_address = address
  protocol_parameters_file = '/tmp/protparams.json'

  "1. Create a policy for our token"
  "TODO : Check if token already exist before creating new policy"
  policy = createPolicy(token)

  "2. Extract protocol parameters (needed for fee calculations)"
  get_protocol_parameters(network, protocol_parameters_file)

  "3. Get UTXOs from our wallet"
  utxo = get_utxo_from_wallet(network, address)

  "4. Calculate tokens balance"
  utxo['balances'] = calculate_tokens_balance(utxo['tokens'])

  "5. Calculate fees for the transaction"
  min_fee = calculate_fees(destination_address, token, amount, policy['policy_id'], utxo, protocol_parameters_file)

  "6. Build actual transaction including correct fees"
  ok_fee_file = '/tmp/'+token+'.txbody-ok-fee'
  build_mint_transaction(destination_address, token, amount, policy['policy_id'], utxo, min_fee, ok_fee_file)

  "7. Sign the transaction"
  sign_file = '/tmp/'+token+'.tx.sign'
  sign_transaction(network, skey, policy, ok_fee_file, sign_file)

  "8. Submit the transaction to the blockchain"
  submit_transaction(network, sign_file)
  return

def main():
  """
  read parameters from command line
  and mint token
  """
  parser = argparse.ArgumentParser(description='Mint amount Token for address with signing key.')
  parser.add_argument('--address', nargs='?', help='address file', required=True)
  parser.add_argument('--skey', nargs='?', help='signing key file', required=True)
  parser.add_argument('--token', nargs='?', help='token name', required=True)
  parser.add_argument('--amount', type=int, help='token amount', required=True)
  args = parser.parse_args()
  address = getAddress(args.address)
  skey = args.skey
  token = args.token
  amount = args.amount
  env_param = os.environ
  env_param['CARDANO_NODE_SOCKET_PATH'] = '../socket'
  network = {}
  network['network'] = '--testnet-magic'
  network['network_magic'] = 3
  network['env'] = env_param
  mint(network, address, skey, token, amount)

if __name__ == '__main__':
  main()

