import argparse
import os
import subprocess
import json
from collections import defaultdict
from dotenv import load_dotenv

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

def get_address(address_file):
  """
  get address from file
  """
  addr_file = open(address_file,'r')
  address = addr_file.readlines()
  return address[0]

def create_policy(token_name, tokens_path):
  """
  create policy for token
  """
  policy_script=tokens_path+token_name+'/policy.script'
  policy_vkey=tokens_path+token_name+'/policy.vkey'
  policy_skey=tokens_path+token_name+'/policy.skey'
  policy = {}
  policy['policy_script'] = policy_script
  policy['policy_vkey'] = policy_vkey
  policy['policy_skey'] = policy_skey

  # check if token exists
  # if so, returns existing policy
  if(os.path.exists(tokens_path+token_name)) :
    print("Token exists : no policy created for token", token_name)
    policy_id = subprocess.run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True)
    policy['policy_id'] = policy_id.stdout.decode().replace('\n', '')
    return policy

  os.makedirs(tokens_path+token_name, mode=0o777, exist_ok=True)

  rc = subprocess.run(['cardano-cli', 'address', 'key-gen', '--verification-key-file', policy_vkey, '--signing-key-file', policy_skey], capture_output=False)

  # create policy script
  keyhash = subprocess.run(['cardano-cli', 'address', 'key-hash', '--payment-verification-key-file', policy_vkey], capture_output=True, text=True)
  data = {}
  data['keyHash'] = keyhash.stdout.replace('\n', '')
  data['type'] = 'sig'
  with open(policy_script, 'w') as outfile:
    json.dump(data, outfile)

  # get policy id
  policy_id = subprocess.run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True, text=True)

  policy['policy_id'] = policy_id.stdout.replace('\n', '')

  return policy

def get_protocol_parameters(network, protparams_file):
  """
  get protocol parameters
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  network_era = network['network_era']
  env_param = network['env']
  subprocess.run(['cardano-cli', 'query', 'protocol-parameters', network_name, network_magic, network_era, '--out-file', protparams_file], \
    capture_output=False, text=True, env=env_param)
  return 

def get_utxo_from_wallet(network, address):
  """
  get utxo from wallet
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  network_era = network['network_era']

  env_param = network['env']
  utxo = {}
  tx_disp = subprocess.run(['cardano-cli', 'query', 'utxo', network_name, network_magic, network_era, '--address', address], \
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

def build_mint_transaction(network, address, token, amount, policy_id, utxo, fee, out_file):
  """
  build mint transaction for token
  """
  network_era = network['network_era']
  balances = utxo['balances']
  balances['lovelace'] = balances['lovelace']-fee
  tx_out=address
  for key, value in balances.items():
    tx_out=tx_out+'+'+str(value)+' '+key
  tx_out = tx_out+' +'+str(amount)+' '+policy_id+'.'+token
  mint = str(amount)+' '+policy_id+'.'+token
  subprocess.run(['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee), utxo['in_utxo'].split(' ')[0], \
    utxo['in_utxo'].split(' ')[1], '--tx-out', tx_out, '--mint', mint, '--out-file', out_file], \
    capture_output=False, text=True)
  return

def calculate_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for on chain transaction
  """
  draft_file = '/tmp/'+token+'.txbody-draft'
  build_mint_transaction(network, address, token, amount, policy_id, utxo, 0, draft_file)

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

def mint(network, address, skey_file, token, amount):
  """
  mint amount of token for address on given network
  """
  source_address = address
  destination_address = address
  protocol_parameters_file = '/tmp/protparams.json'

  # 1. Create a policy for our token
  policy = create_policy(token, network['tokens_path'])

  # 2. Extract protocol parameters (needed for fee calculations)
  get_protocol_parameters(network, protocol_parameters_file)

  # 3. Get UTXOs from our wallet
  utxo = get_utxo_from_wallet(network, address)

  # 4. Calculate tokens balance
  utxo['balances'] = calculate_tokens_balance(utxo['tokens'])

  # 5. Calculate fees for the transaction
  min_fee = calculate_fees(network, destination_address, token, amount, policy['policy_id'], utxo, protocol_parameters_file)

  # 6. Build actual transaction including correct fees
  ok_fee_file = '/tmp/'+token+'.txbody-ok-fee'
  build_mint_transaction(network, destination_address, token, amount, policy['policy_id'], utxo, min_fee, ok_fee_file)

  # 7. Sign the transaction
  sign_file = '/tmp/'+token+'.tx.sign'
  sign_transaction(network, skey_file, policy, ok_fee_file, sign_file)

  # 8. Submit the transaction to the blockchain
  submit_transaction(network, sign_file)
  return

def main():
  """
  read parameters from command line
  and mint token
  """
  # parse command line parameters
  example_text = '''example:

  python3 %(prog)s --address paymentAlice.addr --skey paymentAlice.skey --token TOK --amount 10000
  '''
  parser = argparse.ArgumentParser(description='Mint amount Token for address with signing key.', epilog=example_text)
  parser.add_argument('-a', '--address', nargs='?', help='address file', required=True)
  parser.add_argument('-s', '--skey', nargs='?', help='signing key file', required=True)
  parser.add_argument('-t', '--token', nargs='?', help='token name', required=True)
  parser.add_argument('--amount', type=int, help='token amount', required=True)
  args = parser.parse_args()

  # load env vars
  load_dotenv()
  env_param = os.environ
  env_param['CARDANO_NODE_SOCKET_PATH'] = os.getenv('CARDANO_NODE_SOCKET_PATH')
  network = {}
  network['env'] = env_param
  network['network'] = '--'+os.getenv('NETWORK')
  network['network_magic'] = int(os.getenv('NETWORK_MAGIC'))
  network['network_era'] = '--'+os.getenv('NETWORK_ERA')
  network['tokens_path'] = os.getenv('TOKENS_PATH')
  addresses_path = os.getenv('ADDRESSES_PATH')
  
  # set parameters
  address = get_address(addresses_path+args.address)
  skey_file= addresses_path+args.skey
  token = args.token
  amount = args.amount

  # mint token
  mint(network, address, skey_file, token, amount)

if __name__ == '__main__':
  main()

