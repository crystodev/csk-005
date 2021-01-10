"""
This modules provides some Cardano blockchain transaction tools
"""
from os import path
from json import loads as json_loads
from subprocess import run as subprocess_run
from tokutils import get_address, get_protocol_keydeposit

def split_list(tx):
  """
  split list in two with '+' as a separator
  """
  idx_list = [idx for idx, val in enumerate(tx) if val == '+']
  idx_list1 = [idx+1 for idx, val in enumerate(tx) if val == '+']
  size = len(tx)
  if(size == 2):
    # lovelace only
    return [tx]
  
  # lovelace + tokens
  tx = [tx[i: j] for i, j in
      zip([0] + idx_list1, idx_list + 
      ([size] if idx_list[-1] != size else []))]
  return tx

def build_burn_transaction(network, address, token, amount, policy_id, utxo, fee, out_file):
  """
  build burn transaction for token
  """
  return build_mint_transaction(network, address, token, -amount, policy_id, utxo, fee, out_file)

def build_mint_transaction(network, address, token, amount, policy_id, utxo, fee, out_file):
  """
  build mint transaction for token
  """
  network_era = network['network_era']
  ada_id = 'lovelace'
  asset_id = policy_id+'.'+token
  balances = utxo['balances']
  balances[ada_id] = balances[ada_id]-fee
  tx_out=address
  for key, value in balances.items():
    tx_out=tx_out+'+'+str(value)+' '+key
  tx_out = tx_out+' +'+str(amount)+' '+asset_id
  mint = str(amount)+' '+asset_id
  ttl = calculate_ttl(network)
  run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + \
    ['--ttl', str(ttl), '--tx-out', tx_out, '--mint', mint, '--out-file', out_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return

def build_send_transaction(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, fee, out_file):
  """
  build transfer transaction for token
  """
  key_deposit = get_protocol_keydeposit(network)
  lovelace_amount = ada_amount * 1000000
  if lovelace_amount == 0:
    lovelace_amount = key_deposit
  elif lovelace_amount < key_deposit:
    if not fee == 0:
      print('To few lovelace for transaction', ada_amount, 'ADA ; you need at least', key_deposit, 'lovelace for transaction')
    return False
  network_era = network['network_era']
  ada_id = 'lovelace'
  balances = utxo['balances'].copy()
  balances[ada_id] = balances[ada_id]-fee-lovelace_amount
  if token is not None:
    asset_id = policy_id+'.'+token
    balances[asset_id] = balances[asset_id]-token_amount
  tx_out_src=source_address
  for key, value in balances.items():
    tx_out_src=tx_out_src+' +'+str(value)+' '+key
  tx_out_dst = destination_address+'+'+str(lovelace_amount)+' lovelace+'+str(token_amount)
  if token is not None:
    tx_out_dst = tx_out_dst +' '+asset_id
  
  ttl = calculate_ttl(network)
  run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + \
    ['--ttl', str(ttl), '--tx-out', tx_out_dst, '--tx-out', tx_out_src, '--out-file', out_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return True

def calculate_burn_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for burn transaction
  """
  return calculate_mint_fees(network, address, token, amount, policy_id, utxo, protparams_file)

def calculate_mint_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for mint transaction
  """
  draft_file = get_transaction_file(token, 'draft')
  build_mint_transaction(network, address, token, amount, policy_id, utxo, 0, draft_file)

  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_send_fees(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, protparams_file):
  """
  calculate fee for transfer transaction
  """
  draft_file = get_transaction_file(token, 'draft')

  build_send_transaction(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, 0, draft_file)
  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_ttl(network):
  FORWARD_SLOT=300
  run_params = ['cardano-cli', 'query', 'tip', network['network'], str(network['network_magic'])]
  print(run_params)
  tip = subprocess_run(run_params, capture_output=True, text=True)
  slot = int(json_loads(tip.stdout).get('slotNo'))+FORWARD_SLOT
  return slot

def get_address_key_file(addresses_path, address_type, address_or_key, name):
  """
  give file name for name type address or key
  """
  if not address_type in ['payment', 'stake']:
    print('Unknown address type :', address_type)
    return None
  if address_or_key == 'address':
    ext = '.addr'
  elif address_or_key == 'signing_key':
    ext = '.skey'
  elif address_or_key == 'verification_key':
    ext = '.vkey'
  else:
    print('Unknown type :', address_or_key)
    return None

  addr_key_file = addresses_path+address_type+name+ext
  return addr_key_file

def get_address_file(addresses_path, address_type, name):
  """
  give file name for name type address
  """
  return get_address_key_file(addresses_path, address_type, 'address', name)

def get_skey_file(addresses_path, address_type, name):
  """
  give file name for name type address
  """
  return get_address_key_file(addresses_path, address_type, 'signing_key', name)

def get_vkey_file(addresses_path, address_type, name):
  """
  give file name for name type address
  """
  return get_address_key_file(addresses_path, address_type, 'verification_key', name)

def create_address(network, address_type, addresses_path, address_prefix, name):
  """
  create address based on name
  """
  vkey_file = addresses_path+address_prefix+name+'.vkey'

  if not path.exists(vkey_file) :
    print(address_prefix, "verification key missing for", name)
    return None
  addr_file = addresses_path+address_prefix+name+'.addr'
  if path.exists(addr_file) :
    print(address_prefix, "address already exists for", name)
    return get_address(addr_file)
  network_name = network['network']
  network_magic = str(network['network_magic'])

  run_params = ['cardano-cli', address_type, 'build', network_name, network_magic, \
    '--'+address_prefix+'-verification-key-file', vkey_file, '--out-file', addr_file]
  subprocess_run(run_params, capture_output=False, text=True)
 
  return get_address(addr_file)

def get_transaction_file(token, file_type):
  if file_type == 'draft':
    ext = 'txbody.draft'
  elif file_type == 'ok-fee':
    ext = '.txbody-ok-fee'
  elif file_type == 'sign':
    ext = '.tx.sign'
  else:
    ext = ''

  if token is None:
    file_name = '/tmp/lovelace'+ext
  else:
    file_name = '/tmp/'+token+ext
  return file_name
  
def get_utxo_from_wallet(network, address):
  """
  get utxo from wallet
  """
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

def sign_mint_transaction(network, skey, policy, ok_fee_file, sign_file):
  """
  sign mint transaction
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  rc = subprocess_run(['cardano-cli', 'transaction', 'sign', network_name, network_magic, '--signing-key-file', skey, '--signing-key-file', policy['policy_skey'], \
    '--script-file', policy['policy_script'], '--tx-body-file', ok_fee_file, '--out-file', sign_file], \
    capture_output=False, text=True)
  return

def sign_burn_transaction(network, skey, policy, ok_fee_file, sign_file):
  """
  sign burn transaction
  """
  return sign_mint_transaction(network, skey, policy, ok_fee_file, sign_file)

def sign_send_transaction(network, skey, ok_fee_file, sign_file):
  """
  sign transfer transaction
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  rc = subprocess_run(['cardano-cli', 'transaction', 'sign', network_name, network_magic, '--signing-key-file', skey, \
    '--tx-body-file', ok_fee_file, '--out-file', sign_file], \
    capture_output=False, text=True)
  return

def submit_transaction(network, sign_file):
  """
  submit signed transaction on the network
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  env_param = network['env']
  rc = subprocess_run(['cardano-cli', 'transaction', 'submit', network_name, network_magic, '--tx-file', sign_file], \
    capture_output=False, text=True, env=env_param)
  return

