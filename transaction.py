"""
This module provides some Cardano blockchain transaction tools
"""
from os import path
from json import loads as json_loads
from subprocess import run as subprocess_run
from tokutils import get_address, get_address_file, get_vkey_file, get_protocol_keydeposit

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

def build_burn_transaction(network, address, token_name, token_amount, policy_id, utxo, fee, out_file):
  """
  build burn transaction for token
  """
  token_id = policy_id+'.'+token_name
  if token_id not in utxo['balances'].keys():
    print("Cannot burn token", token_name, "with policy", policy_id, ": no token")
    return False
  if utxo['balances'].get(token_id) < token_amount: 
    print("Cannot burn token", token_name, ": not enough token")
    return False
  if policy_id == "":
    print("No policy_id")
    return False
  if utxo['count_utxo'] == 0:
    print("No utxo found")
    return False
  network_era = network['network_era']
  ada_id = 'lovelace'
  asset_id = policy_id+'.'+token_name
  mint = str(-token_amount)+' '+asset_id
  balances = utxo['balances'].copy()
  balances[ada_id] = balances[ada_id]-fee
  balances[asset_id] = balances[asset_id]-token_amount
  if balances[asset_id] < 0 :
    print(address, "does not have", token_amount, token)
    return False
  tx_out = address
  for key, value in balances.items():
    tx_out=tx_out+'+'+str(value)+' '+key

  ttl = calculate_ttl(network)
  run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + \
    ['--ttl', str(ttl), '--tx-out', tx_out, '--mint', mint, '--out-file', out_file]
  print(run_params)
  rc =subprocess_run(run_params, capture_output=False, text=True)
  return rc.returncode == 0

def build_mint_transaction(network, source_address, destination_address, token_name, token_amount, policy_id, utxo, fee, out_file):
  """
  build mint transaction for token
  """
  if token_name is None or token_amount == 0:
    return False
  return build_send_transaction(network, source_address, destination_address, 0, token_name, token_amount, True, policy_id, utxo, fee, out_file)

def build_send_transaction(network, source_address, destination_address, ada_amount, token, token_amount, do_mint, policy_id, utxo, fee, out_file):
  """
  build transfer transaction for token
  """
  if ada_amount < 0 or token_amount < 0:
    print("Can not send negative amount")
    return False
  if ada_amount == 0 and token_amount == 0:
    print("Nothing to send")
    return False
  if policy_id == "" or policy_id == None:
    print("No policy_id")
    return False
  if utxo['count_utxo'] == 0:
    print("No utxo found")
    return False
  if destination_address is None:
    print("No valid destination address")
    return False
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
  if balances[ada_id] < 0 :
    print(source_address, "does not have", fee+lovelace_amount, "lovelaces (", fee+lovelace_amount/1000000, "ada )")
    return False
  if token is not None:
    asset_id = policy_id+'.'+token
    if not do_mint :
      balances[asset_id] = balances[asset_id]-token_amount
      if balances[asset_id] < 0 :
        print(source_address, "does not have", token_amount, token)
        return False
  tx_out_src=source_address
  for key, value in balances.items():
    tx_out_src=tx_out_src+' +'+str(value)+' '+key
  tx_out_dst = destination_address+'+'+str(lovelace_amount)+' lovelace'
  if token is not None:
    tx_out_dst = tx_out_dst +'+'+str(token_amount)+' '+asset_id
  
  ttl = calculate_ttl(network)
  if do_mint:
    run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + \
     ['--ttl', str(ttl), '--tx-out', tx_out_dst, '--tx-out', tx_out_src, '--mint', str(token_amount)+' '+asset_id, '--out-file', out_file]
  else:
    run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + \
     ['--ttl', str(ttl), '--tx-out', tx_out_dst, '--tx-out', tx_out_src, '--out-file', out_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return True

def calculate_burn_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for burn transaction
  """
  draft_file = get_transaction_file(token, 'draft')
  rc = build_burn_transaction(network, address, token, amount, policy_id, utxo, 0, draft_file)
  if not rc:
    print("Failed to build transaction")
    return None

  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_mint_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for mint transaction
  """
  draft_file = get_transaction_file(token, 'draft')
  rc = build_mint_transaction(network, address, address, token, amount, policy_id, utxo, 0, draft_file)
  if not rc:
    print("Failed to build transaction")
    return None

  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_send_fees(network, source_address, destination_address, ada_amount, token, token_amount, policy_id, utxo, protparams_file):
  """
  calculate fee for transfer transaction
  """
  draft_file = get_transaction_file(token, 'draft')

  rc = build_send_transaction(network, source_address, destination_address, ada_amount, token, token_amount, False, policy_id, utxo, 0, draft_file)
  if not rc: 
    print("Failed to build transaction")
    return None
  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_ttl(network):
  FORWARD_SLOT=300
  run_params = ['cardano-cli', 'query', 'tip', network['network'], str(network['network_magic'])]
  tip = subprocess_run(run_params, capture_output=True, text=True)
  slot = int(json_loads(tip.stdout).get('slotNo'))+FORWARD_SLOT
  return slot

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

def get_transaction_file(token, file_type):
  if file_type == 'draft':
    ext = '.txbody.draft'
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
  return rc

