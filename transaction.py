"""
This modules provides some Cardano blockchain transaction tools
"""
from subprocess import run as subprocess_run

def split_list(tx):
  """
  split list in two with '+' as a separator
  """
  idx_list = [idx for idx, val in enumerate(tx) if val == '+']
  idx_list1 = [idx+1 for idx, val in enumerate(tx) if val == '+']
  size = len(tx)
  tx = [tx[i: j] for i, j in
      zip([0] + idx_list1, idx_list + 
      ([size] if idx_list[-1] != size else []))]
  return tx

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
  run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + ['--tx-out', tx_out, '--mint', mint, '--out-file', out_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return

def build_burn_transaction(network, address, token, amount, policy_id, utxo, fee, out_file):
  """
  build burn transaction for token
  """
  return build_mint_transaction(network, address, token, -amount, policy_id, utxo, fee, out_file)

def build_send_transaction(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, fee, out_file):
  """
  build transfer transaction for token
  """
  network_era = network['network_era']
  ada_id = 'lovelace'
  asset_id = policy_id+'.'+token
  balances = utxo['balances'].copy()
  balances[ada_id] = balances[ada_id]-fee-2000000
  balances[asset_id] = balances[asset_id]-token_amount
  tx_out_src=source_address
  for key, value in balances.items():
    tx_out_src=tx_out_src+' +'+str(value)+' '+key
  tx_out_dst = destination_address+'+2000000 lovelace+'+str(token_amount)+' '+asset_id
  
  run_params = ['cardano-cli', 'transaction', 'build-raw', network_era, '--fee', str(fee)] + utxo['in_utxo'] + ['--tx-out', tx_out_dst, '--tx-out', tx_out_src, '--out-file', out_file]
  subprocess_run(run_params, capture_output=False, text=True)

  return

def calculate_mint_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for on chain mint transaction
  """
  draft_file = '/tmp/'+token+'.txbody-draft'
  build_mint_transaction(network, address, token, amount, policy_id, utxo, 0, draft_file)

  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

def calculate_burn_fees(network, address, token, amount, policy_id, utxo, protparams_file):
  """
  calculate fee for on chain burn transaction
  """
  return calculate_mint_fees(network, address, token, amount, policy_id, utxo, protparams_file)

def calculate_send_fees(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, protparams_file):
  """
  calculate fee for on chain transfer transaction
  """
  draft_file = '/tmp/'+token+'.txbody-draft'
  build_send_transaction(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, 0, draft_file)
  rc = subprocess_run(['cardano-cli', 'transaction', 'calculate-min-fee', '--tx-body-file', draft_file, '--tx-in-count', str(utxo['count_utxo']), \
    '--tx-out-count', '1', '--witness-count', '1', '--byron-witness-count', '0', '--protocol-params-file', protparams_file], \
    capture_output=True, text=True)
  min_fee = int(rc.stdout.split(' ')[0])
  return min_fee

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

