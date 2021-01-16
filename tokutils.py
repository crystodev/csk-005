"""
Utility module for Cardano Token manipulation
"""
from os import makedirs, path
from subprocess import run as subprocess_run 
from collections import defaultdict
from json import dump as json_dump, loads as json_loads

def calculate_tokens_balance(tokens):
  """
  compute total balance for each list of tokens (token,amount)
  return a dict with total amount for each token
  """
  result = defaultdict(int)
  for key, value in tokens:
    result[key] += value
  return result

def create_keypair(address_type, addresses_path, address_prefix, name):
  """
  create keypair based on address_name
  """
  vkey_file = get_vkey_file(addresses_path, address_prefix, name)
  skey_file = get_skey_file(addresses_path, address_prefix, name)

  if(path.exists(vkey_file)) :
    print(address_prefix, "key pair already exists for", name)
    return
  
  makedirs(path.dirname(vkey_file), mode=0o777, exist_ok=True)

  run_params = ['cardano-cli', address_type, 'key-gen', '--verification-key-file', vkey_file, '--signing-key-file', skey_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return

def create_policy(policy_name, policy_path):
  """
  create policy
  """
  policy_script=policy_path+'policy.script'
  policy_vkey=policy_path+'policy.vkey'
  policy_skey=policy_path+'policy.skey'
  policy = {}
  policy['policy_script'] = policy_script
  policy['policy_vkey'] = policy_vkey
  policy['policy_skey'] = policy_skey

  # check if token exists
  # if so, returns existing policy
  if path.exists(policy_script) :
    print("Policy exists : no policy created for", policy_name)
    policy_id = subprocess_run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True)
    policy['policy_id'] = policy_id.stdout.decode().replace('\n', '')
    return policy

  makedirs(policies_folder+policy_name, mode=0o777, exist_ok=True)

  rc = subprocess_run(['cardano-cli', 'address', 'key-gen', '--verification-key-file', policy_vkey, '--signing-key-file', policy_skey], capture_output=False)

  # create policy script
  keyhash = subprocess_run(['cardano-cli', 'address', 'key-hash', '--payment-verification-key-file', policy_vkey], capture_output=True, text=True)
  data = {}
  data['keyHash'] = keyhash.stdout.replace('\n', '')
  data['type'] = 'sig'
  with open(policy_script, 'w') as outfile:
    json_dump(data, outfile)

  # get policy id
  policy_id = subprocess_run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True, text=True)

  policy['policy_id'] = policy_id.stdout.replace('\n', '')

  return policy

def get_address(address_file):
  """
  get address from file
  """
  if not path.exists(address_file) :
    print("file not found :", address_file)
    return None
  addr_file = open(address_file,'r')
  address = addr_file.readlines()
  return address[0]

def get_policy(policy_name, policy_path):
  """
  get policy
  """
  if policy_name is None:
    return {}
  policy_script=policy_path+'policy.script'
  policy_vkey=policy_path+'policy.vkey'
  policy_skey=policy_path+'policy.skey'

  policy = {}
  policy['policy_script'] = policy_script
  policy['policy_vkey'] = policy_vkey
  policy['policy_skey'] = policy_skey

  # check if policy script exists
  # if so, returns existing policy
  if(path.exists(policy_script)) :
    run_params = ['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script]
    policy_id = subprocess_run(run_params, capture_output=True)
    policy['policy_id'] = policy_id.stdout.decode().replace('\n', '')
    return policy
  else:
    return {}

def get_policy_id(token_name, utxo):
  """
  retrieve policy id from token name in utxo
  """
  assets_id = [k.split('.') for k in utxo['balances'].keys() if len(k.split('.')) == 2 and k.split('.')[1] == token_name]
  if len(assets_id) == 1:
    policy_id = assets_id[0][0]
  else:
    policy_id = None
  return policy_id

def get_protocol_keydeposit(network):
  """
  get keyDeposit parameter from protocol
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  network_era = network['network_era']
  env_param = network['env']
  rc = subprocess_run(['cardano-cli', 'query', 'protocol-parameters', network_name, network_magic, network_era], \
    capture_output=True, text=True, env=env_param)
  return int(json_loads(rc.stdout)['keyDeposit'])

def get_protocol_parameters(network, protparams_file):
  """
  get protocol parameters
  """
  network_name = network['network']
  network_magic = str(network['network_magic'])
  network_era = network['network_era']
  env_param = network['env']
  subprocess_run(['cardano-cli', 'query', 'protocol-parameters', network_name, network_magic, network_era, '--out-file', protparams_file], \
    capture_output=False, text=True, env=env_param)
  return 

def get_policy_path(address_path, owner, policy_name, policies_folder):
  return get_address_path(address_path, owner)+policies_folder+policy_name+'/'

def get_address_path(addresses_path, name):
  return addresses_path+name+'/'

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

  addr_key_file = get_address_path(addresses_path, name)+address_type+name+ext
  return addr_key_file

def get_address_file(addresses_path, address_type, name):
  """
  give file name for name type address
  """
  return get_address_key_file(addresses_path, address_type, 'address', name)

def get_skey_file(addresses_path, address_type, name):
  """
  give file name for name type signing key
  """
  return get_address_key_file(addresses_path, address_type, 'signing_key', name)

def get_vkey_file(addresses_path, address_type, name):
  """
  give file name for name type verification key
  """
  return get_address_key_file(addresses_path, address_type, 'verification_key', name)

