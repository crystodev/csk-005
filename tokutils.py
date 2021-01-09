"""
Utility module for Cardano Token manipulation
"""
from os import makedirs, path
from subprocess import run as subprocess_run 
from collections import defaultdict
from json import dump as json_dump

def calculate_tokens_balance(tokens):
  """
  compute total balance for each list of tokens (token,amount)
  return a dict with total amount for each token
  """
  result = defaultdict(int)
  for key, value in tokens:
    result[key] += value
  return result

def create_address(address_type, addresses_path, address_prefix, address_name):
  """
  create address based on address_name
  """
  vkey_file = addresses_path+address_prefix+address_name+'.vkey'
  skey_file = addresses_path+address_prefix+address_name+'.skey'

  if(path.exists(vkey_file)) :
    print(address_prefix, "address already exists :", address_name)
    return
  
  makedirs(addresses_path, mode=0o777, exist_ok=True)

  run_params = ['cardano-cli', address_type, 'key-gen', '--verification-key-file', vkey_file, '--signing-key-file', skey_file]
  subprocess_run(run_params, capture_output=False, text=True)
  return

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
  if(path.exists(tokens_path+token_name)) :
    print("Token exists : no policy created for token", token_name)
    policy_id = subprocess_run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True)
    policy['policy_id'] = policy_id.stdout.decode().replace('\n', '')
    return policy

  makedirs(tokens_path+token_name, mode=0o777, exist_ok=True)

  rc = subprocess.run(['cardano-cli', 'address', 'key-gen', '--verification-key-file', policy_vkey, '--signing-key-file', policy_skey], capture_output=False)

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
  addr_file = open(address_file,'r')
  address = addr_file.readlines()
  return address[0]


def get_policy(token_name, tokens_path):
  """
  get policy for token
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
  if(path.exists(tokens_path+token_name)) :
    policy_id = subprocess_run(['cardano-cli', 'transaction', 'policyid', '--script-file', policy_script], capture_output=True)
    policy['policy_id'] = policy_id.stdout.decode().replace('\n', '')
    return policy
  else:
    return {}

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
