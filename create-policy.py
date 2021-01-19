#!/usr/bin/env python3
'''
Create Cardano Minting Policy
'''
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import  create_policy, get_policy_path

def main():
  """
  read parameters from command line
  and create a single-issuer policy
  """
  # parse command line parameters

  parser = ArgumentParser(description='Create Cardano minting policy')
  parser.add_argument('policy', help='policy name')
  parser.add_argument('-o', '--owner', help='address owner name', required=True)
  args = parser.parse_args()

  # load env vars
  load_dotenv()
  env_param = environ
  env_param['CARDANO_NODE_SOCKET_PATH'] = getenv('CARDANO_NODE_SOCKET_PATH')
  network = {}
  network['env'] = env_param
  network['network'] = '--'+getenv('NETWORK')
  network['network_magic'] = int(getenv('NETWORK_MAGIC'))
  network['network_era'] = '--'+getenv('NETWORK_ERA')
  network['policies_path'] = getenv('POLICIES_FOLDER')
  
  # set parameters
  owner_name = args.owner.capitalize()
  policy_name = args.policy
  addresses_path = getenv('ADDRESSES_PATH')
  network['policies_path'] = get_policy_path(addresses_path, owner_name, policy_name, getenv('POLICIES_FOLDER'))
  
  # create policy
  policy = create_policy(args.policy, network['policies_path'])
  if (policy == {}):
    print("Policy", args.policy, "not created")
    return
  else:
    print("Policy id :", policy.get('policy_id'))
  
if __name__ == '__main__':
  main()