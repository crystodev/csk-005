#!/usr/bin/env python3
'''
Mint Cardano Token
'''
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import calculate_tokens_balance, create_policy, get_address, get_address_file, get_skey_file, get_policy, get_policy_path, get_protocol_parameters
from transaction import build_mint_transaction, calculate_mint_fees, get_transaction_file, get_utxo_from_wallet, sign_mint_transaction, submit_transaction

def mint(network, owner, src_address, skey_file, dst_address, policy_name, token_name, token_amount):
  """
  mint amount of token for address on given network
  """
  protocol_parameters_file = '/tmp/protparams.json'

  # 1. Create a policy for our token
  if policy_name is None:
    # if no policy name specified, search policy with token name
    policy = create_policy(token_name, network['policies_path'])
    policy_name = token_name
  else :
    policy = get_policy(policy_name, network['policies_path'])

  if (policy == {}):
    print("Policy does not exist : no policy for token", token_name, "with policy", policy_name)
    return

  # don't mint if amount equals 0
  if token_amount == 0:
    return

  # 2. Extract protocol parameters (needed for fee calculations)
  get_protocol_parameters(network, protocol_parameters_file)

  # 3. Get UTXOs from our wallet
  utxo = get_utxo_from_wallet(network, src_address)

  # 4. Calculate tokens balance
  utxo['balances'] = calculate_tokens_balance(utxo['tokens'])

  # 5. Calculate fees for the transaction
  min_fee = calculate_mint_fees(network, src_address, token_name, token_amount, policy['policy_id'], utxo, protocol_parameters_file)
  if min_fee is None:
    return
  
  # 6. Build actual transaction including correct fees
  ok_fee_file = get_transaction_file(token_name, 'ok-fee')

  rc = build_mint_transaction(network, src_address, dst_address, token_name, token_amount, policy['policy_id'], utxo, min_fee, ok_fee_file)
  if not rc:
    print("Failed to build transaction")
    return
  
  # 7. Sign the transaction
  sign_file = get_transaction_file(token_name, 'sign')
  sign_mint_transaction(network, skey_file, policy, ok_fee_file, sign_file)

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

  python3 %(prog)s --owner Alice --policy POL --token TOK 10000
  ;
  python3 %(prog)s -owner Alice --from-address paymentAlice.addr paymentAlice.skey --policy POL --token TOK 10000
  '''
  parser = ArgumentParser(description='Mint amount Token for address with signing key.', epilog=example_text)
  parser.add_argument('-o', '--owner', help='mint address owner name', required=True)
  parser.add_argument('-f', '--from-address', nargs=2, help='mint address_file and signing_key_file')
  group_dst = parser.add_mutually_exclusive_group(required=False)
  group_dst.add_argument('-d', '--destination', help='destination address owner name')
  group_dst.add_argument('-a', '--address', help='destination address')
  group_dst.add_argument('--to-address', help='destination address_file')
  parser.add_argument('-t', '--token', nargs=2, help='token name and token amount', required=True)
  parser.add_argument('-p', '--policy', help='policy name', required=True)
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
  addresses_path = getenv('ADDRESSES_PATH')

  # set parameters
  name = args.owner.capitalize()
  policy_name = args.policy
  network['policies_path'] = get_policy_path(addresses_path, name, policy_name, getenv('POLICIES_FOLDER'))
  if args.from_address:
    src_address = get_address(addresses_path+args.from_address[0])
    skey_file= addresses_path+args.from_address[1]
  else:
    src_address = get_address(get_address_file(addresses_path, 'payment', name))
    skey_file = get_skey_file(addresses_path, 'payment', name)
  if args.token:
    token_name = args.token[0]
    token_amount = int(args.token[1])
  else:
    token_name = None
    token_amount = 0
  if args.destination:
    dst_address = get_address(get_address_file(addresses_path, 'payment', args.destination.capitalize()))
  elif args.address:
    dst_address = args.address
  elif args.to_address:
    dst_address = get_address(addresses_path+args.to_address)
  else :
    dst_address = src_address

  # mint token
  mint(network, name, src_address, skey_file, dst_address, policy_name, token_name, token_amount)

if __name__ == '__main__':
  main()