#!/usr/bin/env python3
'''
Send some Cardano Token
'''
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import calculate_tokens_balance, get_policy, get_address, get_protocol_parameters
from transaction import build_send_transaction, calculate_send_fees, get_address_file, get_skey_file, get_transaction_file, get_utxo_from_wallet, sign_send_transaction, submit_transaction

def send(network, destination_address, source_address, skey_file, ada_amount, token, token_amount):
  """
  send amount of token for address on given network
  """
  protocol_parameters_file = '/tmp/protparams.json'

  # 1. Get policy for our token
  if token is not None:
    policy = get_policy(token, network['tokens_path'])
    if (policy == {}):
      print("Token does not exist : no policy for token", token)
      return
    policy_id = policy['policy_id']
  else:
    policy_id = None
  # 2. Extract protocol parameters (needed for fee calculations)
  get_protocol_parameters(network, protocol_parameters_file)

  # 3. Get UTXOs from our wallet
  utxo = get_utxo_from_wallet(network, source_address)

  # 4. Calculate tokens balance
  utxo['balances'] = calculate_tokens_balance(utxo['tokens'])

  # 5. Calculate fees for the transaction
  min_fee = calculate_send_fees(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, protocol_parameters_file)
  if min_fee is None:
    return

  # 6. Build actual transaction including correct fees
  ok_fee_file = get_transaction_file(token, 'ok-fee')
  rc = build_send_transaction(network, destination_address, source_address, ada_amount, token, token_amount, policy_id, utxo, min_fee, ok_fee_file)
  if not rc:
    print('Transaction aborted')
    return

  # 7. Sign the transaction
  sign_file = get_transaction_file(token, 'sign')
  sign_send_transaction(network, skey_file, ok_fee_file, sign_file)

  # 8. Submit the transaction to the blockchain
  submit_transaction(network, sign_file)
  return
 
def main():
  """
  read parameters from command line
  and transfer token
  """
  # parse command line parameters
  example_text = '''example:

  python3 %(prog)s --destination Bob --source Alice --token TOK 10000
  ;
  python3 %(prog)s --to-address paymentBob.addr --from--address paymentAlice.addr paymentAlice.skey --token TOK 10000
  '''
  parser = ArgumentParser(description='Mint amount Token for address with signing key.', epilog=example_text)
  group_src = parser.add_mutually_exclusive_group(required=True)
  group_src.add_argument('-s', '--source', help='payer name')
  group_src.add_argument('-f', '--from-address', nargs=2, help='payer address_file and signing_key_file')
  group_dst = parser.add_mutually_exclusive_group(required=True)
  group_dst.add_argument('-d', '--destination', help='destination address owner name')
  group_dst.add_argument('-a', '--address', help='destination address')
  group_dst.add_argument('--to-address', help='destination address_file')
  parser.add_argument('-t', '--token', nargs=2, help='token name and token amount', default=None)
  parser.add_argument('--ada', type=int, help='ada amount', default=0)
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
  network['tokens_path'] = getenv('TOKENS_PATH')
  addresses_path = getenv('ADDRESSES_PATH')
  
  # set parameters
  if args.source:
    name = args.source
    src_address = get_address(get_address_file(addresses_path, 'payment', name))
    skey_file = get_skey_file(addresses_path, 'payment', name)
  else:
    src_address = get_address(addresses_path+args.from_address[0])
    skey_file= addresses_path+args.from_address[1]
  if args.destination:
    dst_address = get_address(get_address_file(addresses_path, 'payment', args.destination))
  elif args.address:
    dst_address = args.address
  else:
    dst_address = get_address(addresses_path+args.to_address)
  if dst_address is None :
    print("Invalid destination address")
    return

  if args.token:
    token = args.token[0]
    token_amount = int(args.token[1])
  else:
    token_amount = 0
  ada_amount = args.ada

  # send token
  send(network, dst_address, src_address, skey_file, ada_amount, token, token_amount)

if __name__ == '__main__':
  main()