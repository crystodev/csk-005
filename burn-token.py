'''
Burn Cardano Token
'''
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import calculate_tokens_balance, get_policy, get_address, get_protocol_parameters
from transaction import build_burn_transaction, calculate_burn_fees, get_address_file, get_skey_file, get_transaction_file, get_utxo_from_wallet, sign_burn_transaction, submit_transaction

def burn(network, address, skey_file, token, amount):
  """
  burn amount of token for address on given network
  """
  protocol_parameters_file = '/tmp/protparams.json'

  # 1. Get policy for our token
  policy = get_policy(token, network['tokens_path'])
  if (policy == {}):
    print("Token does not exist : no policy for token", token)
    return

  # 2. Extract protocol parameters (needed for fee calculations)
  get_protocol_parameters(network, protocol_parameters_file)

  # 3. Get UTXOs from our wallet
  utxo = get_utxo_from_wallet(network, address)

  # 4. Calculate tokens balance
  utxo['balances'] = calculate_tokens_balance(utxo['tokens'])

  # 5. Calculate fees for the transaction
  min_fee = calculate_burn_fees(network, address, token, amount, policy['policy_id'], utxo, protocol_parameters_file)
  if min_fee is None:
    return

  # 6. Build actual transaction including correct fees
  ok_fee_file = get_transaction_file(token, 'ok-fee')
  build_burn_transaction(network, address, token, amount, policy['policy_id'], utxo, min_fee, ok_fee_file)

  # 7. Sign the transaction
  sign_file = get_transaction_file(token, 'sign')
  sign_burn_transaction(network, skey_file, policy, ok_fee_file, sign_file)

  # 8. Submit the transaction to the blockchain
  submit_transaction(network, sign_file)
  return

def main():
  """
  read parameters from command line
  and burn token
  """
  # parse command line parameters
  example_text = '''example:

  python3 %(prog)s --name Alice --token TOK --amount 10000
  ;
  python3 %(prog)s --address paymentAlice.addr paymentAlice.skey --token TOK --amount 10000
  '''
  parser = ArgumentParser(description='Mint amount Token for address with signing key.', epilog=example_text)
  group = parser.add_mutually_exclusive_group(required=True)
  group.add_argument('-n', '--name', help='payment address owner name')
  group.add_argument('-a', '--address', nargs=2, help='address_file and signing_key_file')
  parser.add_argument('-t', '--token', help='token name', required=True)
  parser.add_argument('--amount', type=int, help='token amount', required=True)
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
  if args.name:
    name = args.name
    address = get_address(get_address_file(addresses_path, 'payment', name))
    skey_file = get_skey_file(addresses_path, 'payment', name)
  else:
    address = get_address(addresses_path+args.address[0])
    skey_file= addresses_path+args.address[1]
  token = args.token
  amount = args.amount

  # burn token
  burn(network, address, skey_file, token, amount)

if __name__ == '__main__':
  main()