'''
Mint Cardano Token
'''
import argparse
import os
import json
from dotenv import load_dotenv
from tokutils import calculate_tokens_balance, create_policy, get_address, get_protocol_parameters
from transaction import build_mint_transaction, calculate_fees, get_utxo_from_wallet, sign_transaction, submit_transaction

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