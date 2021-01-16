#!/usr/bin/env python3
'''
Get address value, utxo and balance for Name
'''
from locale import setlocale, LC_ALL
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import calculate_tokens_balance, get_address, get_address_file
from transaction import get_utxo_from_wallet

def print_balance(network, name, address):

  utxo = get_utxo_from_wallet(network, address)
  if utxo is None:
    print("No transaction for address", address)
    return
  token_dict = calculate_tokens_balance(utxo['tokens'])
  setlocale(LC_ALL, '')
  print('%32s%52s'%('Token', 'Amount') )
  print('----------------------------------------------------------------------------------------')
  for token, amount in token_dict.items():
    print('%-77s%11s'% (token, "{0:n}".format(amount)))
  print("\n")
  return

def print_utxo(network, name, address):
  utxo = get_utxo_from_wallet(network, address)
  if utxo is None:
    print("No transaction for address", address)
    return
  tx_list = utxo['raw']
  for tx in tx_list:
    print(tx)
  return

def main():
  """
  read parameters from command line
  and get address, utxo or balance for address name
  """
  # parse command line parameters
  example_text = '''examples:

  get utxo for Alice's payment address :  
  python3 %(prog)s --utxo Alice
   ; 
  get balance for Alice's stake address :  
  python3 %(prog)s --balance --stake Alice
  '''
  parser = ArgumentParser(description='Get utxo or balance for address of user Name', epilog=example_text)
  parser.add_argument('name', help='address or key pair name')
  parser.add_argument('-p', '--payment', action="store_true", help='payment address', required=False)
  parser.add_argument('-s', '--stake', action="store_true", help='stake address', required=False)
  parser.add_argument('-b', '--balance', action="store_true", help='display balance', required=False)
  parser.add_argument('-u', '--utxo', action="store_true", help='display utxo', required=False)

  args = parser.parse_args()

  # load env vars
  load_dotenv()
  env_param = environ
  env_param['CARDANO_NODE_SOCKET_PATH'] = getenv('CARDANO_NODE_SOCKET_PATH')
  addresses_path = getenv('ADDRESSES_PATH')
  network = {}
  network['env'] = env_param
  network['network'] = '--'+getenv('NETWORK')
  network['network_magic'] = int(getenv('NETWORK_MAGIC'))
  network['network_era'] = '--'+getenv('NETWORK_ERA')
  
  # check parameters
  if not any(vars(args).values()):
    parser.print_help()

  name = args.name.capitalize()
  if args.stake:
    address_type = 'stake'
  else:
    address_type = 'payment'
  
  address = get_address(get_address_file(addresses_path, address_type, name))
  if address is None:
    print("No", address_type, "address for", name)
    return
  print("Address :", address, "\n")
  if(args.balance is True):
    print_balance(network, name, address)

  if(args.utxo is True):
    print_utxo(network, name, address)
  return

if __name__ == '__main__':
  main()