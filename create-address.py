'''
Create Cardano Key pair and Address
'''
from argparse import ArgumentParser
from os import environ, getenv
from dotenv import load_dotenv
from tokutils import create_keypair, get_address
from transaction import create_address

def create_payment_address(network, addresses_path, address_name):
  address = create_address(network,'address', addresses_path, 'payment', address_name.capitalize())
  print(address_name, 'payment address :', address)
  return

def create_payment_keypair(addresses_path, address_name):
  create_keypair('address', addresses_path, 'payment', address_name.capitalize())
  return

def create_stake_address(network, addresses_path, address_name):
  address = create_address(network, 'stake-address', addresses_path, 'stake', address_name.capitalize())
  print(address_name, 'stake address :', address)
  return

def create_stake_keypair(addresses_path, address_name):
  create_keypair('stake-address', addresses_path, 'stake', address_name.capitalize())
  return

def main():
  """
  read parameters from command line
  and create address
  """
  # parse command line parameters
  example_text = '''examples:

  create payment key pair for Alice :  
  python3 %(prog)s --payment-key Alice
   ; 
  create payment address for Alice :  
  python3 %(prog)s --payment Alice
   ; 
  create stake key pair for Bob :
  python3 %(prog)s --stake-key Bob
   ; 
  create stake key pair and address for Bob :
  python3 %(prog)s --stake-key --stake Bob
  '''
  parser = ArgumentParser(description='Create payment or stake key pair and address', epilog=example_text)
  parser.add_argument('name', help='address or key pair name')
  parser.add_argument('--payment-key', action="store_true", help='create payment key pair', required=False)
  parser.add_argument('--stake-key', action="store_true", help='create stake key pair', required=False)
  parser.add_argument('-p', '--payment', action="store_true", help='create payment address', required=False)
  parser.add_argument('-s', '--stake', action="store_true", help='create stake address', required=False)

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
  
  # check parameters
  if not any(vars(args).values()):
    parser.print_help()
    return

  name = args.name
  if(args.payment_key is True):
    create_payment_keypair(addresses_path, name)
  if(args.payment is True) or not (args.payment_key or args.stake_key or args.stake):
    create_payment_address(network, addresses_path, name)

  if(args.stake_key is True):
    create_stake_keypair(addresses_path, name)
  if(args.stake is True):
    create_stake_address(network, addresses_path, name)
  return

if __name__ == '__main__':
  main()