'''
Create new Cardano Address
'''
from argparse import ArgumentParser
from os import getenv
from dotenv import load_dotenv
from tokutils import create_address

def create_payment_address(addresses_path, address_name):
  create_address('address', addresses_path, 'payment', address_name.capitalize())
  return

def create_stake_address(addresses_path, address_name):
  create_address('stake-address', addresses_path, 'stake', address_name.capitalize())
  return

def main():
  """
  read parameters from command line
  and create address
  """
  # parse command line parameters
  example_text = '''examples:

  create payment address for Alice :  
  python3 %(prog)s --payment Alice
   ; 
  create stake address for Bob :
  python3 %(prog)s --stake Bob
  '''
  parser = ArgumentParser(description='Create payment or stake address', epilog=example_text)
  parser.add_argument('-p', '--payment', nargs='?', help='payment address name', required=False)
  parser.add_argument('-s', '--stake', nargs='?', help='stake address name', required=False)
  args = parser.parse_args()

  # load env vars
  load_dotenv()
  
  addresses_path = getenv('ADDRESSES_PATH')
  
  # check parameters
  if not any(vars(args).values()):
    parser.print_help()

  if(args.payment is not None):
    create_payment_address(addresses_path, args.payment)

  if(args.stake is not None):
    create_stake_address(addresses_path, args.stake)
  return

if __name__ == '__main__':
  main()