import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( isJust, fromJust )
import Baseutils ( capitalized )
import Tokutils ( createPolicy, getPolicyPath, getPolicyId )
import Control.Monad (void)
import Configuration.Dotenv (loadFile, defaultConfig)


data Opt = Opt
  { owner      :: String
  , policy      :: String}

pgmOpts :: Parser Opt
pgmOpts = Opt
      <$> strOption
          ( long "owner"
         <> short 'o'
         <> metavar "OWNER"
         <> help "address owner name" )
      <*> strOption
          ( long "policy"
         <> short 'p'
         <> metavar "POLICY"
         <> help "policy name" )


main :: IO ()
main = doCreatePolicy =<< execParser opts
  where
    opts = info (pgmOpts <**> helper)
      ( fullDesc
     <> progDesc "Create Cardano minting policy"
     <> header "create-policy - a simple minting policy creator" )

doCreatePolicy :: Opt -> IO ()
doCreatePolicy (Opt o p ) = do
  -- TODO : Read environment variables addressPath and policiesFolder
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let owner = capitalized o
  let policy = p
  putStrLn $ "Creating policy " ++ policy ++ " for " ++ owner ++ "\n"
  let policyPath = getPolicyPath addressPath owner policy policiesFolder
  putStrLn $ "Policy path : " ++ policyPath

  mpolicy <- createPolicy owner policyPath
  if isJust mpolicy then do
    putStrLn $ "Policy id : " ++ (getPolicyId $ fromJust mpolicy)
  else
    putStrLn $ "Policy " ++ capitalized p ++ " not created"
