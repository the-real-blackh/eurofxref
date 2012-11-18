import Financial.EuroFXRef
import Network.HTTP.Conduit (parseUrl)
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import Data.Time.Format
import Prelude hiding (catch)
import System.Locale (defaultTimeLocale)

main = do
    r <- rebase (Currency "USD") <$> fetch
    dumpRates r
  `catch` (\exc -> do
    putStrLn $ "FAILED: "++show (exc :: EuropeanCentralBankException)
    return ())
  `catch` (\exc -> do
    putStrLn $ "IOException: "++show (exc :: IOException)
    return ())

dumpRates :: Rates Double -> IO ()
dumpRates r = do
    let Currency cur = raReference r
    putStrLn $ "The value of "++ cur ++ " 1 in each currency on "++formatTime defaultTimeLocale "%F" (raTime r)
    forM_ (M.assocs . raRates $ r) $ \(Currency cur, rate) -> do
        putStrLn $ cur ++ " " ++ show rate
