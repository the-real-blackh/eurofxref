{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
-- | Example using ghci, where we read the currency rates relative to Euros, and re-base
-- them to New Zealand dollars.
--
-- > > :m Financial.EuroFXRef Data.Map
-- > > fmap (assocs . raRates . rebase (Currency "NZD")) fetch :: IO [(Currency, Double)]
-- > [(Currency "AUD",0.7696441703909034),(Currency "BGN",1.1064094586185438),...
--
-- Each number is one unit of the reference currency in that currency,
-- e.g. in this example NZD 1 == AUD 0.77.

module Financial.EuroFXRef (
        -- * Simple
        fetch,
        EuropeanCentralBankException(..),
        -- * Lower-level
        europeanCentralBankDaily,
        fetchFrom,
        parseEuropeanCentralBank,
        module Financial.CurrencyRates
    ) where

import Financial.CurrencyRates

import Control.Applicative
import Control.Arrow (second, first)
import Control.Exception
import Control.Failure
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Conduit (MonadResource, runResourceT, ResourceT)
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import Network.HTTP.Conduit as HTTP
import Network.HTTP.Types as HTTP
import Text.XML.Expat.Tree

-- | calendar time YYYYMMDDHHMMSS to posix microseconds 
cal2utc :: (Int, Int, Int, Int, Int, Int) -> UTCTime
cal2utc (y,m,d,hh,mm,ss) =
    let ymd = toModifiedJulianDay $ fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
    in  UTCTime
            (ModifiedJulianDay $ fromIntegral ymd)
            (fromIntegral $ hh * 3600 + mm * 60 + ss)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
    [(a, "")] -> Just a
    _         -> Nothing

-- | An exception indicating a parse error in the parsing of European Central Bank.
data EuropeanCentralBankException =
    ECBXMLParseException XMLParseError |
    ECBHttpException HttpException |
    ECBHttpStatusException HTTP.Status |
    ECBParseException String 
    deriving (Show, Typeable)

instance Exception EuropeanCentralBankException where

-- | Parse the European Central Bank's XML format.
parseEuropeanCentralBank :: Read a => UNode String -> Either String (Rates a)
parseEuropeanCentralBank (Element _ _ chs) =
    case execStateT (mapM_ p2 chs) (Nothing, M.empty) of
        Right (Just time, rates) -> Right $ Rates euro time rates
        Right (Nothing, _)       -> Left $ "time stamp is missing"
        Left err                 -> Left err
  where
    euro = Currency "EUR"
    p2 (Element "Cube" _ chs) = mapM_ p3 chs
    p2 _                 = return ()
    p3 e@(Element "Cube" _ chs) = do
        case getAttribute e "time" of
            Just tstr -> case words (map (\x -> if x == '-' then ' ' else x) tstr) of
                [ys, ms, ds] -> case (maybeRead ys, maybeRead ms, maybeRead ds) of
                    (Just y, Just m, Just d) -> modify $ first $ const $ Just $
                        cal2utc (y, m, d, 13, 0, 0)  -- Updated at "3PM CET" which corresponds to
                                                     -- 13:00 UTC. Not sure about daylight savings!
                    _ -> lift $ fail $ "bad time: " ++ tstr
                _ -> lift $ fail $ "bad time: " ++ tstr
            Nothing -> lift $ fail "missing time"
        mapM_ p4 chs
    p3 _                 = return ()
    p4 e@(Element "Cube" _ _) = case (getAttribute e "currency", join $ maybeRead <$> getAttribute e "rate") of
        (Just cur, Just rate) -> modify $ second $ M.insert (Currency cur) rate
        _                     -> return ()
    p4 _                 = return ()
parseEuropeanCentralBank _ = Left "element expected at top level"

-- | The URL for the European Central Bank's free daily reference rates.
europeanCentralBankDaily :: Failure HttpException m => m (HTTP.Request m')
europeanCentralBankDaily = parseUrl "http://www.ecb.int/stats/eurofxref/eurofxref-daily.xml"

-- | Fetch today's currency rates from the specified URL.
--
-- Throws a 'EuropeanCentralBankException' for failures at HTTP and above,
-- or 'IOException' for network-level failures.
fetchFrom :: (Failure EuropeanCentralBankException m, Failure HttpException m,
              MonadResource m, MonadBaseControl IO m, Read a) =>
             HTTP.Request m
          -> HTTP.Manager
          -> m (Rates a)
fetchFrom req mgr = do
    res <- httpLbs req mgr
    let body = responseBody res
    case statusCode (responseStatus res) of
        200 -> do
            case parse defaultParseOptions body of
                (_, Just err) -> failure $ ECBXMLParseException err
                (xml, _) ->
                    case parseEuropeanCentralBank xml of
                        Left err -> failure $ ECBParseException err
                        Right rates -> return rates
        _ -> failure $ ECBHttpStatusException (responseStatus res)

-- | Fetch today's currency rates from European Central Bank server.
-- 'IO' works for @m@ and 'Double' for @a@.
--
-- Throws a 'EuropeanCentralBankException' for failures at HTTP and above,
-- or 'IOException' for network-level failures.
fetch :: (Failure EuropeanCentralBankException m, Failure HttpException m,
          MonadIO m, Read a) =>
         m (Rates a)
fetch = do
    req <- europeanCentralBankDaily
    liftIO $ withManager $ fetchFrom req
