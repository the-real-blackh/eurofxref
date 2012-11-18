module Financial.CurrencyRates where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock

-- | A standard three-letter currency name.
newtype Currency = Currency String deriving (Eq, Show, Read, Ord)

-- | A table of currency rates.
data Rates a = Rates {
        raReference :: Currency,  -- ^ The reference currency
        raTime      :: UTCTime,   -- ^ The time when the rates were valid
        raRates     :: Map Currency a  -- ^ Value of one unit of the reference currency in each currency
    }

-- | Re-base the rates to a different reference currency, such that the new rates give
-- the value of one unit of that currency.
rebase :: Fractional a => Currency -> Rates a -> Rates a
rebase new (Rates old t m) = Rates new t $ case new `M.lookup` m of
    Just newRate -> M.map (/newRate) . M.insert old 1 $ m
    Nothing      -> M.empty

