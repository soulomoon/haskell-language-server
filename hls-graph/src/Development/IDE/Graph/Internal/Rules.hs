-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Development.IDE.Graph.Internal.Rules where

import           Control.Exception.Extra
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString                      as BS
import           Data.Dynamic
import qualified Data.HashMap.Strict                  as Map
import           Data.IORef
import           Data.Maybe
import           Data.Typeable
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import Data.Kind (Type)

-- | The type mapping between the @key@ or a rule and the resulting @value@.
type family RuleResult key -- = value
type family RunResults keys where
    RunResults '[] = '[]
    RunResults (x ': xs) = RunResult x ': RunResults xs

-- type family MapListType f keys where
--     MapListType _ '[] = '[]
--     MapListType f (x ': xs) = f x ': MapListType f xs

-- type family MapResults as bs where
    -- MapResults '[] = '[]
    -- MapResults (a ': as) = RunResult a ': MapResults as

class HMap f as where
    hMap :: f -> HList as -> HList (RunResults as)

type IsKey a = (Typeable a, Hashable a, Show a)

data HList :: [Type] -> Type where
    HNil :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

class HListKeys as where
    hListList :: HList as -> [Key]
instance HListKeys '[] where
    hListList HNil = []
instance (IsKey a, HListKeys as) => HListKeys (a ': as) where
    hListList (HCons k xs) = newKey k : hListList xs


class HListValues as where
    listHList :: [Dynamic] -> HList as
instance HListValues '[] where
    listHList [] = HNil
    listHList _ = error "listHList: too many elements"
instance (Typeable a, HListValues as) => HListValues (a ': as) where
    listHList  [] = error "listHList: empty list"
    listHList (x:xs) = HCons (unwrapDynamic x) (listHList xs)


action :: Action a -> Rules ()
action x = do
    ref <- Rules $ asks rulesActions
    liftIO $ modifyIORef' ref (void x:)

addRule
    :: forall key value .
       (RuleResult key ~ value, Typeable key, Hashable key, Eq key, Typeable value)
    => (key -> Maybe BS.ByteString -> RunMode -> Action (RunResult value))
    -> Rules ()
addRule f = do
    ref <- Rules $ asks rulesMap
    liftIO $ modifyIORef' ref $ Map.insert (typeRep (Proxy :: Proxy key)) (toDyn f2)
    where
        f2 :: Key -> Maybe BS.ByteString -> RunMode -> Action (RunResult Value)
        f2 (Key a) b c = do
            v <- f (fromJust $ cast a :: key) b c
            v <- liftIO $ evaluate v
            pure $ Value . toDyn <$> v

runRule
    :: TheRules -> Key -> Maybe BS.ByteString -> RunMode -> Action (RunResult Value)
runRule rules key@(Key t) bs mode = case Map.lookup (typeOf t) rules of
    Nothing -> liftIO $ errorIO $ "Could not find key: " ++ show key
    Just x  -> unwrapDynamic x key bs mode

runRules :: Dynamic -> Rules () -> IO (TheRules, [Action ()])
runRules rulesExtra (Rules rules) = do
    rulesActions <- newIORef []
    rulesMap <- newIORef Map.empty
    runReaderT rules SRules{..}
    (,) <$> readIORef rulesMap <*> readIORef rulesActions
