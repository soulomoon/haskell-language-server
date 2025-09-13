module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeNewDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeRunDatabaseForKeysSep,
    shakeProfileDatabase,
    shakeGetBuildStep,
    shakeGetDatabaseKeys,
    shakeGetDirtySet,
    shakeGetCleanKeys
    ,shakeGetBuildEdges,
    shakeShutDatabase,
    shakeGetActionQueueLength,
    shakeComputeToPreserve,
    shakedatabaseRuntimeDep,
    shakePeekAsyncsDelivers) where
import           Control.Concurrent.Async                (Async)
import           Control.Concurrent.STM.Stats            (atomically,
                                                          readTVarIO)
import           Control.Exception                       (SomeException)
import           Control.Monad                           (join)
import           Data.Dynamic
import           Data.Maybe
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Development.IDE.Graph.Classes           ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Profile  (writeProfile)
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.WorkerThread            (DeliverStatus)
import qualified ListT
import qualified StmContainers.Map
import qualified StmContainers.Map                       as SMap


-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeShutDatabase :: Set (Async ()) -> ShakeDatabase -> IO ()
shakeShutDatabase preserve (ShakeDatabase _ _ db) = shutDatabase preserve db

shakeNewDatabase :: (String -> IO ()) -> DBQue -> ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase l que opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase l que extra theRules
    pure $ ShakeDatabase (length actions) actions db

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO [Either SomeException a]
shakeRunDatabase s xs = shakeRunDatabaseForKeys Nothing s xs

-- | Returns the set of dirty keys annotated with their age (in # of builds)
shakeGetDirtySet :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDirtySet (ShakeDatabase _ _ db) =
    Development.IDE.Graph.Internal.Database.getDirtySet db

-- | Returns the build number
shakeGetBuildStep :: ShakeDatabase -> IO Int
shakeGetBuildStep (ShakeDatabase _ _ db) = do
    Step s <- readTVarIO $ databaseStep db
    return s

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- | Assumes that the database is not running a build
-- The nested IO is to
-- seperate incrementing the step from running the build
shakeRunDatabaseForKeysSep
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO (IO [Either SomeException a])
shakeRunDatabaseForKeysSep keysChanged (ShakeDatabase lenAs1 as1 db) as2 = do
    incDatabase db keysChanged
    return $ drop lenAs1 <$> runActions (newKey "root") db (map unvoid as1 ++ as2)

shakedatabaseRuntimeDep :: ShakeDatabase -> IO [(Key, KeySet)]
shakedatabaseRuntimeDep (ShakeDatabase _ _ db) =
    atomically $ ListT.toList $ SMap.listT (databaseRuntimeDep db)


shakeComputeToPreserve :: ShakeDatabase -> KeySet -> IO ([(Key, Async ())], [Key])
shakeComputeToPreserve (ShakeDatabase _ _ db) ks = atomically (computeToPreserve db ks)

--a dsfds
-- fds make it possible to do al ot of jobs
shakeRunDatabaseForKeys
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO [Either SomeException a]
shakeRunDatabaseForKeys keysChanged sdb as2 = join $ shakeRunDatabaseForKeysSep keysChanged sdb as2


shakePeekAsyncsDelivers :: ShakeDatabase -> IO [DeliverStatus]
shakePeekAsyncsDelivers (ShakeDatabase _ _ db) = peekAsyncsDelivers db

-- | Given a 'ShakeDatabase', write an HTML profile to the given file about the latest run.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase _ _ s) file = writeProfile file s

-- | Returns the clean keys in the database
shakeGetCleanKeys :: ShakeDatabase -> IO [(Key, Result )]
shakeGetCleanKeys (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    return [ (k,res) | (k, Clean res) <- keys]

-- | Returns the total count of edges in the build graph
shakeGetBuildEdges :: ShakeDatabase -> IO Int
shakeGetBuildEdges (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    let ress = mapMaybe (getResult . snd) keys
    return $ sum $ map (lengthKeySet . getResultDepsDefault mempty . resultDeps) ress

-- | Returns an approximation of the database keys,
--   annotated with how long ago (in # builds) they were visited
shakeGetDatabaseKeys :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDatabaseKeys (ShakeDatabase _ _ db) = getKeysAndVisitAge db

shakeGetActionQueueLength :: ShakeDatabase -> IO Int
shakeGetActionQueueLength (ShakeDatabase _ _ db) =
    atomically $ databaseGetActionQueueLength db
