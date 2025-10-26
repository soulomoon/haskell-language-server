{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Development.IDE.Graph.Internal.Scheduler
  (
  isDirty
  ) where


import           Development.IDE.Graph.Internal.Types (Result (..))


-- | isDirty
-- only dirty when it's build time is older than the changed time of one of its dependencies
isDirty :: Foldable t => Result -> t (a, Result) -> Bool
isDirty me = any (\(_,dep) -> resultBuilt me < resultChanged dep)
