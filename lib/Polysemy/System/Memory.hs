module Polysemy.System.Memory (
  Memory
, performMajorGC, performMinorGC
, getAllocationCounter, setAllocationCounter
, enableAllocationLimit, disableAllocationLimit
, runMemory
) where

import Prelude
import Polysemy
import qualified System.Mem as M
import Data.Int (Int64)

data Memory :: Effect where
  PerformMajorGC :: Memory m ()
  PerformMinorGC :: Memory m ()
  GetAllocationCounter :: Memory m Int64
  SetAllocationCounter :: Int64 -> Memory m ()
  EnableAllocationLimit :: Memory m ()
  DisableAllocationLimit :: Memory m ()

makeSem ''Memory

runMemory
  :: Member (Embed IO) r
  => InterpreterFor Memory r
runMemory = interpret $ \case
  PerformMajorGC -> embed M.performMajorGC
  PerformMinorGC -> embed M.performMinorGC
  GetAllocationCounter -> embed M.getAllocationCounter
  SetAllocationCounter c -> embed $ M.setAllocationCounter c
  EnableAllocationLimit -> embed M.enableAllocationLimit
  DisableAllocationLimit -> embed M.disableAllocationLimit


