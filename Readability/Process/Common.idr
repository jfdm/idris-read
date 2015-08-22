-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Readability.Process.Common

import Effects
import Effect.State

import Readability.WordTypes
import Readability.Stats
import Readability.Metrics

import Readability.Process.Effs

||| Process a sentence.
processSentence : List String -> Eff () ReadEffs
processSentence Nil     = pure ()
processSentence (x::xs) = do
   let syls = countSyllables x
   let lenstr = cast $ length x
   let sws = if lenstr == 1 then 1 else 0
   let lws = if lenstr > 6 then 1 else 0
   let bws = if syls > 3 then 1 else 0
   updateReadState (\st => updateRStats lenstr syls sws lws bws st)
   processSentence xs

||| Calculate the readability scores using different metrics.
calcScores : RStats -> ReadResult
calcScores st =
    MkReadResult
      (flesch  ws sens sys)
      (ari     cs ws sens)
      (kincaid ws sens sys)
      (coleman cs ws sens)
      (fog     ws sens bwords)
      (smog    bwords sens)
  where
    cs     = cast $ chars st
    sys    = cast $ sylls st
    ws     = cast $ nowords st
    sens   = cast $ sentances st
    bwords = cast $ bigwords st

-- --------------------------------------------------------------------- [ EOF ]
