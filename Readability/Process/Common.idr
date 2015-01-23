module Readability.Process.Common

import Effects
import Effect.State

import Readability.WordTypes
import Readability.Stats
import Readability.Metrics


||| Process a sentence.
processSentence : List String -> {[STATE RStats]} Eff ()
processSentence Nil     = pure ()
processSentence (x::xs) = do
   let syls = countSyllables x
   let lenstr = cast $ length x
   let sws = if lenstr == 1 then 1 else 0
   let lws = if lenstr > 6 then 1 else 0
   let bws = if syls > 3 then 1 else 0
   st <- get
   put $ updateRStats st lenstr syls sws lws bws
   processSentence xs


||| Calculate the readability scores using different metrics.
calcScores : RStats -> List (RMetricTy, Float)
calcScores st = [
    (FLESCH,  flesch ws sens sys),
    (ARI,     ari cs ws sens),
    (KINCAID, kincaid ws sens sys),
    (COLEMAN, coleman cs ws sens),
    (FOG,     fog ws sens bwords),
    (SMOG,    smog bwords sens)
    ]
  where
    cs     = cast $ chars st
    sys    = cast $ sylls st
    ws     = cast $ words st
    sens   = cast $ sentances st
    bwords = cast $ bigwords st

-- --------------------------------------------------------------------- [ EOF ]
