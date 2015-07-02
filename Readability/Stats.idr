module Readability.Stats

import Effects

record RStats where
  constructor MkRStats
  chars : Int
  sylls : Int
  nowords : Int
  shortwords : Int
  longwords : Int
  bigwords : Int
  sentances : Int

instance Default (RStats) where
    default = MkRStats 0 0 0 0 0 0 0

instance Show RStats where
  show st = "No. of chars: " ++ show (chars st) ++
       "\nNo. of syllables: " ++ show (sylls st) ++
       "\nNo. of words: " ++ show (nowords st) ++
       "\nNo. of short words: " ++ show (shortwords st) ++
       "\nNo. of long words: "  ++ show (longwords st) ++
       "\nNo. of big words: "   ++ show (bigwords st) ++
       "\nNo. of sentances: "   ++ show (sentances st)


updateRStats : RStats -> Int -> Int -> Int -> Int -> Int  -> RStats
updateRStats st cs ss sws lws bws = record {
             chars = (chars st) + cs,
             sylls = (sylls st) + ss,
             nowords = (nowords st) + 1,
             shortwords = (shortwords st) + sws,
             longwords  = (longwords st)  + lws,
             bigwords  = (bigwords st)    + bws
             } st

-- --------------------------------------------------------------------- [ EOF ]
