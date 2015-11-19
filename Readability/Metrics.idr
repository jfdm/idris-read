-- ------------------------------------------------------------- [ Metrics.idr ]
-- Module    : Metrics.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Readability Metrics
|||
||| http://www.editcentral.com/gwt1/EditCentral.html
module Readability.Metrics

data RMetricTy = FLESCH | ARI | KINCAID | COLEMAN | FOG | SMOG

||| Flesch reading ease score
|||
||| ```java
||| 206.835 - (1.015 * wordCount) / sentenceCount - (84.6 * syllableCount) / wordCount;
||| ```
||| @wcount Number of words
||| @scount Number of sentences.
||| @sycount Number of syllables.
flesch : (wcount : Double) -> (scount : Double) -> (sycount : Double) -> Double
flesch ws ss sys = 206.835 - (1.015 * ws) / ss - (84.6 * sys) / ws

||| Automated readability index
|||
||| ```java
||| (4.71 * letterNumberCount) / wordCount + (0.5 * wordCount) / sentenceCount -21.43;
||| ```
||| @ccount Number of Characters
||| @wcount Number of words.
||| @scount Number of sentences.
ari : (ccount : Double) -> (wcount : Double) -> (scount : Double) -> Double
ari as ws ss = (4.71 * as) / ws + (0.5 * ws) / ss - 21.43


||| Flesch-Kincaid grade level
|||
||| ```java
||| (0.39 * wordCount) / sentenceCount + (11.8 * syllableCount) / wordCount - 15.59;
||| ```
||| @wcount The number of words
||| @scount Number of sentences
||| @sycount Number of syllables.
kincaid : (wcount : Double) -> (scount : Double) -> (sycount : Double) -> Double
kincaid ws ss sys = (0.39 * ws) / ss + (11.8 * sys) / ws - 15.59


||| Coleman-Liau index
|||
||| ```java
||| (5.89 * letterNumberCount) / wordCount - (30.0 * sentenceCount) / wordCount - 15.8;
||| ```
||| @ccount Number of letters and digits.
||| @wcount The number of words.
||| @scount The number of sentences.
coleman : (ccount : Double) -> (wcount : Double) -> (scount : Double) -> Double
coleman as ws ss = (5.89 * as) / ws - (30.0 * ss) / ws - 15.8


||| Gunning fog index
|||
||| ```java
|||  0.4 * ( (double)wordCount / sentenceCount + (100.0 * complexCount) / wordCount );
||| ```
||| @wcount Number of Words
||| @scount Number of sentences
||| @bwcount Number of words with three plus syllables.
fog : (wcount : Double) -> (scount : Double) -> (bwcount : Double) -> Double
fog ws ss bs = 0.4 * ( ws / ss + (100.0 * bs) / ws)

||| SMOG index
|||
||| ```java
||| Math.sqrt( complexCount * 30.0 / sentenceCount ) + 3.0;
||| ```
||| @bwcount The number of word with three plus syllables.
||| @scount The number of sentences.
smog : (bwcount : Double) -> (scount : Double) -> Double
smog bs ss = 3.0 + sqrt (bs * (30 / ss))


instance Show RMetricTy where
  show FLESCH  = "FLESCH"
  show ARI     = "ARI"
  show KINCAID = "KINCAID"
  show COLEMAN = "COLEMAN"
  show FOG     = "FOG"
  show SMOG    = "SMOG"

instance Eq RMetricTy where
  (==) FLESCH  FLESCH  = True
  (==) ARI     ARI     = True
  (==) KINCAID KINCAID = True
  (==) COLEMAN COLEMAN = True
  (==) FOG     FOG     = True
  (==) SMOG    SMOG    = True
  (==) _        _      = False

readRMetricTy : String -> Maybe RMetricTy
readRMetricTy s =
  case toLower s of
    "flesch"  => Just FLESCH
    "ari"     => Just ARI
    "kincaid" => Just KINCAID
    "coleman" => Just COLEMAN
    "fog"     => Just FOG
    "smog"    => Just SMOG
    otherwise  => Nothing


record ReadResult where
  constructor MkReadResult
  flesch  : Double
  ari     : Double
  kincaid : Double
  coleman : Double
  fog     : Double
  smog    : Double

toList : ReadResult -> List (RMetricTy, Double)
toList (MkReadResult a b c d e f) =
    [ (FLESCH,  a)
    , (ARI,     b)
    , (KINCAID, c)
    , (COLEMAN, d)
    , (FOG,     e)
    , (SMOG,    f)]

instance Show ReadResult where
  show rslt = show (Metrics.toList rslt)
-- --------------------------------------------------------------------- [ EOF ]
