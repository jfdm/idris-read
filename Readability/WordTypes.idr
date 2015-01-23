module Readability.WordTypes

import Prelude.Strings

%access private

inTwos : List a -> List (a,a)
inTwos [] = []
inTwos (x::y::xs) = (x,y) :: inTwos xs

isSomething : String -> List String -> Bool
isSomething w ws = List.elem w ws

articles : List String
articles = ["the", "a", "an"]

public
isArticle : String -> Bool
isArticle w = isSomething w articles


pronouns : List String
pronouns = ["i", "me", "we", "us", "you", "he", "him", "she", "her", "it", "they",
    "them", "thou", "thee", "ye", "myself", "yourself", "himself",
    "herself", "itself", "ourselves", "yourselves", "themselves",
    "oneself", "my", "mine", "his", "hers", "yours", "ours", "theirs", "its",
    "our", "that", "their", "these", "this", "those", "your"]

public
isPronoun : String -> Bool
isPronoun w = isSomething w pronouns

iter_pronouns : List String
iter_pronouns = ["why", "who", "what", "whom", "when", "where", "how"]

public
isInterrogativePronoun : String -> Bool
isInterrogativePronoun w = isSomething w iter_pronouns

conjunctions : List String
conjunctions = ["and", "but", "or", "yet", "nor"]

public
isConjunction : String -> Bool
isConjunction w = isSomething w conjunctions

nominalisation : List String
nominalisation = ["tion", "ment", "ence", "ance"]

public
isNominalisation : String -> Bool
isNominalisation w = elem True (map (\x => isSuffixOf (unpack x) (unpack w)) nominalisation)

subconjunction : List String
subconjunction =  ["after", "because", "lest", "till", "'til", "although", "before",
    "now that", "unless", "as", "even if", "provided that", "provided",
    "until", "as if", "even though", "since", "as long as", "so that",
    "whenever", "as much as", "if", "than", "as soon as", "inasmuch",
    "in order that", "though", "while"]

public
isSubconjunction : String -> Bool
isSubconjunction w = isSomething w subconjunction

preposition : List String
preposition = ["aboard", "about", "above", "according to", "across from",
    "after", "against", "alongside", "alongside of", "along with",
    "amid", "among", "apart from", "around", "aside from", "at", "away from",
    "back of", "because of", "before", "behind", "below", "beneath", "beside",
    "besides", "between", "beyond", "but", "by means of",
    "concerning", "considering", "despite", "down", "down from", "during",
    "except", "except for", "excepting for", "from among",
    "from between", "from under", "in addition to", "in behalf of",
    "in front of", "in place of", "in regard to", "inside of", "inside",
    "in spite of", "instead of", "into", "like", "near to", "off",
    "on account of", "on behalf of", "onto", "on top of", "on", "opposite",
    "out of", "out", "outside", "outside of", "over to", "over", "owing to",
    "past", "prior to", "regarding", "round about", "round",
    "since", "subsequent to", "together", "with", "throughout", "through",
    "till", "toward", "under", "underneath", "until", "unto", "up",
    "up to", "upon", "with", "within", "without", "across", "along",
    "by", "of", "in", "to", "near", "of", "from"]

public
isPreposition : String -> Bool
isPreposition w = isSomething w preposition

auxverbs : List String
auxverbs = ["will", "shall", "cannot", "may", "need to", "would", "should",
    "could", "might", "must", "ought", "ought to", "can't", "can"]

public
isAuxVerb : String -> Bool
isAuxVerb w = isSomething w auxverbs

toBeVerbs : List String
toBeVerbs = ["be", "being", "was", "were", "been", "are", "is"]

public
isToBeVerb : String -> Bool
isToBeVerb w = isSomething w toBeVerbs


public
isVowel : Char -> Bool
isVowel c = List.elem (toLower c) ['a', 'e', 'i', 'o', 'u', 'y']

public
countSyllables : String -> Int
countSyllables w = case length (ws) <= 2 of
    True => 1
    False => if isSuffixOf (unpack "ed") ws
               then countSyllables' (take (length ws - 2) ws)
               else let res = countSyllables' ws in
                        if res == 0
                          then 1
                          else res

  where
    ws : List Char
    ws = unpack w
    countSyllables' : List Char -> Int
    countSyllables' []         = 0
    countSyllables' (x::y::xs) = if (isVowel x) && not (isVowel y)
                                  then 1 + countSyllables' xs
                                  else countSyllables' (y::xs)
    countSyllables' x          = 0

-- --------------------------------------------------------------------- [ EOF ]
