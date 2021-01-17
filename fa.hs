--Finite Automota
module Fa(
-- Data type for finite automita
FA(..),
-- Functions
kleene,
kleene_str,
makeFA,
-- Exported vars
binStrings,
even0,
even1
) where

--standard libraries
import Data.List
import Data.String
import Control.Monad

--importing utils to be able to use in ghci
import Utils

--Kleene star set of an alphabet, doesnt work for lists of strings
kleene :: [a] -> [[a]]
kleene al = []:[ (x:ys) | ys <- kleene al, x <- al]

--Kleene star that works on strings 
kleene_str :: [String] -> [String]
kleene_str al = []:[ (x++ys)  | ys <- kleene_str al, x <- al]

--Kleene star of all binary strings
binStrings = kleene "01"

--Model of DFA/NFA
data FA q s = DFA {
                        start :: q,
                        accept :: [q],
                        trans :: [(q, s, q)]
                   }
             | NFA { 
                        start :: q,
                        accept :: [q],
                        trans' :: [(q, s, [q])]
                    } deriving (Show)

--finds where we transition to when in state 'q' and encounter symbol 's'
dfaMove :: (Eq q) => (Eq s) => [(q, s, q)] -> q -> s -> q
dfaMove t q s = head [ q2 | (q1, s1, q2) <- t, q1 == q, s1 == s]

--finds list of states we can transiton to from state 'q' when encountering symbol 's'
nfaMove:: (Eq q) => (Eq s) => [(q, s, [q])] -> q -> s -> [q]
nfaMove t q s = (concat [q2 | (q1, s1, q2) <- t, q1 == q, s1 == s])

--returns a *FA with the specified start, accept, and transition ready to test strings
makeFA :: (Eq q) => (Eq s) => FA q s -> [s] -> Bool
makeFA (DFA q0 f t) = (`elem` f) . foldl (dfaMove t) q0
makeFA (NFA q0 f t) = any (`elem` f) . foldM (nfaMove t) q0

--Basic machines
even0 = (DFA 1 [1] [(1, '0', 2), (1, '1', 1), (2, '0', 1), (2, '1', 2)])
even1 = (DFA 1 [1] [(1, '0', 1), (1, '1', 2), (2, '0', 2), (2, '1', 1)])

--Actions on the machines:
--Returns a maching that is the logical negation of the provided machine
notFA :: (Eq q) => (Eq s) => (FA q s) -> (FA q s)
notFA (DFA q0 f t) = (DFA q0 (nub [q | (q,s,q1) <- t, not (elem q f)]) t)

intersectFA ::(Eq q) => (Eq s) => (FA q s) -> (FA q s) -> (FA (q,q) s)
intersectFA (DFA q0 f t) (DFA q0' f' t') = (DFA (q0, q0') [(a,b) | a <- f, b <- f'] [( (q, q'), (s), (q1, q1')) | (q,s,q1) <- t, (q',s',q1') <- t', s == s'])
