-- Rabin Karp 
-- Algoritmul face cautari pe baza unei functii de hash
-- Algoritmul va returna indexul la care a fost gasit subsirul cautat

-- functia hash folosita : Se decide o baza anume, transfromare cu ajutorul codului ASCII sirul in baza respectiva
-- Baza aleasa pentru acest proiect este 101
-- Exemplu: Dacă subșirul este "hi" iar baza este 101, valoarea hash este 104 × 101^1 + 105 × 101^0 = 10609 
-- (codul ASCII al lui 'h' este 104 iar al lui 'i' este 105).
-- Beneficiul obținut de o astfel de reprezentare este că se poate calcula valoarea hash a următorului șir pe baza celui anterior
-- efectuând doar un număr constant de operații, independent de lungimea subșirului. (https://ro.wikipedia.org/wiki/Algoritmul_Rabin-Karp)

import Data.Char (ord, digitToInt)
import Data.Maybe (listToMaybe)
import Numeric (readInt)
import System.IO

-- Funtia mea de hash primeste ca si parametru un string
hash_function :: [Char] -> Int
hash_function [] = 0
hash_function xs = ord(head(xs)) + hash_function(tail xs) * 101 -- altfel, apelez codul ascii al caracterului fara prima litera
-- inmultit cu baza la puterea pozitiei caracterului

rabin_karp :: [Char] -> [Char] -> Int -- primul parametru : sirul in care caut, al doilea parametru sirul pe care il caut
rabin_karp [] [] = error "Null Input"
rabin_karp [] _ = -1 -- conditia de stop a cautarii -> cand sirul devine gol si nu mai am unde cauta
rabin_karp s sb = 
    let
        h_s = hash_function (reverse(take (length sb) s)) -- functia de hash pentru primele caractere de lungimea subsirului cautat
    in
        if h_sb == h_s then length s-- daca rezultatul acestor doua functii sunt egale, atunci programul returneaza True
        else rabin_karp (tail s) sb -- altfel, reiau cautarea pentru sirul in care caut, fara ultima litera si subsirul cautat
    where h_sb = hash_function (reverse sb) -- functia hash pt subsir

-- string_length :: [Char] -> Int
-- string_length x = length x

readAString :: IO String
readAString = readLn

main = do
    putStr "Enter String: "
    do
        input_string <- readAString                               
        do
            putStr "Enter Pattern: "
            input_pattern <- readAString
            if rabin_karp input_string input_pattern == -1 then
                putStrLn("Pattern Not Found!")
            else
                print((length input_string) - (rabin_karp input_string input_pattern))

--function RabinKarp(string s[1..n], string sub[1..m])
--    hsub := hash(sub[1..m])     
--    for i from 1 to n-m+1
--        if hs = hsub
--            if s[i..i+m-1] = sub
--                return i
--        hs := hash(s[i+1..i+m])
--    return not found
-- sursa: https://ro.wikipedia.org/wiki/Algoritmul_Rabin-Karp