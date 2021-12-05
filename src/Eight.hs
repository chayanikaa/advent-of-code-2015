{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Eight
    (
        part1,
        calculateParsedLength,
        part2,
    ) where

decode :: String -> String
decode ('\\':'\\':xs)    = ('\\':decode xs)
decode ('\\':'"':xs)     = ('"':decode xs)
decode ('\\':'x':x:y:xs) = ('!':decode xs)
decode (x:xs)            = (x:decode xs)
decode []                = []

encode :: String -> String
encode ('\\':xs)    = ("\\\\" ++ encode xs)
encode ('"':xs)     = ("\\\"" ++ encode xs)
encode (x:xs)            = (x:encode xs)
encode []                = []


calculateParsedLength :: String -> Int
calculateParsedLength = length . decode . tail . init

calculateDiff1 :: String -> Int
calculateDiff1 str = length str - calculateParsedLength str

calculateDiff2 :: String -> Int
calculateDiff2 str = (length . encode) str - length str + 2 -- + 2 to escape the surrounding quotes

part1 :: [String] -> Int
part1 = sum . map calculateDiff1

part2 :: [String] -> Int
part2 = sum . map calculateDiff2
