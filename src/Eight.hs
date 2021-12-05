{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Eight
    (
        part1,
        calculateParsedLength,
    ) where

decode :: String -> String
decode = f
    where f ('\\':'\\':xs)    = ('\\':decode xs)
          f ('\\':'"':xs)     = ('"':decode xs)
          f ('\\':'x':x:y:xs) = ('!':decode xs)
          f (x:xs)            = (x:decode xs)
          f []                = []


calculateParsedLength :: String -> Int
calculateParsedLength = length . decode . tail . init

calculateDiff :: String -> Int
calculateDiff str = length str - calculateParsedLength str

part1 :: [String] -> Int
part1 = sum . map calculateDiff
