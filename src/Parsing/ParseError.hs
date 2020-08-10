module Parsing.ParseError ( ParseError(..) ) where

import Parsing.Position (Position)
import qualified Parsing.Position as Pos

data ParseError = ParseError {
    message :: String,
    currentLine :: String,
    currentPos :: Position
}

instance Show ParseError where
    show e = case (currentLine e) of
        [] -> errorMessage
        l -> mkError l
      where
        mkError l = errorMessage <> "\n"
                 <> extract contextFrom contextTo l <> "\n"
                 <> replicate contextIndent ' ' <> "^" 
          where
            contextFrom = max 0 (colIndex - contextSize)
            contextTo = colIndex + contextSize
            contextIndent = if colIndex > contextSize
                            then contextSize + 2
                            else colIndex

        errorMessage = "Parsing error on line " <> show (lineIndex + 1) <> ", col " <> show (colIndex + 1) <> ": " <> message e
        
        extract :: Int -> Int -> String -> String
        extract _ _ [] = []
        extract from to (x:xs)
            | from > to = []
            | to < 0 = []
            | to == 0 = "..."
            | max 0 from > 3 = extract (from - 1) (to - 1) xs
            | max 0 from > 0 = '.' : (extract (from - 1) (to - 1) xs)
            | otherwise = x : (extract 0 (to - 1) xs) 
        
        contextSize = 10
        contextFrom = colIndex - contextSize
        contextTo = colIndex + contextSize

        lineIndex :: Int
        lineIndex = Pos.line $ currentPos e

        colIndex :: Int
        colIndex = Pos.col $ currentPos e
