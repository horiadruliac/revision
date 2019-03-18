module Lib
    ( someFunc, getRandLine, makeEven, determineYes, delFrom, addTo, removeHeads,
    printFancy, enterFile
    ) where

import System.IO
import Control.Monad
import System.Random
import Data.Char
import System.Directory

someFunc :: IO ()
someFunc = do
    filesAvailablePlus <- getDirectoryContents "resources"
    let filesAvailable = removeHeads filesAvailablePlus
    if (length filesAvailable /= 0)
        then
            do
                putStrLn "In directory resources, you have the following files available: "
                printFancy filesAvailable
                fileChosen <- enterFile filesAvailable
                contents <- readFile $ "resources/" ++ fileChosen
                let linesOfFiles = lines contents
                let initialOverFlow = []
                getRandLine linesOfFiles initialOverFlow
                putStrLn "Go again? "
                response <- getLine
                case (determineYes response) of
                    True -> someFunc
                    False -> return ()
        else
            do
                workingDir <- getCurrentDirectory
                putStrLn $ "No flashcard files available! Make sure to add files to " ++ workingDir ++ "/!"

getRandLine :: [String] -> [String] -> IO ()
getRandLine [] [] = return ()
getRandLine [] xs = getRandLine xs []
getRandLine strArr overflow = do
    i <- randomRIO (0, length strArr - 1)
    let questionIndex = makeEven i
    putStrLn $ strArr !! questionIndex
    firstResponse <- getLine
    putStrLn $ strArr !! (questionIndex + 1)
    putStr "Did you get this correct? "
    yesOrNo <- getLine
    case (determineYes yesOrNo) of
        True -> getRandLine (delFrom questionIndex strArr) overflow
        False -> getRandLine (delFrom questionIndex strArr) (addTo questionIndex strArr overflow)

makeEven :: Int -> Int
makeEven x = case (even x) of
    True -> x
    False -> x - 1

determineYes :: String -> Bool
determineYes str | map toLower str == "yes" = True
                 | otherwise = False

delFrom :: Int -> [String] -> [String]
delFrom x xs = l ++ r where
    (l, (_:_:r)) = splitAt x xs

addTo :: Int -> [String] -> [String] -> [String]
addTo x xs ys = f:s:ys where
    (_, (f:s:_)) = splitAt x xs

removeHeads :: [a] -> [a]
removeHeads (x:y:xs) = xs

printFancy :: [String] -> IO ()
printFancy [x] = do
    putStrLn x
printFancy (x:xs) = do
    putStr $ x ++ ", "
    printFancy xs

enterFile :: [String] -> IO String
enterFile available = do
    putStr "Which file would you like to choose? "
    response <- getLine
    if (elem response available)
        then
            return response
        else
            do
                putStrLn "This isn't a file! Please enter a valid file."
                enterFile available
