module PpmFile (
    PpmFile(..),
    Pixel(..),
    newPpm,
    ppmFromFile
) where

import Data.List

data PpmFile = F (Int, Int) Int [[Pixel]] deriving (Eq)
data Pixel = P Int Int Int deriving (Eq)

-- Esta funcao cria uma nova funcao do tipo PpmFile
newPpm :: Int -> Int -> Int -> [[Pixel]] -> PpmFile
newPpm width height maxValue pixels = F (width, height) maxValue pixels

-- Esta funcao recebe uma lista de strings , 4 ints e uma lista vazia e preenche a lista vazia com pixeis e chama a funcao para criar uma nova instancia
ppmFromFile :: [String] -> Int -> Int -> Int -> Int -> [[Pixel]] -> PpmFile
ppmFromFile [] _ width height maxValue pixels = newPpm width height maxValue pixels
ppmFromFile (x:xs) lineNum width height maxValue pixels
    | lineNum == 0 || x !! 0 == '#' = ppmFromFile xs nextLine width height maxValue pixels
    | lineNum == 1 = ppmFromFile xs nextLine (read $ lineList !! 0) (read $ lineList !! 1) maxValue pixels
    | lineNum == 2 = ppmFromFile xs nextLine width height (read x) pixels
    | otherwise = newPpm width height maxValue (getPixels (x:xs) 0 width [] [])
        where 
            lineList = words x
            nextLine = lineNum + 1

-- Esta funcao ira juntar todos os pixeis numa lista de listas, listas estas que representam uma linha de pixeis
getPixels :: [String] -> Int -> Int -> [Pixel] -> [[Pixel]]-> [[Pixel]]
getPixels (x:[]) _ maxPixelcount pixels finalPixels = finalPixels ++ [pixels ++ (getPixelsAux (words x) [])]
getPixels (x:xs) pixelCount maxPixelcount pixels finalPixels
    | pixelCount < maxPixelcount = getPixels xs (pixelCount + l) maxPixelcount pixelLine finalPixels
    | otherwise = getPixels (x:xs) 0 maxPixelcount (drop maxPixelcount pixels) (finalPixels ++ [(take maxPixelcount pixels)]) 
        where 
            lineList = words x
            pixelLine = pixels ++ (getPixelsAux lineList [])
            l = (length (getPixelsAux lineList []))

-- Esta funcao é uma funcao auxilar da funcao getPixels e irá juntar os Pixeis a uma lista de pixeis
getPixelsAux :: [String] -> [Pixel] -> [Pixel] 
getPixelsAux [] pixelsList = pixelsList
getPixelsAux (r:g:b:rest) pixelsList = getPixelsAux rest (pixelsList ++ [P (read r) (read g) (read b)]) 

-- Esta funcao recebe uma lista de listas de Pixeis e retorna os pixeis em formato String
showPixelList :: [[Pixel]] -> String -> String
showPixelList (x:[]) finalList = finalList ++ (concatMap show x)
showPixelList (x:xs) finalList = showPixelList xs (finalList ++ (concatMap show x) ++ "\n")


instance Show PpmFile where 
    show (F (width, height) maxValue pixels) = "P3 \n" ++ (show width) ++ " " ++ (show height) ++ "\n" ++ (show maxValue) ++ "\n" ++ (showPixelList pixels [])

instance Show Pixel where 
   show (P r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " 