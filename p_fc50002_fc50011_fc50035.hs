import System.Environment   
import Data.List  
import Data.Map
import PpmFile
import ModifyImage
import Test.QuickCheck

{-
Projeto de Principios de Programacao

Feito por alunos:
Alexandre Nascimento, Nº50002
Ana Claudia Alferes, Nº50035
Rodrigo Alcarva, Nº50011
-}


main = do
    args <- getArgs
    if length args > 1
        then do
            (fileToRead : fileToWrite : flags) <- getArgs
            content <- readFile fileToRead
            let ppmFile = ppmFromFile (lines content) 0 0 0 0 []
            writeFile fileToWrite (show $ readFlags ppmFile flags)
        else
            runTests

-- Esta funcao recebe uma instancia PpmFile e aplica as flags indicadas pelo utilizador e devolve uma nova instancia
readFlags :: PpmFile -> [String] -> PpmFile 
readFlags ppmFile [] = ppmFile
readFlags ppmFile (x:xs)
    | x == "-fh" = readFlags (invertHorizontal ppmFile) xs
    | x == "-fv" = readFlags (invertVertical ppmFile) xs
    | x == "-hh" = readFlags (halfHeight ppmFile) xs
    | x == "-hw" = readFlags (halfWidth ppmFile) xs
    | x == "-gs" = readFlags (onlyGrey ppmFile) xs
    | x == "-rc" = readFlags (onlyRed ppmFile) xs
    | x == "-bc" = readFlags (onlyBlue ppmFile) xs
    | x == "-gc" = readFlags (onlyGreen ppmFile) xs

runTests = do
    quickCheck prop_invert_twice
    quickCheck prop_respect_max_value
    quickCheck prop_pixel_num
    