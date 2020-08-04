module ModifyImage (
    invertVertical,
    invertHorizontal,
    halfWidth,
    halfHeight,
    onlyGrey,
    onlyRed,
    onlyGreen,
    onlyBlue,
    prop_invert_twice,
    prop_respect_max_value,
    prop_pixel_num
) where
    
import Data.List
import PpmFile
import Test.QuickCheck

-- Esta função recebe uma instancia PpmFile e ira inverter verticalmente o mesmo
invertVertical :: PpmFile -> PpmFile
invertVertical (F (width, height) maxValue pixels) = newPpm width height maxValue (reverse pixels) 

-- Esta função recebe uma instancia PpmFile e ira inverter horinzontalmente o mesmo
invertHorizontal :: PpmFile -> PpmFile
invertHorizontal (F (width, height) maxValue pixels) = newPpm width height maxValue (map reverse pixels) 

-- Esta funcao recebe uma instancia PpmFile e ira passar a largura para metade do tamanho original
halfWidth :: PpmFile -> PpmFile
halfWidth (F (width, height) maxValue pixels) = newPpm (width `div` 2) height maxValue (map (\p -> halfWidthfAux p []) pixels)

-- Esta funcao recebe uma lista de pixeis e junta os pixeis pela media e auxilia a funcao halfWidth
halfWidthfAux :: [Pixel] -> [Pixel] -> [Pixel]
halfWidthfAux [] halfPixels = halfPixels
halfWidthfAux ((P r1 g1 b1):[]) halfPixels = halfPixels
halfWidthfAux ((P r1 g1 b1):(P r2 g2 b2):xs) halfPixels = halfWidthfAux xs (halfPixels ++ [(P ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((g1 + g2) `div` 2))])

-- half heigth

-- Esta funcao recebe uma instancia PpmFile e ira passar a altura para metade do tamanho original
halfHeight :: PpmFile -> PpmFile
halfHeight (F (width, height) maxValue pixels) = newPpm width (height `div` 2) maxValue (halfHeighthAux pixels [])

-- Esta funcao recebe uma lista de pixeis e junta os pixeis pela media e auxilia a funcao halfHeight
halfHeighthAux :: [[Pixel]] -> [[Pixel]] -> [[Pixel]]
halfHeighthAux [] halfPixels = halfPixels
halfHeighthAux (l1:l2:xs) halfPixels = halfHeighthAux xs (halfPixels ++ [(linesAverage l1 l2 [])])

-- Esta funcao percorre as linhas todas e faz a media dos elementos correspondentes em duas listas (elementos correspondentes ou elementos com o mesmo index na lista)
linesAverage :: [Pixel] -> [Pixel] -> [Pixel] -> [Pixel]
linesAverage [] [] joinedLine = joinedLine
linesAverage ((P r1 g1 b1):xs1) ((P r2 g2 b2):xs2) joinedLine = linesAverage xs1 xs2 (joinedLine ++ [(P ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((g1 + g2) `div` 2))])

-- color to grey

-- Esta funcao recebe uma instancia PpmFile onde com o auxilio da funcao changeColorAux irá mudar a cor da imagem para cinzento
onlyGrey :: PpmFile -> PpmFile
onlyGrey (F (width, height) maxValue pixels) = newPpm width height maxValue (map (\p -> onlyGreyAux p []) pixels)

-- Esta funcao aplica um algoritmo para mudar a cor para grey scale
onlyGreyAux :: [Pixel] -> [Pixel] -> [Pixel]
onlyGreyAux [] finalList = finalList
onlyGreyAux ((P r g b):xs) finalList = onlyGreyAux xs (finalList ++ [(P ((r + g + b) `div` 3) ((r + g + b) `div` 3) ((r + g + b) `div` 3))])

-- change color

-- Esta funcao recebe uma instancia PpmFile onde com o auxilio da funcao changeColorAux irá mudar a cor da imagem para vermelho
onlyRed :: PpmFile -> PpmFile
onlyRed (F (width, height) maxValue pixels) = newPpm width height maxValue (map (\p -> changeColorAux p "red" []) pixels)

-- Esta funcao recebe uma instancia PpmFile onde com o auxilio da funcao changeColorAux irá mudar a cor da imagem para verde
onlyGreen :: PpmFile -> PpmFile
onlyGreen (F (width, height) maxValue pixels) = newPpm width height maxValue (map (\p -> changeColorAux p "green" []) pixels)

-- Esta funcao recebe uma instancia PpmFile onde com o auxilio da funcao changeColorAux irá mudar a cor da imagem para azul
onlyBlue :: PpmFile -> PpmFile
onlyBlue (F (width, height) maxValue pixels) = newPpm width height maxValue (map (\p -> changeColorAux p "blue" []) pixels)

-- Esta funcao recebe uma lista de pixeis, uma string e uma lista que acabara por returna-la, consoante a string vai-se mudar os valores do pixeis
changeColorAux :: [Pixel] -> String -> [Pixel] -> [Pixel]
changeColorAux [] _ finalList = finalList
changeColorAux ((P r g b):xs) color finalList
    | color == "red" = changeColorAux xs color (finalList ++ [(P r 0 0)])
    | color == "green" = changeColorAux xs color (finalList ++ [(P 0 g 0)])
    | otherwise = changeColorAux xs color (finalList ++ [(P 0 0 b)])

-- Tests  
instance Arbitrary PpmFile where
    arbitrary = do
        height <- choose (1, 512)
        width <- choose (1, 512)
        maxValue <- choose (512, 1000)
        pixels <- vectorOf height (vectorOf width arbitrary)
        return $ F (width, height) maxValue pixels

instance Arbitrary Pixel where
    arbitrary = do
        n1 <- choose (1, 512)
        n2 <- choose (1, 512)
        n3 <- choose (1, 512)
        return $ P n1 n2 n3

prop_invert_twice ppmFile = invertVertical (invertVertical ppmFile) == ppmFile

prop_respect_max_value (F (width, height) maxValue pixels) = length (filter (\(P r g b) -> r > maxValue && g > maxValue && b > maxValue) (concat pixels)) == 0

prop_pixel_num (F (width, height) maxValue pixels) = length (concat pixels) ==  width * height
