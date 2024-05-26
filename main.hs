module Main where

determinarYear :: Int -> String
determinarYear id
  | stringYear == "241" = "2024-1"
  | stringYear == "242" = "2024-2"
  | stringYear == "251" = "2025-1"
  | stringYear == "252" = "2025-2"
  | stringYear == "261" = "2026-1"
  | stringYear == "262" = "2026-2"
  | otherwise = ""
  where stringYear = take 3 (show id)

-- Función para determinar el tipo de programa 
tipoPrograma :: Int -> String 
tipoPrograma programa 
  | esAbundante programa = "Administrative"
  | esPerfecto programa = "Engineering"
  | esDeficiente programa = "Humanities"
  | otherwise = ""

-- Tomar cuarto y quinto elemento del entero
cuartoYQuinto :: Int -> Int
cuartoYQuinto n
  | length str == 8 = (read [str !! 3] :: Int) * 10 + (read [str !! 4] :: Int)
  | length str <= 8 = 0 + (read [str !! 4] :: Int)
  where
    str = show n

-- Divisores
sumaDivisores :: Int -> Int
sumaDivisores programa = sum [x | x <- [1..programa-1], programa `mod` x == 0]

-- Es abundante 
esAbundante :: Int -> Bool
esAbundante programa = sumaDivisores programa > programa

-- Es perfecto
esPerfecto :: Int -> Bool
esPerfecto programa = sumaDivisores programa == programa

-- Es deficiente 
esDeficiente :: Int -> Bool
esDeficiente programa = sumaDivisores programa < programa

-- Últimos tres números
ultimosTres :: Int -> Int
ultimosTres n
  | length str >= 3 = read (drop (length str - 3) str) :: Int
  | otherwise       = error ""
  where
    str = show n

-- Determinar si un número es par o impar
esPar :: Int -> Bool
esPar = even

-- Mostrar información del número
mostrarNumeros :: Int -> String
mostrarNumeros n = "num" ++ show n ++ " " ++ if esPar n then "even" else "odd"

main :: IO ()
main = do 
  id <- readLn :: IO Int
  putStrLn $ determinarYear id ++ " " ++ tipoPrograma (sumaDivisores (cuartoYQuinto id)) ++ " " ++ mostrarNumeros (ultimosTres id)

