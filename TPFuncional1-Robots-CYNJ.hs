{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
type Programa = Robot -> Robot

data Robot = Robot {
  nombre :: String,
  nivelExperiencia :: Int,
  cantidadEnergia :: Int,
  programas :: [Programa]
}

recargarBateria :: Int -> Robot -> Robot
recargarBateria cantidad robot = robot{
    cantidadEnergia = cantidadEnergia robot + cantidad}

descargaElectrica::Robot->Robot
descargaElectrica robot 
                |cantidadEnergia robot > 10= robot{
                    cantidadEnergia = cantidadEnergia robot - 10
                    }
                |otherwise = robot{
                    cantidadEnergia = cantidadEnergia robot `div` 2
                }

olvidarProgramas::Int->Robot->Robot
olvidarProgramas n robot= robot{
    programas= drop n (programas robot)
}


autoAtaque::Robot->Robot
autoAtaque robot
            | null(programas robot) = error "El robot no tiene progrmas para atacarse"
            |otherwise= head (programas robot) robot

poder :: Robot -> Int
poder robot = cantidadEnergia robot + nivelExperiencia robot * length (programas robot)

danio :: Robot -> Programa -> Int
danio robot programa = cantidadEnergia robot - cantidadEnergia (programas robot)

diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs(poder robot1 - poder robot2)

type Academia = [Robot]

existeRobot::String->Academia->Bool
existeRobot nombreBuscado academia = any(\robot -> nombre robot == nombreBuscado && null(programas robot)) academia

losViejosSonObstinados :: Academia -> Bool
losViejosSonObstinados = all (\robot -> nivelExperiencia robot <= 16 && length(programas robot) > 3 * nivelExperiencia robot)


{-
f x [y] = y
f x (y1:y2:ys)
      | x y1 >= x y2 = f x (y1:ys)
      | otherwise = f x (y2 : ys)

Tipo:
f::f :: Ord b => (a -> b) -> [a] -> a
La función f recibe dos parametros, uno de los parametros es una función que transforma 
un valor de tipo a en otro de tipo b, donde b es ordenable (Ord b) y el otro parametro es y una lista de elementos de tipo a.
Su objetivo es recorrer la lista y devolver el elemento que tenga el mayor valor según el criterio de la funcion 

Mejora en terminos de expresividad:
maximo :: Ord b => (a -> b) -> [a] -> a
maximo _ [elemento] = elemento
maximo funcion (primero:segundo:resto)
  | funcion primero >= funcion segundo = maximo funcion (primero : resto)
  | otherwise     = maximo funcion (segundo : resto)
Versión más expresiva (sin recursivadad)
maximo :: Ord b => (a -> b) -> [a] -> a
maximo funcion  = foldl1 (\a b -> if criterio a >= funcion b then a else b)
Esta versión utiliza la función de orden superior foldl1 
para acumular el mejor elemento de la lista, comparando siempre dos elementos a la vez, -}


mejorProgramaContra :: Robot -> Robot -> Programa
mejorProgramaContra robotOponente robot =
  foldl1 (\programa1 programa2 ->
    if danio robotOponente programa1 >= danio robotOponente programa2
      then programa1
      else programa2
  ) (programas robot)

mejorOponente :: Robot -> Academia -> Robot
mejorOponente robot academia =
    foldl1(\robot1 robot2->
        if diferenciaDePoder robot robot1 >= diferenciaDePoder robot robot2
            then robot1
            else robot2
        ) academia


noPuedeDerrotarle :: Robot -> Robot -> Bool
noPuedeDerrotarle atacante defensor = cantidadEnergia (foldr (\programa robot -> programa robot) defensor (programas atacante)) >= cantidadEnergia defensor