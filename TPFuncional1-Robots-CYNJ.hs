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
danio robot programa = cantidadEnergia robot - cantidadEnergia (programa robot)

diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs(poder robot1 - poder robot2)

type Academia = [Robot]

existeRobot::String->Academia->Bool
existeRobot nombreBuscado academia = any(\robot -> nombre robot == nombreBuscado && null(programas robot)) academia

losViejosSonObstinados :: Academia -> Bool
losViejosSonObstinados = all (\robot -> nivelExperiencia robot <= 16 && length(programas robot) > 3 * nivelExperiencia robot)


-- TIPO: 
f:: Ord a => a -> [a] -> a
--FORMA MAS EXPRESIVA: 
f x [y] = y
f x (y1:y2:ys) = if x > y1 && x > y2 then y2 else (y2 : ys)
--PROPOSITO y EJEMPLO:
--La función f recibe una numero y una lista, 
--y devuelve el elemento de la lista (si la lista tiene un solo elemento); si la 
--lista tiene al menos 2 elementos,compara el numero con los 2 primeros elementos de la misma;
--si el numero es mayor a y1 y y2, devuelve y2
--de lo contrario, devuelve la cola de la lista a partir de y2.
{-ej:
f 5 [2,1,8,9]  -> 5>2 and 5>1 (siiiiiiii)
f 4 [7,6,2,1]  -> 4>7 and 4>6 (NOOOO!)
	           -> 4>6 and 4>2 (noooooooo!)
               -> 4>2 and 4>1 (siiiiiiiiiii.!!)
-}

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