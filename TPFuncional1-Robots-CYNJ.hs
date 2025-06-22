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

poder :: Robot -> Int
poder robot = cantidadEnergia robot + nivelExperiencia robot * length (programas robot)