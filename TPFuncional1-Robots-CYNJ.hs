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