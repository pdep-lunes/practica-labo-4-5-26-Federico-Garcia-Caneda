data Perros = unPerro {
                        raza :: String
                        juegos :: [String]
                        tiempo :: Int
                        energia :: Int
}

jugar :: Perros -> Perros
jugar unPerro = unPerro {energia = modificarEnergia unPerro 10}

modificarEnergia :: Perros -> Int -> Perros
modificarEnergia unPerro unaCantidad = unPerro {energia = max(energia unPerro + unaCantidad) 0}


ladrar :: Int -> Perros -> Perros
ladrar unaCantidad unPerro = unPerro {energia = modificarEnergia unPerro (unaCantidad * energiaGanadaPorLadrido(unPerro unaCantidad))}

energiaGanadaPorLadrido :: Perros -> Int -> Perros
cantidadLadridos unPerro unaCantidad = {energia = energia unPerro + unaCantidad / 2}

regalar :: String -> Perro -> Perros
regalar unJuguete unPerro = unPerro{(: unJuguete) . juegos & unPerro}


diaDeSpa :: Perros -> Perros
| unPerro{energia unPerro} > 50 || esDeRazaExtravagante unPerro = unPerro{energia = energia unPerro + 10}
| otherwise = unPerro

esDeRazaExtravagante :: Perros -> Bool
| raza unPerro == "Dalmata" = True
| raza unPerro == "Pomerania" = True
| otherwise = False

diaDeCampo :: Perros -> Perros
unPerro = jugar unPerro
| length juegos unPerro > 0 = unPerro{juegos = drop(1 juegos unPerro)}
| otherwise = unPerro

Zara :: Perros
Zara = unPerro "Dalmata" ["Pelota", "Mantita"] 90 80

type Tiempo = Int
type Actividad = (Perros -> Perros, Tiempo)
data Guarderia = UnaGuarderia{
                            actividades :: [Actividad]
}

PdePerritos :: Guarderia
PdePerritos = unaGuarderia {actividades = [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 20), (diaDeSpa, 120), (diaDeCampo, 720)]}

perroPuedeEstar :: Perros -> Guarderia -> Bool
perroPuedeEstar unPerro unaGuarderia = tiempo unPerro <= sum . map snd . actividades $ unaGuarderia

perroResponsable :: Perros -> Bool
perroResponsable unPerro = (>3) . length . juegos . diaDeCampo & unPerro
