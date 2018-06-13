import Data.List
import Text.Show.Functions

-- Primera Parte________________________________________________________________________________________________________

--defino mi tipo de dato cliente.
data Persona = Cliente {
                        nombre::String,
                        resistencia::Int,
                        amigos::[Persona],
                        bebidas::[Bebida] }

data Bebida =
  GrogXD
  | JarraLoca
  | Klusener {sabor::String}
  | Tintico
  | Soda {fuerza::Int}
  | JarraPopular {espirituosidad::Int}

type Accion = (Persona->Persona)

instance Show Persona where
  show cliente =
    nombre cliente
    ++ " tomó: "
    ++ show (bebidas cliente)
    ++ ", resistencia "
    ++ show (resistencia cliente)
    ++ ", amigos: "
    ++ show (map nombre (amigos cliente))
    ++ " resistencias respectivas "
    ++ show (map resistencia (amigos cliente))

instance Show Bebida where
    show GrogXD = "GrogXD"
    show JarraLoca = "Jarra Loca"
    show (Klusener sabor) = ("Klusener de " ++ sabor)
    show Tintico = "Tintico"
    show (Soda fuerza) = ("Soda de fuerza " ++ show(fuerza))
    show (JarraPopular espirituosidad) =
         ("Jarra popular de espirituosidad " ++ show(espirituosidad))

rodri = Cliente "Rodri" 55 [] [Tintico]
marcos = Cliente "Marcos" 40 [rodri] [(Klusener "guinda")]
cristian = Cliente "Cristian" 2 [] [GrogXD,JarraLoca]
ana = Cliente "Ana" 120 [marcos,rodri] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []


quitaDiez:: Persona->Persona
quitaDiez (Cliente nombre resitencia amigos bebidas)= (Cliente nombre (subtract 10 resitencia) amigos (JarraLoca:bebidas))

generoR:: Int->String
generoR n
          |n>1  = "r" ++ (generoR (subtract 1 n))
          |n == 0 = ""
          |otherwise = "r"

rescatarse:: Int->Persona->Persona
rescatarse horas (Cliente nombre resistencia amigos bebidas)
                          | horas > 3 = (Cliente nombre (resistencia + 200) amigos bebidas)
                          |otherwise = (Cliente nombre (resistencia + 100) amigos bebidas)

comoEsta:: Persona->String
comoEsta (Cliente nombre resistencia amigos bebidas)
                                  | resistencia > 50 = "Esta Fresco"
                                  | length(amigos)> 1 = "Esta Piola"
                                  | otherwise = "Esta Duro"

miembroAmigos:: String->[Persona]->Bool
miembroAmigos x (cliente:lista) | (null lista) = False
                                | (nombre cliente) == x = True
                                | otherwise = miembroAmigos x lista

obtenerNombres listaClientes
                            | null listaClientes = []
                            | otherwise = ((nombre (head listaClientes)):obtenerNombres (tail listaClientes))


addFriend:: Persona->Persona->Persona
addFriend nuevo_amigo cliente
                          | ((nombre cliente) == (nombre nuevo_amigo)) = cliente --veo si no soy yo
                          | (length (amigos cliente)) == 0 = (Cliente (nombre cliente) (resistencia cliente) [nuevo_amigo] (bebidas cliente))
                          |  elem (nombre nuevo_amigo) (obtenerNombres (amigos cliente))= cliente --veo si es amigo mio | esta agregando el amigo aunque lo tenga, pero solo 1 vez, luego no lo agrega.
                          | (not (elem (nombre nuevo_amigo) ((obtenerNombres) (amigos cliente) ) ))= (Cliente (nombre cliente) (resistencia cliente) (nuevo_amigo:(amigos cliente)) (bebidas cliente))



tomar GrogXD (Cliente nombre _ amigos bebidas) = Cliente nombre 0 amigos (GrogXD:bebidas)
tomar JarraLoca (Cliente nombre resistencia amigos bebidas) = quitaDiez (Cliente nombre resistencia (map quitaDiez amigos) bebidas)
tomar (Klusener gusto) (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (subtract (length gusto) resistencia) amigos ((Klusener gusto):bebidas))
tomar Tintico (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (resistencia + ((length amigos)*5)) amigos (Tintico:bebidas))
tomar (Soda fuerza) (Cliente nombre resistencia amigos bebidas) = (Cliente ((("e"++(generoR fuerza))++"p") ++ nombre) resistencia amigos ((Soda fuerza):bebidas))
tomar (JarraPopular espirituosidad) cliente
                              | espirituosidad == 0 = cliente
                              | not (null (amigos cliente)) = tomar (JarraPopular (subtract 1 espirituosidad)) (foldl (flip addFriend) cliente  (unificarLista (formarListaAmigosDeMisAmigos cliente)))
                              | otherwise = cliente


--Segunda Parte____________________________________________________________________________________________________________________

--Funcion que hace a un cliente beber un trago.
beber:: Persona->Bebida->Persona
beber cliente bebida = tomar bebida cliente

--Funcion que hace tomar una lista de tragos a un cliente.
tomarTragos::Persona->[Bebida]->Persona
tomarTragos cliente listaTragos
                                |(not (null listaTragos))= tomarTragos (beber cliente (head listaTragos)) (tail listaTragos)
                                | null listaTragos = cliente

--Funcion que hace tomar el ultimo trago tomado a un cliente.
dameOtro:: Persona->Persona
dameOtro cliente = beber cliente (head (bebidas cliente))

--Funcion que define si un cliente puede tomar una bebida sin quedarse con 0 puntos de resistencia.
puedoTomar:: Persona->Bebida->Bool
puedoTomar cliente trago  | (resistencia (tomar trago cliente)) >0 = True
                          | otherwise = False

--Funcion que define si se puede tomar una bebida y llama a otra funcion para tomar el resto de las bebidas dada una lista de tragos.
calcularTragos:: Persona->[Bebida]->[Bebida]
calcularTragos cliente tragos | puedoTomar cliente (head tragos) = ((head tragos):cualesPuedeTomar cliente (tail tragos))
                              | otherwise = (cualesPuedeTomar cliente (tail tragos))

--Funcion que define cuales tragos puede tomar un cliente dada una lista de tragos.
cualesPuedeTomar:: Persona->[Bebida]->[Bebida]
cualesPuedeTomar cliente tragos | null tragos = []
                                | not(null tragos) = calcularTragos cliente tragos
                                | not(puedoTomar cliente (head tragos)) =  []
--Funcion que define la cantidad de tragos que puede tomar dada una lista de tragos
cuantasPuedeTomar:: Num a => Persona->[Bebida]->a
cuantasPuedeTomar cliente tragos = genericLength (cualesPuedeTomar cliente tragos)
--Defino el tipo de dato itinerario

data Itinerario = Itinerario { nombreItinerario::String,
                               duracion::Float,
                               acciones::[Accion] }

instance Show Itinerario where
  show itinerario = "Itinerario: "
                    ++ show (nombreItinerario itinerario)
                    ++ " Tiene una duración de "
                    ++ show (duracion itinerario)
                    ++ " horas, y sus acciones son: "
                    ++ show (acciones itinerario)

--Defino los 3 itinerrarios mezclaExplosiva,itinerarioBasico y salidaDeAmigos.
mezclaExplosiva = (Itinerario "mezcla Explosiva" 2.5 [(tomar GrogXD),(tomar GrogXD),(tomar (Klusener "huevo")),(tomar (Klusener "frutilla"))])
itinerarioBasico = (Itinerario "basico" 5 [(tomar JarraLoca),(tomar (Klusener "chocolate")), (rescatarse 2), (tomar (Klusener "huevo"))])
salidaDeAmigos = (Itinerario "salida de amigos" 1 [(tomar (Soda 1)),(tomar Tintico),(addFriend robertoCarlos),(tomar JarraLoca)])


--hacerItinerario cliente itinerario = tomarTragos cliente (acciones itinerario)
--hacerItinerario:: Persona->Itinerario->Persona
hacerItinerario cliente itinerario = hacerAcciones cliente (acciones itinerario)
--hacerAcciones:: Persona->[Accion]->Persona
hacerAcciones cliente acciones
                            | null acciones = cliente
                            | not (null acciones) = hacerAcciones ((head acciones) cliente) (tail acciones)


intensidad itinerario = genericLength (acciones itinerario) / (duracion itinerario)
--intensidad itinerario = fromRational ((toRational ( length (acciones itinerario)) / realToFrac(duracion itinerario)))


obtenerItinerarioMasIntenso itinerarioMasIntensoViejo itinerariosRestantes
                              | null itinerariosRestantes  = itinerarioMasIntensoViejo
                              | (intensidad itinerarioMasIntensoViejo) < (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso (head itinerariosRestantes) (tail itinerariosRestantes)
                              | (intensidad itinerarioMasIntensoViejo) >= (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso itinerarioMasIntensoViejo (tail itinerariosRestantes)
--Funcion que obtiene el itinerario mas intenso a partir de una lista de itinerarios
--Precondicion que la lista no sea vacia.
itinerarioMasIntenso listaItinerarios
                              | not (null listaItinerarios)= obtenerItinerarioMasIntenso (head listaItinerarios) (tail listaItinerarios)

hacerItinerarioMasIntenso cliente listaItinerarios = hacerItinerario cliente (itinerarioMasIntenso listaItinerarios)


unificarLista:: [[Persona]]->[Persona]
unificarLista listaDeListas
                      | not (null (tail listaDeListas)) = ((head listaDeListas) ++ unificarLista (tail listaDeListas))
                      |otherwise = (head listaDeListas)


formarListaRecursivo clientes
                          | not (null clientes) =  (amigos  (head clientes)): formarListaRecursivo (tail clientes)
                          | otherwise = []

formarListaAmigosDeMisAmigos cliente = formarListaRecursivo (amigos cliente)








--ARREGLAR JARRA POPULAR____

-- TEST OBJETIVO 1D (5) NO DA BIEN

-- TEST OBJETIVO 4A (3) NO DA 0.8, DA 1.4

-- TEST OBJETIVO 5B | 5C FALLAN NO ANDA BIEN JARRAPOPULAR
