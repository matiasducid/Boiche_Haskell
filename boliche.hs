import Data.List
import Text.Show.Functions

-- Primera Parte________________________________________________________________________________________________________

--defino mi tipo de dato cliente.
data Persona = Cliente {nombre::String, resistencia::Int, amigos::[Persona], bebidas::[Bebida]} deriving Show
                        -- agregar como dato a cliente.
type Bebida = (Persona->Persona)
type Accion = (Persona->Persona)

--instance Show Persona where
--  show rodri = (nombre rodri) ++ " tomó: " ++ show (bebidas rodri) ++ ", resistencia " ++ show (resistencia rodri) ++ ", amigos: " ++ show (map nombre (amigos rodri))
--  show cristian = (nombre cristian) ++ " tomó: " ++ show (bebidas cristian) ++ ", resistencia " ++ show (resistencia cristian) ++ ", amigos: " ++ show (map nombre (amigos cristian))
--  show ana = (nombre ana) ++ " tomó: " ++ show (bebidas ana) ++ ", resistencia " ++ show (resistencia ana) ++ ", amigos: " ++ show (map nombre (amigos ana))
--  show marcos = (nombre marcos) ++ " tomó: " ++ show (bebidas marcos) ++ ", resistencia " ++ show (resistencia marcos) ++ ", amigos: " ++ show (map nombre (amigos marcos))
--  show robertoCarlos = (nombre robertoCarlos) ++ " tomó: " ++ show (bebidas robertoCarlos) ++ ", resitencia " ++ show (resistencia robertoCarlos) ++ ", amigos: " ++ show (map nombre (amigos robertoCarlos))

-- No puedo simplificar el mostrar. de este modo no funciona

--data Bebida = grogXD | jarraLoca | klusener {sabor::String} | tintico | soda {fuerza::Int} | jarraPopular {espirituosidad::Int}
--instance Show Bebida where
--  show grogXD = "grogXD"
--  show jarraLoca = "Jarra Loca"
--  show tintico = "Tintico"
--  show (klusener sabor) = ("Klusener de "++ sabor)
--  show (soda fuerza) = ("Soda con fuerza de " ++ show(fuerza))
--  show (jarraPopular espirituosidad) = ("Jarra Popular de  espirituosidad " ++ show (espirituosidad))

rodri = Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 [rodri] [(klusener "guinda")]
cristian = Cliente "Cristian" 2 [] [grogXD,jarraLoca]
ana = Cliente "Ana" 120 [marcos,rodri] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []

grogXD:: Persona->Persona
grogXD (Cliente nombre resistencia amigos bebidas)= (Cliente nombre 0 amigos (grogXD:bebidas))

quitaDiez:: Persona->Persona
quitaDiez (Cliente nombre resitencia amigos bebidas)= (Cliente nombre (subtract 10 resitencia) amigos (jarraLoca:bebidas))
jarraLoca:: Persona->Persona
jarraLoca (Cliente nombre resistencia amigos bebidas) = quitaDiez (Cliente nombre resistencia (map quitaDiez amigos) bebidas)

klusener:: String->Persona->Persona
klusener gusto (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (subtract (length gusto) resistencia) amigos ((klusener gusto):bebidas))

tintico:: Persona->Persona
tintico (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (resistencia + ((length amigos)*5)) amigos (tintico:bebidas))

soda:: Int->Persona->Persona
soda fuerza (Cliente nombre resistencia amigos bebidas) = (Cliente ((("e"++(generoR fuerza))++"p") ++ nombre) resistencia amigos ((soda fuerza):bebidas))
--Genera una cadena de "r" segun el numero que se le pase.
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
addFriend2 cliente nuevo_amigo
                          | ((nombre cliente) == (nombre nuevo_amigo)) = cliente --veo si no soy yo
                          | (length (amigos cliente)) == 0 = (Cliente (nombre cliente) (resistencia cliente) [nuevo_amigo] (bebidas cliente))
                          |  elem (nombre nuevo_amigo) (obtenerNombres (amigos cliente))= cliente --veo si es amigo mio | esta agregando el amigo aunque lo tenga, pero solo 1 vez, luego no lo agrega.
                          | (not (elem (nombre nuevo_amigo) ((obtenerNombres) (amigos cliente) ) ))= (Cliente (nombre cliente) (resistencia cliente) (nuevo_amigo:(amigos cliente)) (bebidas cliente))

--Segunda Parte____________________________________________________________________________________________________________________

--Funcion que hace a un cliente beber un trago.
beber:: Persona->Bebida->Persona
beber cliente bebida = bebida cliente

--Funcion que hace tomar una lista de tragos a un cliente.
tomarTragos::Persona->[Bebida]->Persona
tomarTragos cliente listaTragos
                                |(not (null listaTragos))= tomarTragos (beber cliente (head listaTragos)) (tail listaTragos)
                                | null listaTragos = cliente

--Funcion que hace tomar el ultimo trago tomado a un cliente.
dameOtro:: Persona->Persona
dameOtro cliente = beber cliente (last (bebidas cliente))

--Funcion que define si un cliente puede tomar una bebida sin quedarse con 0 puntos de resistencia.
puedoTomar:: Persona->Bebida->Bool
puedoTomar cliente trago  | (resistencia (trago cliente)) >0 = True
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

--Funcion que cuenta de manera recursiva cada trago y lo va contando.
contarRecursivo cliente trago
                            | puedoTomar cliente trago = 1 + (contarRecursivo (trago cliente) trago)
                            | not(puedoTomar cliente trago) = 0
--Funcion que hace el verdadero conteo de la cantidad de tragos a tomar.
contar cliente tragos
                    | null tragos = []
                    | otherwise = (contarRecursivo cliente (head tragos): contar cliente (tail tragos))
--Funcion que define la cantidad de tragos que puede tomar dada una lista de tragos
cuantasPuedeTomar:: Num a => Persona->[Bebida]->a
cuantasPuedeTomar cliente tragos = genericLength (cualesPuedeTomar cliente tragos)
--Defino el tipo de dato itinerario

data Itinerario = Itinerario {nombreItinerario::String, duracion::Float, acciones::[Accion]} deriving Show

--Defino los 3 itinerrarios mezclaExplosiva,itinerarioBasico y salidaDeAmigos.
mezclaExplosiva = (Itinerario "mezcla Explosiva" 2.5 [grogXD,grogXD,(klusener "huevo"),(klusener "frutilla")])
itinerarioBasico = (Itinerario "basico" 5 [grogXD,jarraLoca,(klusener "huevo"),(klusener "chocolate"), tintico, (soda 10),(soda 0)])
salidaDeAmigos = (Itinerario "salida de amigos" 1 [(soda 1),tintico,(addFriend robertoCarlos),jarraLoca])

--hacerItinerario2 cliente itinerario = foldl cliente (acciones itinerario)
hacerItinerario cliente itinerario = tomarTragos cliente (acciones itinerario)

intensidad2 itinerario = genericLength (acciones itinerario) / (duracion itinerario)
intensidad itinerario = fromRational ((toRational ( length (acciones itinerario)) / realToFrac(duracion itinerario)))


obtenerItinerarioMasIntenso itinerarioMasIntensoViejo itinerariosRestantes
                              | null itinerariosRestantes  = itinerarioMasIntensoViejo
                              | (intensidad itinerarioMasIntensoViejo) < (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso (head itinerariosRestantes) (tail itinerariosRestantes)
                              | (intensidad itinerarioMasIntensoViejo) >= (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso itinerarioMasIntensoViejo (tail itinerariosRestantes)
--Funcion que obtiene el itinerario mas intenso a partir de una lista de itinerarios
--Precondicion que la lista no sea vacia.
itinerarioMasIntenso listaItinerarios
                              | not (null listaItinerarios)= obtenerItinerarioMasIntenso (head listaItinerarios) (tail listaItinerarios)

hacerItinerarioMasIntenso cliente listaItinerarios = hacerItinerario cliente (itinerarioMasIntenso listaItinerarios)

--[FALLA] no esta agregando los amigos.
--version anterior agregaba recursivamente, peor aún.
--agregarUnNivel espirituosidad cliente = jarraPopular (subtract 1 espirituosidad) (foldl addFriend2 cliente (amigos cliente))










unificarLista:: [[Persona]]->[Persona]
unificarLista listaDeListas
                      | not (null (tail listaDeListas)) = ((head listaDeListas) ++ unificarLista (tail listaDeListas))
                      |otherwise = (head listaDeListas)


formarListaRecursivo clientes
                          | not (null clientes) =  (amigos  (head clientes)): formarListaRecursivo (tail clientes)
                          | otherwise = []

formarListaAmigosDeMisAmigos cliente = formarListaRecursivo (amigos cliente)

jarraPopular espirituosidad cliente
                              | espirituosidad == 0 = cliente
                              | otherwise = jarraPopular (subtract 1 espirituosidad) (foldl addFriend2 cliente  (unificarLista (formarListaAmigosDeMisAmigos cliente))) 









--ARREGLAR JARRA POPULAR____

-- TEST OBJETIVO 1D (5) NO DA BIEN

-- TEST OBJETIVO 4A (3) NO DA 0.8, DA 1.4

-- TEST OBJETIVO 5B | 5C FALLAN NO ANDA BIEN JARRAPOPULAR
