--Zona de imports.
import Data.List
import Text.Show.Functions

-- Primera Parte___________________________________________________________________________________________________________________________________________________

--defino mi tipo de dato cliente.
data Persona = Cliente {
                        nombre::String,
                        resistencia::Int,
                        amigos::[Persona],
                        bebidas::[Bebida] }
--Defino el tipo de dato bebida.
data Bebida =
  GrogXD
  | JarraLoca
  | Klusener {sabor::String}
  | Tintico
  | Soda {fuerza::Int}
  | JarraPopular {espirituosidad::Int}

type Accion = (Persona->Persona)

--Redefino como mostrar los clientes
instance Show Persona where
  show cliente =
    nombre cliente
    ++ " tomó: "
    ++ show (bebidas cliente)
    ++ ", resistencia "
    ++ show (resistencia cliente)
    ++ ", amigos: "
    ++ show (map nombre (amigos cliente))

--Redefino como mostrar las bebidas.
instance Show Bebida where
    show GrogXD = "GrogXD"
    show JarraLoca = "Jarra Loca"
    show (Klusener sabor) = ("Klusener de " ++ sabor)
    show Tintico = "Tintico"
    show (Soda fuerza) = ("Soda de fuerza " ++ show(fuerza))
    show (JarraPopular espirituosidad) =
         ("Jarra popular de espirituosidad " ++ show(espirituosidad))

-- Creo los clientes.
rodri = Cliente "Rodri" 55 [] [Tintico]
marcos = Cliente "Marcos" 40 [rodri] [(Klusener "guinda")]
cristian = Cliente "Cristian" 2 [] [GrogXD,JarraLoca]
ana = Cliente "Ana" 120 [marcos,rodri] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []

--Funcion que reduce 10 de resistencia a una lista de personas y agrega la bebida como tomada. Utilizada por JarraLoca.
quitaDiez:: Persona->Persona
quitaDiez (Cliente nombre resitencia amigos bebidas)= (Cliente nombre (subtract 10 resitencia) amigos (JarraLoca:bebidas))

--Funcion que dado un numero devuelve una cadena de 'r'. Utilizada en Soda.
generoR:: Int->String
generoR n
          |n>1  = "r" ++ (generoR (subtract 1 n))
          |n == 0 = ""
          |otherwise = "r"

--Funcion que permite 'rescatarse' a una persona.
rescatarse:: Int->Persona->Persona
rescatarse horas (Cliente nombre resistencia amigos bebidas)
                          | horas > 3 = (Cliente nombre (resistencia + 200) amigos bebidas)
                          |otherwise = (Cliente nombre (resistencia + 100) amigos bebidas)

--Funcion que devuelve el estado en que se encuentra una persona según su resistencia.
comoEsta:: Persona->String
comoEsta (Cliente nombre resistencia amigos bebidas)
                                  | resistencia > 50 = "Esta Fresco"
                                  | length(amigos)> 1 = "Esta Piola"
                                  | otherwise = "Esta Duro"

--Funcion que devuelve verdadero si dado un nombre y un cliente, este nombre pertenece a alguno de los amigos del cliente, sinó devuelve falso.
miembroAmigos:: String->[Persona]->Bool
miembroAmigos x (cliente:lista) | (null lista) = False
                                | (nombre cliente) == x = True
                                | otherwise = miembroAmigos x lista

--Funcion que dada una lista de personas devuelve sus nombres.
obtenerNombres :: [Persona] -> [String]
obtenerNombres listaClientes
                            | null listaClientes = []
                            | otherwise = ((nombre (head listaClientes)):obtenerNombres (tail listaClientes))

--Funcion que agrega un amigo a un cliente si este puede ser agregado respetando las reglas de restriccion.
addFriend:: Persona->Persona->Persona
addFriend nuevo_amigo cliente
                          | ((nombre cliente) == (nombre nuevo_amigo)) = cliente --veo si no soy yo
                          | (length (amigos cliente)) == 0 = (Cliente (nombre cliente) (resistencia cliente) [nuevo_amigo] (bebidas cliente))
                          |  elem (nombre nuevo_amigo) (obtenerNombres (amigos cliente))= cliente --veo si es amigo mio | esta agregando el amigo aunque lo tenga, pero solo 1 vez, luego no lo agrega.
                          | (not (elem (nombre nuevo_amigo) ((obtenerNombres) (amigos cliente) ) ))= (Cliente (nombre cliente) (resistencia cliente) (nuevo_amigo:(amigos cliente)) (bebidas cliente))


--Funcion que define como tomar cada bebida.
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

--Funcion que hace tomar el ultimo trago tomado a un cliente (se utiliza 'head' porque los tragos se van agregando a la cabeza de la lista de bebidas tomadas cuando se toma un nuevo trago).
dameOtro:: Persona->Persona
dameOtro cliente = beber cliente (head (bebidas cliente))

--Funcion que define si un cliente puede tomar una bebida sin quedarse con 0 puntos de resistencia o menos.
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

--Redefino como mostrar un itinerario
instance Show Itinerario where
  show itinerario = "Itinerario: "
                    ++ show (nombreItinerario itinerario)
                    ++ " Tiene una duración de "
                    ++ show (duracion itinerario)
                    ++ " horas, y sus acciones son: "
                    ++ show (acciones itinerario)

--Defino los 3 itinerrarios mezclaExplosiva,itinerarioBasico y salidaDeAmigos.
mezclaExplosiva = (Itinerario "mezcla Explosiva" 2.5 [(tomar GrogXD),
                                                      (tomar GrogXD),
                                                      (tomar (Klusener "huevo")),
                                                      (tomar (Klusener "frutilla"))])
itinerarioBasico = (Itinerario "basico" 5 [(tomar JarraLoca),
                                           (tomar (Klusener "chocolate")),
                                           (rescatarse 2),
                                           (tomar (Klusener "huevo"))])
salidaDeAmigos = (Itinerario "salida de amigos" 1 [(tomar (Soda 1)),
                                                  (tomar Tintico),
                                                  (addFriend robertoCarlos),
                                                  (tomar JarraLoca)])

--Funcion que dado un cliente y un itinerario, obtiene las acciones del itinerario y hace que el cliente realize todas las acciones del itinerario.
hacerItinerario :: Persona -> Itinerario -> Persona
hacerItinerario cliente itinerario = hacerAcciones cliente (acciones itinerario)

--Funcion que dado un cliente y una lista de acciones, hace que este cliente realize todas las acciones.
hacerAcciones :: t -> [t -> t] -> t
hacerAcciones cliente acciones
                            | null acciones = cliente
                            | not (null acciones) = hacerAcciones ((head acciones) cliente) (tail acciones)

--Funcion que calcula la intensidad de un itinerario.
intensidad :: Itinerario -> Float
intensidad itinerario = genericLength (acciones itinerario) / (duracion itinerario)

--Funcion que calcula la intensidad de todos los itinerarios de una lista de itinerarios y devuelve el itinerario mas intenso.
obtenerItinerarioMasIntenso :: Itinerario -> [Itinerario] -> Itinerario
obtenerItinerarioMasIntenso itinerarioMasIntensoViejo itinerariosRestantes
                              | null itinerariosRestantes  = itinerarioMasIntensoViejo
                              | (intensidad itinerarioMasIntensoViejo) < (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso (head itinerariosRestantes) (tail itinerariosRestantes)
                              | (intensidad itinerarioMasIntensoViejo) >= (intensidad (head itinerariosRestantes)) = obtenerItinerarioMasIntenso itinerarioMasIntensoViejo (tail itinerariosRestantes)




--Funcion que obtiene el itinerario mas intenso a partir de una lista de itinerarios
--Precondicion que la lista no sea vacia.
itinerarioMasIntenso :: [Itinerario] -> Itinerario
itinerarioMasIntenso listaItinerarios
                              | not (null listaItinerarios)= obtenerItinerarioMasIntenso (head listaItinerarios) (tail listaItinerarios)

--Funcion que dado un cliente y una lista de itinerarios hace que el cliente realize el itinerario mas intenso.
hacerItinerarioMasIntenso :: Persona -> [Itinerario] -> Persona
hacerItinerarioMasIntenso cliente listaItinerarios = hacerItinerario cliente (itinerarioMasIntenso listaItinerarios)

--Funcion que dada una lista de listas devuelve una unica lista que tiene todos los elementos de cada lista.
unificarLista:: [[Persona]]->[Persona]
unificarLista listaDeListas
                      | not (null (tail listaDeListas)) = ((head listaDeListas) ++ unificarLista (tail listaDeListas))
                      |otherwise = (head listaDeListas)

--Funcion que dada una lista de clientes obtiene los amigos de cada uno y devuelve una lista con todos los amigos (forma de matriz).
formarListaRecursivo :: [Persona] -> [[Persona]]
formarListaRecursivo clientes
                          | not (null clientes) =  (amigos  (head clientes)): formarListaRecursivo (tail clientes)
                          | otherwise = []

--Funcion que dado un cliente obtiene los amigos de sus amigos.
formarListaAmigosDeMisAmigos :: Persona -> [[Persona]]
formarListaAmigosDeMisAmigos cliente = formarListaRecursivo (amigos cliente)
