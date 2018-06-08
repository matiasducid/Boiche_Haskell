
import Text.Show.Functions

--defino mi tipo de dato cliente.
data Persona = Cliente {nombre::String,
                        resistencia::Int,
                        amigos::[Persona],
                        bebidas::[Bebida]} deriving Show
                        -- agregar como dato a cliente.
type Bebida = (Persona->Persona)

--instance Show Persona where
--  show (Cliente "rodri" _ _) = show "rodri"
--  show (Cliente "cristian" _ _) = show "cristian"
--  show (Cliente "marcos" _ _) = show "marcos"
--  show (Cliente "ana" _ _) = show "ana"

-- No puedo simplificar el mostrar. de este modo no funciona

--instance Show Bebida where
--  show (grogXD) = show "grogXD"
--  show (jarraLoca) = show "Jarra Loca"
--  show (klusener) = show "Klusener"
--  show (tintico) = show "Tintico"
--  show (soda) = show "Soda"



rodri = Cliente "rodri" 55 [] [tintico]
marcos = Cliente "marcos" 40 [rodri] [(klusener "guinda")]
cristian = Cliente "cristian" 2 [] [grogXD,jarraLoca]
ana = Cliente "ana" 120 [marcos,rodri] []


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

--Agrega al primer cliente, el segundo cliente
--Anda mal, no sirve para cuando no tienen amigos. tercer regla (not (null (amigos cliente)) = ...)
--  si ya lo tiene como amigo, lo agrega, esta mal eso.
addFriend:: Persona->Persona->Persona
addFriend cliente nuevo_amigo
                          | nombre cliente == nombre nuevo_amigo = cliente --veo si no soy yo
                          | (miembroAmigos (nombre nuevo_amigo) (amigos cliente)) = cliente --veo si es amigo mio | esta agregando el amigo aunque lo tenga, pero solo 1 vez, luego no lo agrega.
                          | (not (null (amigos cliente))) && (not(miembroAmigos (nombre nuevo_amigo) (amigos cliente))) = (Cliente (nombre cliente) (resistencia cliente) (nuevo_amigo:(amigos cliente)) (bebidas cliente))

--Funcion que hace a un cliente beber un trago.
beber:: Persona->Bebida->Persona
beber cliente bebida = bebida cliente

--Funcion que hace tomar una lista de tragos a un cliente.
tomarTragos::Persona->[Bebida]->Persona
tomarTragos cliente listaTragos |(not (null listaTragos))= tomarTragos (beber cliente (head listaTragos)) (tail listaTragos)
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
--pincha con tintico y soda.
cuantasPuedoTomar cliente tragos
                                | null tragos = []
                                | not (null tragos) = contar cliente (cualesPuedeTomar cliente tragos)
