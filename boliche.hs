--defino mi tipo de dato cliente.

import Text.Show.Functions

data Persona = Cliente {nombre::String,
                        resistencia::Int,
                        amigos::[Persona],
                        bebidas::[Bebida]} deriving Show
                        -- agregar como dato a cliente.
--Creo que bebidas deberia ser de un tipo de dato Bebida,
--que sean las funciones definidas como grogXD, tintico, etc.
--data Bebida  |seria type
--instance Show Persona where
--  show (Cliente "rodri" _ _) = show "rodri"
--  show (Cliente "cristian" _ _) = show "cristian"
--  show (Cliente "marcos" _ _) = show "marcos"
--  show (Cliente "ana" _ _) = show "ana"
-- No puedo simplificar el mostrar. de este modo no funciona

--data Bebida = Bebida (Persona->Persona) | Bebida (Int->Persona->Persona)| Bebida (String->Persona->Persona)
type Bebida = (Persona->Persona)
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

beber:: Persona->Bebida->Persona
beber cliente bebida = bebida cliente

tomarTragos::Persona->[Bebida]->Persona
tomarTragos cliente listaTragos |(not (null listaTragos))= tomarTragos (beber cliente (head listaTragos)) (tail listaTragos)
                                | null listaTragos = cliente
dameOtro:: Persona->Persona
dameOtro cliente = beber cliente (last (bebidas cliente))



--calcularTrago:: Persona->Bebida->[Bebida]
--calcularTrago cliente trago | ((resitencia (trago cliente)) >0) = [trago]
--                            | otherwise = []
--cualesPuedeTomar:: Persona->[Bebida]->[Bebida]
--cualesPuedeTomar cliente listaTragos
--                          | (resitencia cliente) == 0 = []
--                          | (resitencia cliente) > 0 = calcularTrago cliente (head listaTragos)
