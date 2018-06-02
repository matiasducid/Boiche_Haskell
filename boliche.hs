--defino mi tipo de dato cliente.


data Persona = Cliente Nombre Resistencia [Persona] deriving(Show)
--type Edad = Int
type Nombre = String
type Resistencia = Int


rodri = Cliente "rodri" 55 []
marcos = Cliente "marcos" 40 [rodri]
cristian = Cliente "cristian" 2 []
ana = Cliente "Ana" 120 [marcos,rodri]



grogXD (Cliente nombre resistencia amigos)= (Cliente nombre 0 amigos)

comoEsta (Cliente nombre resistencia amigos)
                                  | resistencia > 50 = "Esta Fresco"
                                  | length(amigos)> 1 = "Esta Piola"
                                  | otherwise = "Esta Duro"
member x y =  if null y then False
              else if x == head y then True
              else member x tail y

--Agrega al primer cliente, el segundo cliente
addFriend (Cliente nombre1 resistencia1 amigos1) (Cliente nombre2 resistencia2 amigos2)
                                  | nombre1 == nombre2 = (Cliente nombre1 resistencia1 amigos1)
                                  | member nombre2 amigos1 = (Cliente nombre1 resistencia1 amigos1)
                                  | not(member) nombre2 amigos1 = (Cliente nombre1 resistencia1 ((Cliente nombre2 resistencia2 amigos2):amigos1))
