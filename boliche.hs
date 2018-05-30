--defino mi tipo de dato cliente.
type Cliente=(Edad,Nombre,Resistencia,Amigos)
type Edad = Int
type Nombre = String
type Resistencia = Int
type Amigos = [String]
--type Amigos = String



drink_grogXD (edad,nombre,resistencia,amigos)= (edad,nombre,0,amigos)

--new_Cliente(Int,String,Int,[String])
--agrego un nuevo cliente si es mayor a 18 años.
new_Cliente ::Cliente->Int
--deberia devolver Amigos no un Bool
new_Cliente (edad,nombre,resitencia,amigos) =
  if edad >=18 then
    0
  else
    1
--somos_Amigos yo amigo =
verifico_No_Soy_Yo (_,nombre1,_,_) (_,_,nombre2,_) =
  nombre1 /= nombre2

add_Friend yo amigo =
  verifico_No_Soy_Yo yo amigo
--  not(somos_Amigos)
