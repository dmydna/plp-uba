
{-
data grupo  = newType  param param
            | newType2 param

-}

data Forma = Rectangulo Float Float
           | Circulo Float

Rectangulo :: Float -> Float -> Forma
Circulo :: Float -> Forma

area :: Forma -> Float
area (Rectangulo ancho alto) = ancho * alto
area (Circulo radio) = radio * radio * pi

data Nat = Zero
         | Succ Nat

Zero :: Nat
Succ :: Nat -> Nat


type Cuenta = String
data Banco = Iniciar
           | Depositar Cuenta Int Banco
           | Extraer Cuenta Int Banco
           | Transferir Cuenta Cuenta Int Banco

bancoPLP = Transferir "A" "B" 3 (Depositar "A" 10 Iniciar)

saldo :: Cuenta -> Banco -> Int
saldo cuenta Iniciar = 0
saldo cuenta (Depositar cuenta’ monto banco)
  | cuenta == cuenta’ = saldo cuenta banco + monto
  | otherwise = saldo cuenta banco

saldo cuenta (Extraer cuenta’ monto banco)
  | cuenta == cuenta’ = saldo cuenta banco - monto
  | otherwise = saldo cuenta banco

saldo cuenta (Transferir origen destino monto banco)
  | cuenta == origen = saldo cuenta banco - monto
  | cuenta == destino = saldo cuenta banco + monto
  | otherwise = saldo cuenta banco



data AB a = Nil | Bin (AB a) a (AB a)

insertar x Nil = Bin Nil x Nil
insertar x (Bin izq y der)
   | x < y = Bin (insertar x izq) y der
   | x > y = Bin izq y (insertar x der)
   | otherwise = Bin izq y der

