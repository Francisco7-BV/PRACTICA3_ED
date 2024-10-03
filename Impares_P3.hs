data List a = Void | Node a (List a) deriving Show

--1. Longitud de una lista.:
--La funcion longitud calcula los numeros de elementos de nuestra estructura Lista.

longitud :: List a -> Int
longitud Void = 0
longitud (Node a lista) = 1 + longitud lista

--2. Contencion de un elemento en una lista:
--La funcion estaContenido calcula si un elemento esta contenido en una estructura lista, de ser
--el caso, devuelve True, y en caso contrario False.

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a = False
estaContenido (Node a lista) b = if b == a
                                then True 
                                else estaContenido lista b


--3. Convertir una lista de haskell a nuestra nueva estructura de lista:
--La funcion convertirAEstructura convierte una lista ya definida en haskell a nuestra nueva
--estructura de lista.

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)


--5. Convertir en un conjunto nuestra nueva estructura de lista:
--La funcion conjunto convierte en un conjunto un ejemplar de nuestra nueva estructura lista, es
--decir, sin elementos repetidos. 

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node a lista) = if estaContenido lista a 
                                            then conjunto lista
                                            else Node a (conjunto lista)

--7. Agregar un elemento en un  indice especifico.:
--La funcion insertarIndice agrega el elemento en el  idice especificado que sera pasado como
--parametro de la funcion. 
--NOTA 1: Tendran que verificar que el indice no sea menor a 0 y no sea mayor a la (longitud de
--la lista)-1, recordando que los indices empiezan desde 0 y terminan hasta (longitud de la lista)-1.
--Si no se cumplen estas condiciones tendran que mandar el siguiente mensaje: Indice fuera
--del rango permitido.
--NOTA 2: Al agregar el nuevo elemento en el  indice especificado, los elementos restantes despues
--de dicho  indice se concatenan despues del nuevo elemento, es decir, queremos agregar a la lista
--(Node 1 (Node 2 (Node 3 Void))) en el  indice 1 el nuevo elemento 4, por lo que tendriamos
--que el nuevo elemento se agregaria en la posicion donde estaba Node 2, y que pasa con los
--elementos restantes, se desplazan un lugar, teniendo la nueva lista, Node 1 (Node 4 (Node 2
--(Node 3 Void)))


insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void i b = if i == 0    
                        then Node b Void 
                        else error "Indice fuera del rango permitido"
insertarIndice (Node a lista) i b = if i == 0    
                                    then Node b (Node a lista) 
                                    else if i > 0     
                                        then Node a (insertarIndice lista (i - 1) b)  
                                        else error "Indice fuera del rango permitido"





