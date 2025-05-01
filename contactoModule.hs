module ContactoModule where

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre ((n,t):contactos) | nombre == n = True  
                                        | otherwise = enLosContactos nombre contactos

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto contacto [] = [contacto]
agregarContacto (nombre,tel) ((n,t):contactos) | nombre == n = (n,tel):contactos
                                               | otherwise = (n,t):agregarContacto (nombre,tel) contactos


getContacto :: Integer -> Contacto
getContacto 1 = ("Franco", "123")
getContacto 2 = ("Franco", "321")
getContacto 3 = ("Emmanuel", "321")
getContacto 4 = ("Marcos", "456")
getContacto 5 = ("Juan", "789")
getContacto 6 = ("Nahuel", "324")

getListaContacto :: Integer -> [Contacto]
getListaContacto 1 = [getContacto 1, getContacto 4, getContacto 3]
getListaContacto 2 = [getContacto 6, getContacto 3, getContacto 3]
getListaContacto 3 = [getContacto 1, getContacto 1, getContacto 3]
getListaContacto 4 = [getContacto 1, getContacto 5, getContacto 3]
   