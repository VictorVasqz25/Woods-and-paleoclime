Modificaciones realizadas a la matriz

La base de datos de InsideWood posee un mecanismo mediante el cual, se puede anexar información más alla de estar presente o ausente el caracter.
Permite añadir una "v" en caso de que este caracter sea variable, es decir que aveces esté rpesente ene sa especie y a veces no. Y en caso de que la 
presencia de un caracter sea dudosa, se puede anexar "?" con el valor de la caracteristica. Con eso en mente, los cambios realziados para la matriz fueron
los siguientes:

1.Todos los estados de caracter que tenían v, fueron cambiados y se dejaron normales.
Ejemplo:
Antes- 2v3v
Después- 23

2.Los catrcateres que tenían más de un estado de caracter y tenían un incógnito y uno presente, se eliminó el incógnito
y se dejó el presente
Ejemplo:
Antes 2?3
Depués 3

3.Si habían más de un estado de caracter presente, se dejaban ambos y se eliminaba el incógnito.
Ejmeplo:
Antes-2?34
Después-34


4.Al caracter Arreglo de los vasos, se le incluyó un nuevo estado de caracter. Caracter 3 (Vasos sin arreglo). Ya que es usual
enocntrar maderas que no tienen un arreglo de vasos claro y no se llena este caracter para la base de datos.

5.Al caracter Tipo de bandemiento del parénquima, se le incluyó el estado de caracter 5(Parénquima axial sin bandas). Ya que es usual
enocntrar maderas que no tienen un un bandemientod el parénquima claro y no se llena este caracter para la base de datos.

6.Para as especies que tenían más de un individuo, sólo se conservó una de manera aleatoria.

7.El caracter parénquima paratraquel se dejó Ausente y presente, para evitar que este caracter fuese polimórfico para el análisis

8. Se eliminaron los sigueintes caracteres ya que tenían muchos NA

-Agrupamiento de los vasos
-Forma de punteaduras intervasculares
-Tamaño de ls punteaduras intervascualres
-Punteaduras ornamentadas
-Punteaduras vaso-radio
-Engrosamientos espiralados
-Vasos de dos tipos de diámetros distintos
-Vasos por milímetro cuadrado
-Longitud media del eleemento de vaso
-Tilosas y depositos en vasos
-Gomas y otros depósitos
-Traqueidas y fibrotraqueidas
-Engrosamientos en otros tejidos fibrosos
-Parenquima dispuesto en bandas alternando con fibras
-Longitud media de la fibra
-Parenquima apotraqueal

9.Se eliminaron las especies que tenían casillas vacías o con carcateres incógnitos

10.Se cambiaron los múltiples estados de caracter y se dejó sólo uno de forma aleatoria
Ejemplo:
Antes <- 234
despues <- 3

antes <-01
despues <-0
