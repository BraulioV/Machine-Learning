###############################################################################
# Autor: Braulio Vargas López
# Grupo: 1
# Práctica: 1
# Fecha de entrega: 23/03/2016
# Fecha actual: 1/03/2016
#--------------------------------------------
#
# set.seed(3)
funciones = c(function(x,y) y - ecu[1]*x - ecu[2],
              function(x,y) (x - 10)^2 + (y - 20)^2 - 400,
              function(x,y) 0.5*(x + 10)^2 + (y - 20)^2 - 400,
              function(x,y) 0.5*(x - 10)^2 - (y + 20)^2 - 400,
              function(x,y) y - 20*x^2 - 5*x + 3,
              function(x,y) y - rectaEJ7[1]*x - rectaEJ7[2],
              function(x,y) x^2+y^2 - 25
              )

funciones_eval = c(function(eval) eval[2] - ecu[1]*eval[1] - ecu[2],
                   function(eval) (eval[1] - 10)^2 + (eval[2] - 20)^2 - 400,
                   function(eval)  0.5*(eval[1] + 10)^2 + (eval[2] - 20)^2 - 400,
                   function(eval) 0.5*(eval[1] - 10)^2 - (eval[2] + 20)^2 - 400,
                   function(eval) eval[2] - 20*eval[1]^2 - 5*eval[1] + 3,
                   function(eval) eval[2] - rectaEJ7[1]*eval[1] - rectaEJ7[2],
                   function(eval) eval[1]^2+eval[2]^2 - 25
                   )

#---------------------------
# EJERCICIO 3.2
#
# Ejercicio de Generación y Visualización de datos
#
#   1.  Construir una función lista = simula_unif(N,dim,rango) que calcule una 
#       lista de longitud N de vectores de dimensión dim conteniendo números
#       aleatorios uniformes en el intervalo rango.

simula_unif <- function(N=5,dim=2,rango=5:20){
    # Generamos una matriz de datos aletorios distribuidos uniformemente
    # en el rango de valores introducido. Esta matriz tendrá el un número
    # de filas N, y un número de columnas dim.
    num = matrix(runif(N*dim, min=rango[1], max=rango[length(rango)]), 
      nrow = N, ncol = dim)
    num
}

print("######################################################################")
print("Ejercicio 1")
print("######################################################################")
lista = simula_unif()
print(lista)

#   2.  Construir una función lista = simula_gaus(N,dim,sigma) que calcule una
#       lista de longitud N de vectores de dimensión dim conteniendo números
#       aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
#

simula_gaus <- function(N=5,dim=2,sigma=1){
    media = 0
    # Al igual que antes, generamos una matriz,pero esta vez sigue una 
    # distribución gaussiana, con media 0, inicializada más arriba y sigma = 1
    # por defecto. La matriz tendrá N filas y dim columnas.
    num = matrix(rnorm(N*dim, media, sigma), nrow = N, ncol = dim)
    num
}


print("######################################################################")
print("Ejercicio 2")
print("######################################################################")
lista = simula_gaus()
print(lista)

pintar <- function(puntos, funcion, intervalo, nombreGrafica, 
    colores = colors(), verFuncion = FALSE){
    if(verFuncion){
        x <- y <- seq(range(puntos[,1])[1],range(puntos[,1])[2],length=100)
        z <- outer(x,y,funcion)
        contour(
            x=x, y=x, z=z,
            levels=0, las=1, drawlabels=FALSE, lwd=3, xlab="Eje X", 
            ylab="Eje y",main = nombreGrafica
        )
    }
    else{
        plot(intervalo, intervalo, xlab="Eje X", ylab="Eje y", type="n", 
        main = nombreGrafica)
    }

    points(puntos, col = colores, pch=19, lwd = 2)
}


#   3.  Suponer N = 50, dim = 2, rango = [-50,+50] en cada dimensión. Dibujar
#       una gráfica de la salida de la función correspondiente

pintaUnif <- function(N = 50, dim = 2, rangoV = -50:50){
  pintar(simula_unif(N,dim,rangoV), intervalo = rangoV, 
    nombreGrafica = "EJERCICIO 3", verFuncion = F)

}

print("######################################################################")
print("Ejercicio 3")
print("######################################################################")

pintaUnif()

#  4.  Suponer N = 50, dim = 2, rango = [5,7] en cada dimensión. Dibujar
#      una gráfica de la salida de la función correspondiente.

pintaGauss <- function(N = 50, dim = 2, sigma = 5:7){
    pintar(simula_gaus(N,dim,sigma), intervalo = (sigma[3]*(-4)):(sigma[3]*4), 
        nombreGrafica = "EJERCICIO 4", verFuncion = F)
}

pintaGauss()

# 5.  Construir la función v=simula_recta(intervalo) quealcula los 
#     parámetros, v=(a,b) de una recta aleatoria, y = ax + b, que corte al
#     cuadrado [-50,50] x [-50,50] (Ayuda: Para calcular la recta simular las
#     coordenadas de dos puntos dentro del cuadrado y calcular la recta que
#     pasa por ellos)

simula_recta <- function(dim = -50:50, punto_1 = c(sample(dim, 2)),
    punto_2 = c(sample(dim, 2)), verPunto = T){
    # Por defecto, obtenemos dos puntos aleatorios dentro del intervalo,
    # pero en caso de que 
    if(verPunto){
        print("#####Punto 1")
        print(punto_1)
    
        print("#####Punto 2")
        print(punto_2)
    }
    
    # Para calcular la recta que pasa por dos puntos, usaremos la expresión:
    #       
    #          x - x1      y - y1
    #        --------- = ----------
    #         x2 - x1     y2 - y1
    #
    # Con lo que si despejamos obtenemos
    #
    #   => (y2-y1)x - x1(y2-y1) = y(x2-x1) - y1(x2-x1)
    #   => y = (y2-y1)x/(x2-x1) - (x1(y2-y1)+y1(x2-x1))/(y2-y1)

    punto_aux = c(punto_2[1]-punto_1[1], punto_2[2]-punto_1[2])

    ecuacion = c(punto_aux[2]/punto_aux[1], 
        (((-1*punto_1[1]*punto_aux[2])+(punto_1[2]*punto_aux[1]))/punto_aux[1]))
    ecuacion
}

print("######################################################################")
print("Ejercicio 5")
print("######################################################################")
ecu = simula_recta()

print(ecu)



# 6.  Generar una muestra 2D de puntos usando simula_unif() y etiquetar la 
#     muestra usando el signo de la función f (x, y) = y − ax − b de cada 
#     punto a una recta simulada con simula_recta(). Mostrar una gráfica con 
#     el resultado de la muestra etiquetada junto con la recta usada para ello.



print("######################################################################")
print("Ejercicio 6")
print("######################################################################")

evaluaPuntos <- function(puntosAEvaluar, intervalo, apartado){

    # etiquetas = mapply(function(x,y) y - ecu[1]*x - ecu[2], puntosAEvaluar)
    etiquetas = apply(X=puntosAEvaluar, FUN=funciones_eval[[apartado]], MARGIN=1)
    sign(etiquetas)
}

range = -50:50
puntos = simula_unif(N=100,dim=2,rango=range)
etiquetas6 = evaluaPuntos(puntos, range, apartado = 1)

pintar(puntos, funciones[[1]], range, "EJERCICIO 6", 
    colores =  etiquetas6 + 4, verFuncion = T)

# 7.  Usar la muestra generada en el apartado anterior y etiquetarla con +1,-1 
#     usando el signo de cada una de las siguientes funciones
#         - f (x, y) = (x − 10)^2 + (y − 20)^2 − 400
#         - f (x, y) = 0.5*(x + 10)2 + (y − 20)2 − 400
#         - f (x, y) = 0.5(x − 10)2 − (y + 20)2 − 400
#         - f (x, y) = y − 20x2 − 5x + 3
#     Visualizar el resultado del etiquetado de cada función junto con su 
#     gráfica y comparar el resultado con el caso lineal Â¿Que consecuencias 
#     extrae sobre la forma de las regiones positiva y negativa?

etiquetas7_1 = evaluaPuntos(puntos, range, apartado = 2)
# pintar(puntos, funciones[[2]], range, "EJERCICIO 7.1", 
#     colores = etiquetas7_1+4, verFuncion = T)

etiquetas7_2 = evaluaPuntos(puntos, range, apartado = 3)
# pintar(puntos, funciones[[3]], range, "EJERCICIO 7.2", 
#     colores = etiquetas7_2+4, verFuncion = T)

etiquetas7_3 = evaluaPuntos(puntos, range, apartado = 4)
# pintar(puntos, funciones[[4]], range, "EJERCICIO 7.3", 
#     colores = etiquetas7_3+4, verFuncion = T)

etiquetas7_4 = evaluaPuntos(puntos, range, apartado = 5)
# pintar(puntos, funciones[[5]], range, "EJERCICIO 7.4", 
#     colores = etiquetas7_4+4, verFuncion = T)

etiquetasFunc = list(etiquetas6, etiquetas7_1, 
    etiquetas7_2, etiquetas7_3, etiquetas7_4)


# 8.  Considerar de nuevo la muestra etiquetada en el apartado.6. Modifique 
#     las etiquetas de un 10 % aleatorio de muestras positivas y otro 10 % 
#     aleatorio de negativas. Visualice los puntos con las nuevas etiquetas y 
#     la recta del apartado 6. En una gráfica aparte visualice nuevo los mismos 
#     puntos pero junto con las funciones del apartado 7.

cambiarEtiqueta <- function(etiquetas, porcentaje = 0.1){
    i = 1
    num = sample(1:length(etiquetas),length(etiquetas)*0.1)
    etiquetas[num] = -etiquetas[num]
    etiquetas
}

asignaColorEtiquetasErroneas <- function(etiquetasOriginales, etiquetasErroneas){
    coloresEtiquetas = etiquetasOriginales
    coloresEtiquetas[coloresEtiquetas != etiquetasErroneas] = 
        coloresEtiquetas[coloresEtiquetas != etiquetasErroneas] -1
    coloresEtiquetas + 4
}

etiquetasErroneas8_1 = cambiarEtiqueta(etiquetas6)

pintar(puntos, funciones[[1]], range, "EJERCICIO 8.1", 
    colores = asignaColorEtiquetasErroneas(etiquetas6,etiquetasErroneas8_1), 
    verFuncion = T)

etiquetasErroneas8_2 = cambiarEtiqueta(etiquetas7_1)

pintar(puntos, funciones[[2]], range, "EJERCICIO 8.2", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_1,etiquetasErroneas8_2), 
    verFuncion = T)

etiquetasErroneas8_3 = cambiarEtiqueta(etiquetas7_2)

pintar(puntos, funciones[[3]], range, "EJERCICIO 8.3", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_2,etiquetasErroneas8_3), 
    verFuncion = T)

etiquetasErroneas8_4 = cambiarEtiqueta(etiquetas7_3)

pintar(puntos, funciones[[4]], range, "EJERCICIO 8.4", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_3,etiquetasErroneas8_4), 
    verFuncion = T)

etiquetasErroneas8_5 = cambiarEtiqueta(etiquetas7_4)

pintar(puntos, funciones[[5]], range, "EJERCICIO 8.5", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_4,etiquetasErroneas8_5), 
    verFuncion = T)


print("#######################################################################")
print("EJERCICIO PERCEPTRON")
print("#######################################################################")

PLA <- function(datos, label, max_iter = 3000, vini = c(00,00,00), verRecorrido = F){
    
    # Añadimos un 1 por la izquierda a la matriz de puntos para que los puntos
    # de la matriz sean (x0, x1, x2) donde x1 será un 1, y x1 y x2 las 
    # coordenadas del punto.
    datos = cbind(rep(1, nrow(datos)), datos)
    i = 1 # Contador para saber el número de iteraciones
    converge = FALSE # Variable para saber si el perceptron cambia o no. Es decir,
                     # cuando deje de cambiar, el algoritmo para

    # Empezamos el bucle del perceptron
    while ((i <= max_iter) & (!converge)){
        # while (i <= max_iter){
        cambiado = FALSE
        # iteramos sobre los puntos de cada dato
        for(j in 1:nrow(datos)){
            # Calculamos el signo en función de los pesos realizando el producto
            # del punto y el vector de pesos
            signo = sign(datos[j,]%*% vini)
            
            # En caso de que el signo sea 0 por la función sign, se pone a 1
            if (signo == 0){
                signo = signo + 1
            }

            # Cuando las etiquetas no coinciden, se actualiza el vector 
            # de pesos y se cambia la variable "cambiado" a true para indicar
            # que se ha equivocado y por lo tanto no converge
            if(label[j] != signo){
                cambiado = TRUE
                vini = vini + datos[j,]*label[j]
                if(verRecorrido)
                    abline(a=(-vini[1]/vini[3]),
                        b=(-vini[2]/vini[3]), col="grey")
            }
        }

        i = i + 1 # incrementamos el contador de iteraciones

        if(!cambiado){
            # en caso de que no se "equivoque" en ningún punto, decimos que
            # el perceptron ha convergido y finaliza el bucle
            converge = TRUE
        }

    }

    # Se devuelve la recta que ha calculado el perceptron normalizada
    pla_result = c((-vini[2]/vini[3]),(-vini[1]/vini[3]), i)
    pla_result
}

pintarPLA <- function(puntosAPintar, rango, apartado, nombreGrafica, 
    verF = T, verR = F, verLeyenda = F, leyenda, coloresLeyenda,
    usarPocket = F){
    

    pintar(puntosAPintar, funciones[[apartado]], rango, nombreGrafica,
           colores = (etiquetasFunc[[apartado]] + 4), verFuncion = verF)

    if(verLeyenda){
        # Leyenda de la función
        legend(x=15,y=45,legend=leyenda, lty=c(1,1),
            lwd=c(2.5,2.5),col=coloresLeyenda)
    }

    if(!usarPocket)
        perceptron = PLA(datos = puntos, label = etiquetasFunc[[apartado]], 
            verRecorrido = verR)
    else{
        perceptron = PocketPLA(datos = puntos, label = etiquetasFunc[[apartado]], 
            verRecorrido = verR)

        print("ERROR OBTENIDO CON PLAPOCKET")
        print(perceptron[4])
    }

    abline(a=perceptron[2],b=perceptron[1], col="red", lwd=3)
}

pintarPLA(puntos, range, apartado = 1, nombreGrafica = "EJERCICIO PERCEPTRON",
    leyenda = c("Función original","Perceptron"), verLeyenda = T,
    coloresLeyenda=c("black","red"))

#---------------------------
# EJERCICIO SOBRE REGRESIÓN LINEAL
#

# 1.  Abra el fichero ZipDigits.info disponible en la web del curso y lea la 
#     descripción de la representación numérica de la base de datos de números 
#     manuscritos que hay en el fichero ZipDigits.train.

# En el fichero se especifica qué número es al principio de cada una de las filas de datos, seguidas por 256 valores de escalas de grises

leerDatos <- function(nombreFichero="./DigitosZip/zip.train"){
    zip = read.table(file=nombreFichero, sep=" ", stringsAsFactors=F)
    # Como la última columna de la tabla que genera son valores NA, la 
    # eliminamos igualándola a NULL.
    unos = zip[which(zip$V1==1.0000),2:257]
    cincos = zip[which(zip$V1==5.0000),2:257]
    list(unos,cincos)
}

pintarNumero <- function(numeros){
    # print(numeros)
    numero = numeros[[1]][1,]
    matriz = as.matrix(numero)
    dim(matriz)=c(16,16)
    image(matriz)
}

datosLeidos = leerDatos()

#### ejercicio 3

media <- function(n) apply(X=n,FUN=mean,MARGIN=1)
simetria <- function(n) apply(X=n, 
    FUN=function(line)-1*sum(abs(line[1:256]-line[256:1])), MARGIN=1)

calculaSimetria <- function(numeros){
    # Calculamos la media de los píxeles para los unos y los cincos, además 
    # de las simetrias de ambos, devolviendolos en distintas listas
    unos=list(media(numeros[[1]]),simetria(numeros[[1]]))
    cincos=list(media(numeros[[2]]),simetria(numeros[[2]]))
    list(unos,cincos)
}

val=calculaSimetria(datosLeidos)

#### ejercicio 4

representarMediaSimetria <- function(valores){
    mean1 = valores[[1]][[1]]
    simetria1 = valores[[1]][[2]]

    mean5 = valores[[2]][[1]]
    simetria5 = valores[[2]][[2]]

    plot(x=mean5, y=simetria5, col = "red",pch=19, lwd = 2, main="Media y simetría de los cincos")
    plot(x=mean1, y=simetria1, col = "green",pch=19, lwd = 2, main="Media y simetría de los unos")
}

representarMediaSimetria(val)

Regress_Lin <- function(datos, label){
    pseudoinversa = solve(t(datos)%*%datos)%*%t(datos)

    wlin=(pseudoinversa%*%label)
    y_prima=datos%*%(wlin)

    list(wlin, y_prima)

}

ajusteRegresion <- function(media, simetria){
    regresion = Regress_Lin(media, simetria)
    plot(x=media, y=simetria, col = "green",pch=19, lwd = 2, main="Media y simetría de los unos")
    lines(x=media, y = regresion[[2]], col="blue", lwd=2, pch=19)
}

ajusteRegresion(val[[2]][[1]], val[[2]][[2]])
ajusteRegresion(val[[2]][[1]], val[[2]][[2]])

tam = 100
rangoEJ7 = -10:10
rectaEJ7 = simula_recta(dim=rangoEJ7)

calcularEout <- function(ein, datos, tam){
    Eout = ein + (ncol(datos))/tam
    Eout
}

calcularEin <- function(X, wlin, N, y){
    errorIN = (1/N)*(t(wlin)%*%t(X)%*%X%*%wlin - 
        2*t(wlin)%*%t(X)%*%y + 
        t(y)%*%y)
    
}

regresionLinealClasificacion <- function(){

    puntosEJ7 = simula_unif(N=tam, dim=2, rango=rangoEJ7)
    etiquetasEJ7 = evaluaPuntos(puntosEJ7, rangoEJ7, apartado = 6)

    g = Regress_Lin(puntosEJ7, etiquetasEJ7)

    wlin = g[[1]]
    errorIN = calcularEin(X = puntosEJ7, wlin = g[[1]], N = tam, y = etiquetasEJ7)
    
    Eout = calcularEout(errorIN, puntosEJ7, tam)
    c(errorIN, Eout)

}

apartadoEJ7C <- function(){
    # seleccionamos el tamaño de la muestra a 10 y generamos las muestras, 
    # junto con las etiquetas
    tam = 10
    puntosEJ7C = simula_unif(N=tam, dim=2, rango=rangoEJ7)
    etiquetasEJ7C = evaluaPuntos(puntosEJ7C, rangoEJ7, apartado = 6)

    # ajustamos la regresión para los datos y las etiquetas
    regC = Regress_Lin(puntosEJ7C, etiquetasEJ7C)

    # una vez que tenemos los datos, y la regresión, calculamos los 
    # parámetros de la recta que se ajustan a la regresión
    rectaEJ7C = simula_recta(punto_1 = c(puntosEJ7C[1,1],regC[[1]][1,]),
                            punto_2 = c(puntosEJ7C[2,1],regC[[2]][1,]),
                            verPunto = F)    
    
    # lanzamos el pla con la recta que se ajusta a la regresión como vector
    # inicial del pla
    plaEJ7C = PLA(datos=puntosEJ7C, label=etiquetasEJ7C, 
        max_iter = 3000, vini = c(rectaEJ7C[2],rectaEJ7C[1],1))

    plaEJ7C
}

# apartado a y b
Eins = vector(length=1000)
Eouts = vector(length=1000)
numMedioIteraciones = vector(length=1000)
for (i in 1:1000) {
    Eins[i] = regresionLinealClasificacion()[1]
    Eouts[i] = regresionLinealClasificacion()[2]
    numMedioIteraciones[i] = apartadoEJ7C()[3]

}
meanEin = mean(Eins)
meanEout = mean(Eouts)
meanIters = mean(numMedioIteraciones)
cat("Media de Ein para la regresion lineal = ",meanEin,"\n")
cat("Media de Eout para la regresion lineal = ",meanEout,"\n")
cat("Numero medio de iteraciones para el PLA = ",meanIters,"\n")

#------------------------------------------------------------------
#   EJERCICIO 8
#------------------------------------------------------------------

ejercicio8<-function(){
    puntosEJ8 = simula_unif(N=1000, dim=2, rango=(-10:10))
    etiquetasEJ8=evaluaPuntos(puntosAEvaluar=puntosEJ8, 
        intervalo=(-10:10), apartado=7)

    etiquetasEJ8=cambiarEtiqueta(etiquetasEJ8)

    g=Regress_Lin(puntosEJ8,etiquetasEJ8)

    Ein = calcularEin(X = puntosEJ8, wlin = g[[1]], N = 1000, y = etiquetasEJ8)
    Ein
}

Eins8 = vector(length=1000)
for (i in 1:1000) {
    Eins8[i]=ejercicio8()
}

ErrorMedioIn8 = mean(Eins8)
cat("Error medio en Ein del ejercicio 8",ErrorMedioIn8,"\n")

apartado8B <- function(mostrar=F){
    puntosEJ8 = simula_unif(N=1000, dim=2, rango=(-10:10))
    etiquetasEJ8=evaluaPuntos(puntosAEvaluar=puntosEJ8, 
        intervalo=(-10:10), apartado=7)
    
    etiquetasEJ8=cambiarEtiqueta(etiquetasEJ8)

    f<-function(linea) c(1,linea[1],linea[2],linea[1]*linea[2],
        linea[1]*linea[1],linea[2]*linea[2])

    nuevosPuntos=apply(X=puntosEJ8, FUN=f, MARGIN=1)
    nuevosPuntos = t(nuevosPuntos)
    g=Regress_Lin(nuevosPuntos, etiquetasEJ8)
    if(mostrar){
        pintar(puntos=puntosEJ8, funciones[[7]], range, "EJERCICIO 8.b", 
        colores =  etiquetasEJ8 + 4, verFuncion = T)
        points(x=puntosEJ8[,1], y = g[[2]], col="blue", lwd=2, pch=19)
    }

    Ein = calcularEin(X = nuevosPuntos, wlin = g[[1]],
        N = 1000, y = etiquetasEJ8)
    Eout = calcularEout(Ein, nuevosPuntos, 1000)
    Eout
}

apartado8B(T)

Eouts8 = vector(length=1000)
for (i in 1:1000) {
    Eouts8[i]=ejercicio8()
}

ErrorMedioOut8 = mean(Eouts8)
cat("Error medio en Eout del ejercicio 8",ErrorMedioOut8,"\n")