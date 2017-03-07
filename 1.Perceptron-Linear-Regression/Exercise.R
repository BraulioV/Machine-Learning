###############################################################################
# Autor: Braulio Vargas López
# Grupo: 1
# Práctica: 1
# Fecha de entrega: 23/03/2016
# Fecha actual: 1/03/2016
#--------------------------------------------
#
# set.seed(3)
pause <-function(){
    print("Pulsar una tecla para continuar")
    scan(what=character(), n=1)
}

#-----------------------------------------------
#   EJERCICIO 1.1
#-----------------------------------------------
simula_unif <- function(N=5,dim=2,rango=5:20){
    # Generamos una matriz de datos aletorios distribuidos uniformemente
    # en el rango de valores introducido. Esta matriz tendrá el un número
    # de filas N, y un número de columnas dim.
    num = matrix(runif(N*dim, min=rango[1], max=rango[length(rango)]), 
      nrow = N, ncol = dim)
    num
}

print(simula_unif())
pause()

#-----------------------------------------------
#   EJERCICIO 1.2
#-----------------------------------------------

simula_gaus <- function(N=5,dim=2,sigma=1){
    media = 0
    # Al igual que antes, generamos una matriz,pero esta vez sigue una 
    # distribución gaussiana, con media 0, inicializada más arriba y sigma = 1
    # por defecto. La matriz tendrá N filas y dim columnas.
    num = matrix(rnorm(N*dim, media, sigma), nrow = N, ncol = dim)
    num
}

print(simula_gaus())
pause()
#-----------------------------------------------
#   EJERCICIO 1.3
#-----------------------------------------------

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

pintaUnif <- function(N = 50, dim = 2, rangoV = -50:50){
  pintar(simula_unif(N,dim,rangoV), intervalo = rangoV, 
    nombreGrafica = "EJERCICIO 3", verFuncion = F)
}

pintaUnif()
pause()

#-----------------------------------------------
#   EJERCICIO 1.4
#-----------------------------------------------

pintaGauss <- function(N = 50, dim = 2, sigma = 5:7){
    pintar(simula_gaus(N,dim,sigma), intervalo = (sigma[3]*(-4)):(sigma[3]*4), 
        nombreGrafica = "EJERCICIO 4", verFuncion = F)
}

pintaGauss()
pause()

#-----------------------------------------------
#   EJERCICIO 1.5
#-----------------------------------------------

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

ecu = simula_recta()
print(ecu)
pause()

#-----------------------------------------------
#   EJERCICIO 1.6
#-----------------------------------------------

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

evaluaPuntos <- function(puntosAEvaluar, intervalo, apartado){
    etiquetas = apply(X=puntosAEvaluar, FUN=funciones_eval[[apartado]], MARGIN=1)
    sign(etiquetas)
}


range = -50:50
puntos = simula_unif(N=100,dim=2,rango=range)

etiquetas6 = evaluaPuntos(puntos, range, apartado = 1)
pintar(puntos, funciones[[1]], range, "EJERCICIO 6", 
    colores =  etiquetas6 + 4, verFuncion = T)

pause()

#-----------------------------------------------
#   EJERCICIO 1.7
#-----------------------------------------------


etiquetas7_1 = evaluaPuntos(puntos, range, apartado = 2)
pintar(puntos, funciones[[2]], range, "EJERCICIO 7.1", 
    colores = etiquetas7_1+4, verFuncion = T)

pause()

etiquetas7_2 = evaluaPuntos(puntos, range, apartado = 3)
pintar(puntos, funciones[[3]], range, "EJERCICIO 7.2", 
    colores = etiquetas7_2+4, verFuncion = T)

pause()

etiquetas7_3 = evaluaPuntos(puntos, range, apartado = 4)
pintar(puntos, funciones[[4]], range, "EJERCICIO 7.3", 
    colores = etiquetas7_3+4, verFuncion = T)

pause()

etiquetas7_4 = evaluaPuntos(puntos, range, apartado = 5)
pintar(puntos, funciones[[5]], range, "EJERCICIO 7.4", 
    colores = etiquetas7_4+4, verFuncion = T)

pause()

etiquetasFunc = list(etiquetas6, etiquetas7_1, 
    etiquetas7_2, etiquetas7_3, etiquetas7_4)

#-----------------------------------------------
#   EJERCICIO 1.8
#-----------------------------------------------

cambiarEtiqueta <- function(etiquetas, porcentaje = 0.1){
    i = 1
    num = sample(1:length(etiquetas),length(etiquetas)*0.1)
    etiquetas[num] = -etiquetas[num]
    etiquetas
}

# Función para asignar colores distintos a las etiquetas bien clasificasdas
# y a las mal clasificadas
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

pause()

etiquetasErroneas8_2 = cambiarEtiqueta(etiquetas7_1)

pintar(puntos, funciones[[2]], range, "EJERCICIO 8.2", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_1,etiquetasErroneas8_2), 
    verFuncion = T)

pause()

etiquetasErroneas8_3 = cambiarEtiqueta(etiquetas7_2)

pintar(puntos, funciones[[3]], range, "EJERCICIO 8.3", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_2,etiquetasErroneas8_3), 
    verFuncion = T)

pause()

etiquetasErroneas8_4 = cambiarEtiqueta(etiquetas7_3)

pintar(puntos, funciones[[4]], range, "EJERCICIO 8.4", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_3,etiquetasErroneas8_4), 
    verFuncion = T)

pause()

etiquetasErroneas8_5 = cambiarEtiqueta(etiquetas7_4)

pintar(puntos, funciones[[5]], range, "EJERCICIO 8.5", 
    colores = asignaColorEtiquetasErroneas(etiquetas7_4,etiquetasErroneas8_5), 
    verFuncion = T)

pause()

#-----------------------------------------------
#   EJERCICIO 2.1
#-----------------------------------------------

PLA <- function(datos, label, max_iter = 3000, vini = c(0,0,0), verRecorrido = F){
    
    # Añadimos un 1 por la izquierda a la matriz de puntos para que los puntos
    # de la matriz sean (x0, x1, x2) donde x1 será un 1, y x1 y x2 las 
    # coordenadas del punto.
    datos = cbind(rep(1, nrow(datos)), datos)
    i = 1 # Contador para saber el número de iteraciones
    converge = FALSE # Variable para saber si el perceptron cambia o no. Es decir,
                     # cuando deje de cambiar, el algoritmo para

    # Empezamos el bucle del perceptron
    while ((i <= max_iter) & (!converge)){
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
                  abline(a=(-vini[1]/vini[3]),b=(-vini[2]/vini[3]), col="grey");
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
    # y el número de iteraciones hechas por el algoritmo
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

pause()

#-----------------------------------------------
#   EJERCICIO 2.2
#-----------------------------------------------

iteraciones = vector(length = 10)
aux = c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,1,0,1)
pesosM = matrix(aux, nrow = 10, ncol = 3, byrow=T)

for(i in 1:10){
    perceptron = PLA(datos = puntos, label = etiquetas6, vini=pesosM[i,])
    iteraciones[i] = perceptron[3]
}

tabla = cbind(pesosM, iteraciones)
print(tabla)

pause()

#-----------------------------------------------
#   EJERCICIO 2.3
#-----------------------------------------------

etiqErr = list(etiquetasErroneas8_1,etiquetasErroneas8_2,etiquetasErroneas8_3,
    etiquetasErroneas8_4,etiquetasErroneas8_5)

funcionesNoConvergentes <- function(apartado, nombre, rango=range){
    # Se pinta el gráfico inicial, junto con la función original, junto con la
    # leyenda del gráfico
    pintar(puntos, funciones[[apartado]], rango, nombreGrafica=nombre,
           colores = (etiqErr[[apartado]] + 4), verFuncion = T)
    
    legend(x=15,y=45,legend=c("Función original","PLA 10","PLA 100","PLA 1000"),
        lty=c(1,1),lwd=c(2.5,2.5,2.5,2.5),
        col=c("black","red","firebrick","chocolate"))
    
    coloresEj533 = c("red","firebrick","chocolate")
    
    for(i in 1:3){
        plaRecta = PLA(datos = puntos, label = etiqErr[[apartado]], max_iter = 10^i)
        abline(a=plaRecta[2],b=plaRecta[1], col=coloresEj533[i], lwd=3)
    }
}

funcionesNoConvergentes(apartado = 1, nombre = "PLA no convergente 1")

pause()

#-----------------------------------------------
#   EJERCICIO 2.4
#-----------------------------------------------

funcionesNoConvergentes(apartado = 2, nombre = "PLA no convergente 2")

pause()

#-----------------------------------------------
#   EJERCICIO 2.5
#-----------------------------------------------

pintarPLA(puntos, range, apartado = 1, nombreGrafica = "EJERCICIO PERCEPTRON",
    leyenda = c("Función original","Perceptron"), verLeyenda = F,
    coloresLeyenda=c("black","red"), verR = T)
pause()
#-----------------------------------------------
#   EJERCICIO 2.6
#-----------------------------------------------

Ein <- function(datosEval, labels, resultPLA){
    # Esta función de evaluación se puede ver en la página 80 del libro
    # "Learning From Data: A Short Course", donde se minimiza el error dentro
    # de la muestra, calculando el porcentaje de error 
    ein = sign(datosEval%*%resultPLA)
    ein[ein==0]=1
    error = length(ein[ein != labels])/nrow(datosEval)
    error
}

PocketPLA <- function(datos, label, max_iter = 3000, vini = c(0,0,0), verRecorrido = F){
    
    # Añadimos un 1 por la izquierda a la matriz de puntos para que los puntos
    # de la matriz sean (x0, x1, x2) donde x1 será un 1, y x1 y x2 las 
    # coordenadas del punto.
    datos = cbind(rep(1, nrow(datos)), datos)
    i = 1 # Contador para saber el número de iteraciones

    mejorSol = vini
    porcentajeError = Ein(datos=datos, labels=label, resultPLA=mejorSol)

    # Empezamos el bucle del perceptron
    while (i <= max_iter){
        # iteramos sobre los puntos de cada dato
        for(j in 1:nrow(datos)){
            # Calculamos el signo en función de los pesos realizando el producto
            # del punto y el vector de pesos
            signo = sign(datos[j,]%*% vini)
            
            # En caso de que el signo sea 0 por la función sign, se pone a 1
            if (signo == 0){
                signo = signo + 1
            }
            # Cuando las etiquetas no coinciden, se calcula w(t+1) y se guarda
            # en v_aux. Tras esto, se calcula el error dentro de la muestra 
            # (nuevoPorcentaje) y se compara con el que teníamos antes. Si es
            # menor, se actualiza la mejor solucion y el porcentaje minimo. Si
            # no, se pasa a la siguiente iteración con el w(t)
            if(label[j] != signo){
                v_aux = vini + datos[j,]*label[j]
                nuevoPorcentaje = Ein(datos=datos, labels=label, resultPLA=v_aux)
                if(porcentajeError > nuevoPorcentaje){
                    # se muestran los 
                    cat("Antiguo pocentaje de error = ",porcentajeError,"\n")
                    cat("Nuevo porcentaje de error = ",nuevoPorcentaje,"\n")
                    vini = v_aux
                    porcentajeError = nuevoPorcentaje
                }
                if(verRecorrido)
                    abline(a=(-vini[1]/vini[3]),
                        b=(-vini[2]/vini[3]), col="grey")
            }
        }

        i = i + 1 # incrementamos el contador de iteraciones
    }

    # Se devuelve la recta que ha calculado el perceptron normalizada
    pla_result = c((-vini[2]/vini[3]),(-vini[1]/vini[3]), i, porcentajeError)
    pla_result
}

pintarPLA(puntos, range, apartado = 4, nombreGrafica = "EJERCICIO PERCEPTRON POCKET",
    leyenda = c("Función original","Perceptron pocket"), verLeyenda = T,
    coloresLeyenda=c("black","red"), usarPocket = T)
pause()

#-----------------------------------------------
#   EJERCICIO 3.2
#-----------------------------------------------

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

pintarNumero(datosLeidos[1])
pause()

pintarNumero(datosLeidos[2])
pause()

#-----------------------------------------------
#   EJERCICIO 3.3
#-----------------------------------------------

# Función que calcula la media de los datos para cada píxel
media <- function(n) apply(X=n,FUN=mean,MARGIN=1)
# Función que calcula la simetría de la matriz
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
pause()

#-----------------------------------------------
#   EJERCICIO 3.4
#-----------------------------------------------
representarMediaSimetria <- function(valores){
    mean1 = valores[[1]][[1]]
    simetria1 = valores[[1]][[2]]

    mean5 = valores[[2]][[1]]
    simetria5 = valores[[2]][[2]]

    plot(x=mean5, y=simetria5, col = "red",pch=19, lwd = 2, main="Media y simetría de los cincos")
    plot(x=mean1, y=simetria1, col = "green",pch=19, lwd = 2, main="Media y simetría de los unos")
}

representarMediaSimetria(val)
pause()
#-----------------------------------------------
#   EJERCICIO 3.5
#-----------------------------------------------
Regress_Lin <- function(datos, label){
    pseudoinversa = solve(t(datos)%*%datos)%*%t(datos)

    wlin=(pseudoinversa%*%label)
    y_prima=datos%*%(wlin)

    list(wlin, y_prima)
}

#-----------------------------------------------
#   EJERCICIO 3.6
#-----------------------------------------------

ajusteRegresion <- function(media, simetria){
    regresion = Regress_Lin(media, simetria)
    plot(x=media, y=simetria, col = "green",pch=19, lwd = 2, main="Regresion")
    lines(x=media, y = regresion[[2]], col="blue", lwd=2, pch=19)
}
print(ajusteRegresion(val[[1]][[1]], val[[1]][[2]]))
pause()
print(ajusteRegresion(val[[2]][[1]], val[[2]][[2]]))
pause()


#-----------------------------------------------
#   EJERCICIO 3.7
#-----------------------------------------------
# Indicamos el tamaño de la muestra, el rango de los puntos
# y generamos la recta (o función f) que evaluará los puntos
tam = 100
rangoEJ7 = -10:10
rectaEJ7 = simula_recta(dim=rangoEJ7,verPunto = F)

# Función para calcular el error fuera de la muestra
calcularEout <- function(ein, datos, tam){
    Eout = ein + (ncol(datos))/tam
    Eout
}

# Función para calcular el error dentro de la muestra
calcularEin <- function(X, wlin, N, y){
    errorIN = (1/N)*(t(wlin)%*%t(X)%*%X%*%wlin - 
        2*t(wlin)%*%t(X)%*%y + 
        t(y)%*%y)
}

# Función para calcular la regresión lineal para la clasificación.
# En ella se devuelve el error dentro y fuera de la muestra
regresionLinealClasificacion <- function(){
  # Generamos una muestra y calculamos las etiquetas en función de 
  # la recta generada anteriormente
    puntosEJ7 = simula_unif(N=tam, dim=2, rango=rangoEJ7)
    etiquetasEJ7 = evaluaPuntos(puntosEJ7, rangoEJ7, apartado = 6)
  
  # Ajustamos la regresión lineal para los puntos y las etiquetas
    g = Regress_Lin(puntosEJ7, etiquetasEJ7)

    wlin = g[[1]]
  # Calculamos el error dentro de la muestra
    errorIN = (1/tam)*(t(wlin)%*%t(puntosEJ7)%*%puntosEJ7%*%wlin - 
        2*t(wlin)%*%t(puntosEJ7)%*%etiquetasEJ7 + 
        t(etiquetasEJ7)%*%etiquetasEJ7)
  # Y el error dentro de la muestra
    Eout = calcularEout(errorIN, puntosEJ7, tam)
    c(errorIN, Eout)
}

# Esta función se encarga de realizar todo lo necesario para 
# realizar el apartado c
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
pause()
cat("Media de Eout para la regresion lineal = ",meanEout,"\n")
pause()
cat("Numero medio de iteraciones para el PLA = ",meanIters,"\n")
pause()

#-----------------------------------------------
# EJERCICIO 3.8
#-----------------------------------------------
ejercicio8<-function(){
  # Generamos la muestra y la evaluamos según la función
  # mencionada en el enunciado. Tras esto, modificamos el 
  # 10% de las etiquetas
    puntosEJ8 = simula_unif(N=1000, dim=2, rango=(-10:10))
    etiquetasEJ8=evaluaPuntos(puntosAEvaluar=puntosEJ8, 
        intervalo=(-10:10), apartado=7)

    etiquetasEJ8=cambiarEtiqueta(etiquetasEJ8)

  # Ajustamos la regresión
    g=Regress_Lin(puntosEJ8,etiquetasEJ8)
  # Calculamos el error de la muestra
    Ein = calcularEin(X = puntosEJ8, wlin = g[[1]], N = 1000, y = etiquetasEJ8)
    Ein
}

Eins8 = vector(length=1000)
for (i in 1:1000) {
    Eins8[i]=ejercicio8()
}
ErrorMedioIn8 = mean(Eins8)
cat("Error medio en Ein del ejercicio 8",ErrorMedioIn8,"\n")
pause()

#-----------------------------------------------
apartado8B <- function(mostrar=F){
  # Generamos la muestra y la evaluamos según la función
  # mencionada en el enunciado. Tras esto, modificamos el 
  # 10% de las etiquetas
    puntosEJ8 = simula_unif(N=1000, dim=2, rango=(-10:10))
    etiquetasEJ8=evaluaPuntos(puntosAEvaluar=puntosEJ8, 
        intervalo=(-10:10), apartado=7)
    
    etiquetasEJ8=cambiarEtiqueta(etiquetasEJ8)
    
  # Esta función sirve para construir la matriz con 
  # (1,x1,x2,x1x2,x1^2,x2^2)
    f<-function(linea) c(1,linea[1],linea[2],linea[1]*linea[2],
        linea[1]*linea[1],linea[2]*linea[2])

  # Usamos la función appli para hacer el cálculo vectorizado
    nuevosPuntos=apply(X=puntosEJ8, FUN=f, MARGIN=1)
  # Como la matriz que obtenemos es de 1000 columnas y 6 filas
  # realizamos la traspuesta para obtener nuestra matriz de
  # 1000 filas y 6 columnas
    nuevosPuntos = t(nuevosPuntos)
  
  # Ajustamos la regresión
    g=Regress_Lin(nuevosPuntos, etiquetasEJ8)
    
  # En caso de querer mostrar la función, se muestra por pantalla
    if(mostrar){
        pintar(puntos=puntosEJ8, funciones[[7]], range, "EJERCICIO 8.b", 
        colores =  etiquetasEJ8 + 4, verFuncion = T)
        points(x=puntosEJ8[,1], y = g[[2]], col="blue", lwd=2, pch=19)
    }
  # Calculamos el error dentro y fuera de la muestra y devolvemos Eout
    Ein = calcularEin(X = nuevosPuntos, wlin = g[[1]],
        N = 1000, y = etiquetasEJ8)
    Eout = calcularEout(Ein, nuevosPuntos, 1000)
    Eout
}
apartado8B(T)
pause()
#-----------------------------------------------

Eouts8 = vector(length=1000)
for (i in 1:1000) {
    Eouts8[i]=ejercicio8()
}
ErrorMedioOut8 = mean(Eouts8)
cat("Error medio en Eout del ejercicio 8",ErrorMedioOut8,"\n")
pause()
