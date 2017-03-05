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