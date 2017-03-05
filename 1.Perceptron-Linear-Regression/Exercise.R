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