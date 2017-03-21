library("Deriv")
library("ggplot2")
library("orthopolynom")
PI = 3.141592653589793

pause <-function(){
    print("Pulsar una tecla para continuar")
    scan(what=character(), n=1)
}

pintar <- function(puntos, funcion, intervalo, nombreGrafica, 
    colores = colors(), verFuncion = FALSE, aniadir=FALSE, colFunc = "black",k=0){
    par(bg="lightcyan")
    if(verFuncion){
        x <- y <- seq(range(puntos[,1])[1],range(puntos[,1])[2],length=100)
        z <- outer(x,y,funcion)
        contour(
            x=x, y=x, z=z,
            levels=k, las=1, drawlabels=FALSE, lwd=3, xlab="Eje X", 
            ylab="Eje y",main = nombreGrafica, add = aniadir, col = colFunc
        )
    }
    else{
        plot(intervalo, intervalo, xlab="Eje X", ylab="Eje y", type="n", 
            main = nombreGrafica)
    }

    points(puntos, col = colores, pch=19, lwd = 2)
}

simula_unif <- function(N=5,dim=2,rango=5:20){
    num = matrix(runif(N*dim, min=rango[1], max=rango[length(rango)]), 
      nrow = N, ncol = dim)
    num
}
simula_gaus <- function(N=5,dim=2,sigma=1){
    media = 0
    num = matrix(rnorm(N*dim, media, sigma), nrow = N, ncol = dim)
    num
}

simula_recta <- function(dim = -50:50, punto_1 = simula_unif(N=1,dim=2,rango=-1:1),
    punto_2 = simula_unif(N=1,dim=2,rango=-1:1), verPunto = T){
    # Por defecto, obtenemos dos puntos aleatorios dentro del intervalo,
    # pero en caso de que 
    if(verPunto){
        print("#####Punto 1")
        print(punto_1)
    
        print("#####Punto 2")
        print(punto_2)
    }

    punto_aux = c(punto_2[1]-punto_1[1], punto_2[2]-punto_1[2])

    ecuacion = c(punto_aux[2]/punto_aux[1], 
        (((-1*punto_1[1]*punto_aux[2])+(punto_1[2]*punto_aux[1]))/punto_aux[1]))
    ecuacion
}

evaluaPuntos <- function(puntosAEvaluar, func){
    etiquetas = apply(X=puntosAEvaluar, FUN=func, MARGIN=1)
    sign(etiquetas)
}

Ein <- function(datosEval, labels, resultPLA){
    ein = sign(datosEval%*%resultPLA)
    ein[ein==0]=1
    error = length(ein[ein != labels])/nrow(datosEval)
    error
}

PLA <- function(datos, label, max_iter = 3000, vini = c(0,0,0), verRecorrido = F){
    datos = cbind(rep(1, nrow(datos)), datos)
    i = 1
    mejorSol = vini
    porcentajeError = Ein(datos=datos, labels=label, resultPLA=mejorSol)
    cambiado = T
    while (i <= max_iter & cambiado){
        for(j in 1:nrow(datos)){
            signo = sign(datos[j,]%*% vini)
            if (signo == 0){
                signo = signo + 1
            }
            if(label[j] != signo){
                v_aux = vini + datos[j,]*label[j]
            }
        }
        nuevoPorcentaje = Ein(datos=datos, labels=label, resultPLA=v_aux)
        if(porcentajeError > nuevoPorcentaje){
            cat("Antiguo pocentaje de error = ",porcentajeError,"\n")
            cat("Nuevo porcentaje de error = ",nuevoPorcentaje,"\n")
            vini = v_aux
            porcentajeError = nuevoPorcentaje
        }
        else
            cambiado = F
        if(verRecorrido)
            abline(a=(-vini[1]/vini[3]),
                b=(-vini[2]/vini[3]), col="grey")
        i = i + 1
    }
    vini = (vini/vini[1])
    vini[3] = -vini[3]
    pla_result = c(vini, i, porcentajeError)
    pla_result
}

readFile <- function(fileName = "./DigitosZip/zip.train"){
    digit.train <- read.table(fileName, quote="\"", 
        comment.char="", stringsAsFactors=FALSE)

    digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
    digitos = digitos15.train[,1]
    ndigitos = nrow(digitos15.train)
    grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))
    rm(digit.train)
    rm(digitos15.train)
    # grises
    digitos[digitos == 5] = -1
    list(digitos, grises)
}

getSymmetry <- function(data){
    x = abs(data-data[nrow(data):1,])
    -sum(x)
}

intensityAndSymmetry <- function(data){
    n = nrow(data)
    intensity = apply(data[1:n,,],1, mean)
    symmetry = apply(data[1:n,,],1,getSymmetry)

    result = as.matrix(cbind(intensity, symmetry))
    result
}

Regress_Lin_Effic <- function(datos, label){
    b1 = sum((datos-mean(datos)) * (label - mean(label))) / sum((datos-mean(datos))^2)
    b0 = mean(label) - b1*mean(datos)
    c(b0, b1)
}

du <- function(u,v) 2*(u*exp(v) - 2*v*exp(-u))*(exp(v)+2*v*exp(-u))
dv <- function(u,v) 2*(u*exp(v) - 2*v*exp(-u))*(u*exp(v)-2*exp(-u))
fo <- function(u,v) (u*exp(v)-2*v*exp(-u))*(u*exp(v)-2*v*exp(-u))
fb <- function(x,y) x*x +2*y*y + 2*sin(2*PI*x)*sin(2*PI*y)


#------------------------------------------------------------------------------
#  EJERCICIO 1.1
#------------------------------------------------------------------------------

gradienteDescAnalitico <- function(x = 1,y = 1, eta = 0.1, 
    prec=10^(-14), maxIter = 50, showIter = F){
    xOld = 0
    yOld = 0
    nIter = 0
    # Mientras que no se haya alcanzado el error máximo que queremos o 
    # no se haya llegado al número total de iteraciones
    while (abs(du(x,y)) > prec & nIter < maxIter & 
        abs(xOld - x) > prec){
        # Guardamos los valores de X e Y
        xOld = x
        yOld = y
        # Actualizamos los valores de X e Y con las derivadas de su función
        # junto con el factor de aprendizaje
        x = xOld - eta*du(xOld,yOld)
        y = yOld - eta*dv(xOld,yOld)
        # Incrementamos el número de iteraciones
        nIter = nIter+1
    }
    # Si no queremos ver el número de iteraciones, devolverá 
    # el valor de la función en el punto, junto con el
    # valor de X y el valor de Y
    if(!showIter)
        c(fo(x,y),x, y)
    # Si queremos ver el número de iteraciones, realiza lo mismo que antes
    # pero devolviendo como último valor el número de iteraciones
    else
        c(fo(x,y),x, y, nIter)
}

print(gradienteDescAnalitico(showIter = T))
pause()
#------------------------------------------------------------------------------
#  EJERCICIO 1.1.1
#------------------------------------------------------------------------------
gradDesc <- function(x = 1,y = 1, eta = 0.1, func,
    prec=10^(-14), maxIter = 50, showIter = F, 
    pintarGr=F, nombre="Gradiente Descendente"){
    df = Deriv(f=func,x=formalArgs(func))
    xOld = 0
    yOld = 0
    nIter = 0
    # Estas listas contendrán los puntos que va encontrando
    # el gradiente durante el algoritmo
    xs = c()
    ys = c()
    while (abs(df(x,y)[1]) > prec & nIter < maxIter & 
        abs(x - xOld) > prec){
        xOld = x
        yOld = y
        # Insertamos los puntos al final
        xs = c(xs, x)
        ys = c(ys, y)
        newValues = df(xOld,yOld)
        x = xOld - eta*newValues[1]
        y = yOld - eta*newValues[2]
        nIter = nIter+1
    }
    # Generamos la matriz de puntos
    if(pintarGr){
      # Si el flag pintarGr está a True, se pintan los puntos
      pts = cbind(xs,ys)
      pintar(puntos = pts, funcion = func, intervalo=c(-2,2), colores="red",
            verFuncion=T, nombreGrafica=nombre, k=1:20)
    }
        
    if(!showIter)
        c(func(x,y),x, y)
    else
        c(func(x,y),x, y, nIter)
}

print(gradDesc(func=fo,prec=10^(-14),showIter = T))
pause()
print(gradDesc(func=fb,eta = 0.01,pintarGr=T,nombre="Gradiente Descendiente con eta = 0.01"))
pause()
print(gradDesc(func=fb,eta = 0.1,showIter=T,pintarGr=T,nombre="Gradiente Descendiente con eta = 0.1"))
pause()

#------------------------------------------------------------------------------
#  EJERCICIO 1.2
#------------------------------------------------------------------------------
coordinateDescent <- function(x = 1, y = 1, eta = 0.1, func, prec = 10^(-14),
    maxIter = 50, showIter = F, pintarGr=F, nombre = "Gradiente Descendiente"){
    df = Deriv(f=func,x=formalArgs(func))
    xOld = 0
    yOld = y
    nIter = 0
    xs = c()
    ys = c()
    while (abs(df(x,y)[1]) > prec & nIter < maxIter){
      # Almacenamos los valores de x e y en estas variables
      # auxilares para usarlas más adelante
        xOld = x
        xs = c(xs, x)
        ys = c(ys, y)
      # Calculamos el nuevo valor de X usando la derivada
        newValues = df(xOld,yOld)
        x = xOld - eta*newValues[1]
      # Calculamos el nuevo valor de Y usando la derivada, 
      # pero con el valor nuevo de X
        newValues = df(x,yOld)
        y = yOld - eta*newValues[2]
        nIter = nIter+1
    }    
    pts = cbind(xs,ys)
    if(pintarGr)
        pintar(puntos = pts, funcion = func, intervalo=c(-2,2), colores="red",
            verFuncion=T, nombreGrafica=nombre, k=1:20)
    if(!showIter)
        c(func(x,y),x, y)
    else
        c(func(x,y),x, y, nIter)
}

print(coordinateDescent(func=fo,eta = 0.1,maxIter = 15,showIter = T))
pause()
print(gradDesc(func=fo,eta = 0.1,maxIter = 15,showIter = T))
pause()

print(coordinateDescent(func=fo,eta = 0.01,maxIter = 15,showIter = T))
pause()
print(gradDesc(func=fo,eta = 0.01,maxIter = 15,showIter = T))
pause()
print(coordinateDescent(func=fo,eta = 0.01,maxIter = 50,showIter = T))
pause()
print(gradDesc(func=fo,eta = 0.01,maxIter = 50,showIter = T))
pause()

#------------------------------------------------------------------------------
#  EJERCICIO 1.3
#------------------------------------------------------------------------------

newtonMethod <- function(x = 0.1, y = 0.1,prec=10^{-14}, 
  func, maxIter = 50,showIter = T, pintarGr=F, nombre = "Metodo de Newton"){
  # Calculamos la derivada primera de la función
    df1 = Deriv(f=func,x=formalArgs(func))
  # Calculamos la derivada segunda de la función
    df2 = Deriv(f=func,x=formalArgs(func),nderiv=2)
  # Inicializamos los puntos y el contador de iteraciones
    xOld = 0
    yOld = 0
    xs = c()
    ys = c()
    nIter = 0
    itsTimeToStop1 = itsTimeToStop2 = itsTimeToStop3 = F
  # E iniciamos el bucle
    while(!itsTimeToStop1 & !itsTimeToStop2 & !itsTimeToStop3){
        xOld = x
        yOld = y
        
        # Calculamos el gradiente con la primera derivada
        newValues_1 = df1(xOld,yOld)
        
        # Calculamos el gradiente con la segunda derivada
        newValues_2 = df2(xOld,yOld)

        # Actualizamos los puntos
        xy = solve(matrix(newValues_2, ncol=2))%*%newValues_1

        x = xOld - xy[1,1]
        y = yOld - xy[2,1]
        
        
        if(abs(func(xOld, yOld) - func(x, y)) < prec){
          itsTimeToStop1 = T
          print("Me salgo porque estoy en un minimo local")
        }
        else if(nIter >= maxIter-1){
          itsTimeToStop2 = T
          print("Me salgo porque me he pasado de iteraciones")
        }
        else if(norm((as.matrix(df1(x,y))), type = "F") < prec){
          itsTimeToStop3 = T
          print("Me salgo porque he minimizado por debajo del umbral")
        }            
        
        xs = c(xs, nIter)
        ys = c(ys, func(x,y))
        nIter = nIter + 1
    }
    if(pintarGr){
        pts = cbind(xs,ys)
        #pintar(puntos = pts, funcion = func, intervalo=c(-2,2), colores="red",
        #    verFuncion=T, nombreGrafica=nombre)
        plot(pts, type = "l")
    }
    if(!showIter)
        c(func(x,y),x, y)
    else
        c(func(x,y),x, y, nIter)
}

print(newtonMethod(func = fb, pintarGr = T))
pause()
print(newtonMethod(x = 1, y = 1, func = fb, pintarGr = T, nombre = "P. inicio = (1,1)"))
pause()
print(newtonMethod(x = -0.5, y = -0.5, func = fb, pintarGr = T, nombre = "P. inicio = (-0.5,-0.5)"))
pause()
print(newtonMethod(x = -1, y = -1, func = fb, pintarGr = T, nombre = "P. inicio = (-1,-1)"))
pause()

#------------------------------------------------------------------------------
#  EJERCICIO 2
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#  EJERCICIO 2.1
#------------------------------------------------------------------------------
sobreajuste <- function(intervalo=-1:1, prob=0.5, N=100, sigma=1, Qf=15, ejercicio1=TRUE) {
    # Calculamos los coeficientes pertenecientes a la 
    # distribución de probabilidad con media = 1/2
    aq = as.vector(simula_gaus(N=Qf+1, dim=1, sigma=1))
    # Aplicamos a los coeficientes la siguiente función
    #
    #                        a
    #    a =   ----------------------------
    #               ----------------------
    #              /  ---- Qf      1
    #             /   \        ---------
    #            V    /___ q=0   2q+1
    # 
    d = sqrt(sum(sapply(X=0:Qf, FUN=function(q) 1/(2*q+1))))
    aq = aq/d
    # Y realizamos el cálculo de la función objetivo, que consiste en
    #
    #              ---- Qf  
    #        f =   \         aqLq(x)
    #              /___ q=0 
    #     
    legendre = legendre.polynomials(n=Qf, normalized=T) # generamos los 20 primeros
    f = legendre[[1]]*aq[1]
    for (i in 2:length(legendre)) 
        f = f + legendre[[i]] * aq[i]
    # generamos los datos
    x = as.vector(simula_unif(N=N, dim=1, rango=intervalo))
    # generamos epsilon para meter ruido
    eps = as.vector(simula_gaus(N=N, dim=1, sigma=0.5))
    # Generamos la función objetivo
    Y = sapply(X=x, FUN=as.function(f))
    # 2. le sumamos a los Y calculados, sigma*epsilon
    yn = mapply(FUN=function(valor, en) valor + sigma*en, valor=Y, en=eps)
    # generamos la matriz de datos
    datos = cbind(x, yn)
    # Transformamos los datos para realizar la regresión
    datos_predecir = poly(x, degree=Qf)
    # Realizamos la regresión para g2 y g10
    model.g2 <- lm(yn ~ poly(x, degree=2),data=datos_predecir)
    model.g10 <- lm(yn ~ poly(x, degree=10),data=datos_predecir)
    # Almacenamos los pesos
    w2 = as.vector(model.g2$'coefficients')
    w10 = as.vector(model.g10$'coefficients')
    # Calculamos el error dentro de la muestra, similar al 
    # error de entropía  cruzada 
    EinNL <- function(datos, w, etiquetas){
        w=as.matrix(w)
        datos = cbind(rep(1, nrow(datos)), datos)
        y_z = apply(X=datos, FUN=function(dato) t(w)%*%dato, MARGIN=1)
        mean(mapply(FUN = function(yz, y) log(1 + exp(-y * yz)), yz = y_z, y = etiquetas))

    }
    # Calculamos los errores dentro de la muestra
    eing2 = EinNL(poly(x, degree=2), w2, yn)
    eing10 = EinNL(poly(x, degree=10), w10, yn)
    # Y los errores fuera de la muestra
    eoutg2 = EoutBound(eing2, N=nrow(datos), delta = 0.05, d = ncol(poly(x,degree=2)))
    eoutg10 = EoutBound(eing10, N=nrow(datos), delta = 0.05, d = ncol(poly(x,degree=10)))
    
    if(ejercicio1){
        cat("Ein_g2 = ",eing2,"\tEout_g2 = ",eoutg2,"\n")
        cat("Ein_g10 = ",eing10,"\tEout_g10 = ",eoutg10,"\n")
        cat("Eout_g2 - Eout_g10 = ", eoutg2 - eoutg10,"\n")

        (ggplot() + geom_point(data=as.data.frame(datos), aes(x=x, y=yn)) 
          + geom_line(aes(x=x, y=predict(model.g2), color="g2")) 
          + geom_line(aes(x=x, y=predict(model.g10), color="g10")))
    }
    else
            l = c(eing2, eoutg2, eing10, eoutg10)
}

print(sobreajuste())
pause()

#------------------------------------------------------------------------------
#  EJERCICIO 2.2
#------------------------------------------------------------------------------

ejercicio22 <- function(){
    EoutH2 = c()
    EoutH10 = c()
    
    for(i in 1:150){
        l = sobreajuste (intervalo=-1:1, prob=0.5, N=50, sigma=1, Qf=15, ejercicio1=F)
        # Insertamos los errores fuera de la muestra en cada una de las listas
        EoutH2 = c(l[[2]],EoutH2)
        EoutH10 = c(l[[4]],EoutH10)
    }
    # Restamos las medias de los errores para los dos modelos
    mean(EoutH10) - mean(EoutH2)
}

print(ejercicio22())
pause()

# Función para implementar la regresión con "weight decay"
weightDecay <- function(datos, y, lambda = 0.05/nrow(datos)){
    beta = solve(t(datos)%*%datos+lambda*diag(ncol(datos)))%*%t(datos)%*%y   
}

# Función para calcular el error cuadrático
EinSquareError<-function(w, x, y){
    w=as.matrix(w)
    (t(w)*x-y)*(t(w)*x-y)
}

ejercicio3 <- function(N = 100, varianza = 1, lambda = 0.5, d = 2){
    # Generamos el vector aleatorio de pesos
    w = as.vector(simula_gaus(N=1, dim=d+1, sigma = 1))
    # Y una lista vacía para almacenar los errores
    CVErrorsList = c()
    e1 = c()
    e2 = c()
    # La lista (d+15,d+25,...,d+115)
    Ns = seq(from = 15, to = 115, by = 10)
    # Iniciamos la validación cruzada
    for (i in Ns){
        # Con el nuevo conjunto de datos de tamaño Ns_i + d
        n = i+d
        # Generamos una nueva muestra, el ruido y los evaluamos en función
        # de los pesos que tenemos
        newDS = simula_gaus(N=n, dim=d, sigma = 1)
        newDSaux=cbind(rep(1,n),newDS)
        epsilon = as.vector(simula_gaus(N=n, dim=1, sigma=1))
        ynDS = apply(X = newDSaux, FUN = function(dato) dato%*%w,MARGIN=1)
        ynDS = mapply(FUN=function(e,y) y + e*lambda/n, e = epsilon, y=ynDS)
        # Tras esto, aplicamos la validación cruzada e insertamos el error
        # en la lista CVErrorsList
        ei = sapply(X=1:n,FUN=function(k) EinSquareError(
            as.vector(weightDecay(newDSaux[-k,],ynDS[-k])), 
            x=newDSaux[k,], y = ynDS[k]))
        CVErrorsList = c(mean(ei), CVErrorsList)
        e1 = c(ei[1],e1)
        e2 = c(ei[2],e2)
    }
    list(CVErrorsList,e1,e2)
}
cve = ejercicio3()
print(cve)
pause()