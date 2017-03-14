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