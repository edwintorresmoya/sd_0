# sd_0
Function to create a new table whit variables with standar deviation != 0


########## Funcion para saber si existe desviación estandar 0

desviacion_0 = function(base){
    tabla_sd = as.data.frame(matrix(ncol = ncol(base)))
    colnames(tabla_sd) = colnames(base)
    for (i in 1: ncol(base)){
        a = (sd(base[,i]) != 0)
        tabla_sd[1,i] = a
    }
    tru == which(tabla_sd == T)
    print(tabla_sd[,-tru])
    return(list( basCorr = base[,tru], sdno0 = names(base[,tru]), sd0 = names(base[,-tru])))
}

class(dummy)

a = desviacion_0(dummy)
a$sd0
