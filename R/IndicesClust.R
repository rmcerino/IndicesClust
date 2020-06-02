indices_clust <- function(data, min, max, m_fuzzy){

  require(clValid)
  require(cluster)
  require(devtools)
  require(e1071)
  require(tibble)
  require(caret)
  require(sf)
  require(expss)

  if(min < 2){stop("Min tiene que ser mayor o igual a 2")}

  aux <- na.omit(data)

  ## Centrar las variables - STOP if NA
  if(dim(aux)[1] != dim(data)[1]){
    stop("La base contiene NA, se deben eliminar para poder continuar")

  }else{
    clustesc1<- preProcess(data, method = c("center", "scale")) # Se estandariza las variables, por alguna tecnica de centrado puede se (variable  - media)/desviaci?n estandart
    clustesc2<- predict(clustesc1, data) # se predice o se calcula los datos estandarizado

  }

  # Creamos data-frame
  names <- c("XieBeni", "FukSug", "CoefPart_1", "EntrPart")
  Indices0 <- data.frame(names)

  names <- c("CoefPart")
  CoefPart <- data.frame(names)

  for(i in min:max){

    clustesc3<-clustesc2
    n_clases = i

    set.seed (7)
    clustesc4 <- cmeans(clustesc3 , n_clases, 100, method="cmeans", m = m_fuzzy)
    data[[paste("cz",i,sep="")]]  <- clustesc4$cluster

    I2CM <- fclustIndex(clustesc4,clustesc3, index=c("xie.beni", "fukuyama.sugeno",
                                                     "partition.coefficient", "partition.entropy"))

    I2CM <- as.data.frame(I2CM)

    Indices0[[paste("cz",i,sep="")]] <- I2CM$I2CM

    CoefPart[[paste("cz",i,sep="")]] <- 1/vlookup("CoefPart_1", Indices0, which(colnames(Indices0)==paste("cz",i,sep="")))

  }

  zonificacion <<- data

  Indices <- rbind(Indices0, CoefPart)
  Indices <- subset(Indices, names != "CoefPart_1")

  Indices <<- Indices

  options(scipen=999)
  tabla <- Indices


  for (i in 1:4) {
    for (j in 2:dim(Indices)[2]){
      tabla[i,j] <- ifelse(Indices[i,j] == min(Indices[i,-1]), "MIN", 0)
    }
  }

  tabla_final <<- tabla
  return(tabla_final)
}

