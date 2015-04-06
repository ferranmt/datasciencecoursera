#COURSERA ASSIGNMENT 3

#Primer, llegim la taula de outcome-of-care-measures.
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#Ara, observem com esta organitzada i mirem el n° de files i columnes que te la taula.

head(outcome)
ncol(outcome)
nrow(outcome)

#1. PLOT THE 30-DAY MORTALITY RATES FOR HEARTH ATTACK:

#Fem un histograma de 30-day-death rates (és la coumna numero 11 del dataset),
#pero abans, com que hem dit que les "colclasses" són characters, hem de fer que 
#la columna sigui un vector numeric:

outcome[, 11] <- as.numeric(outcome[, 11])

#Ara ja podem fer l'histogram:

hist(outcome[, 11])

#2. FINDING THE BEST HOSPITAL IN A STATE

#Volem saber, segons l'Estat, quin hospital es el millor (te un numero més baix)
#de "heart attack", "pneumonia" i "heart failure"(outcomes), de manera que al escriure la
#funció posant estat i outcome et doni el nom de l'hospital.
#Hospitals sense dades per algun d'aquests outcomes han de quedar exclosos
#Si hi ha empat, han de sortir el primer per ordre alfabètic.

#Escrivim la funció:
best<-function(state, outcome) {
  
  #Llegim les dades de l'arxiu (recordar que l'arxiu ha d'estar guardat al 
  #directori on estem treballant):
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Filtrem i simplifiquem el nom de les columnes:
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  #Validem la variable de l'outcome:
  outcomes= c("heart attack", "heart failure", "pneumonia")
  if (outcome %in% outcomes == FALSE) stop ("invalid outcome")
  
  # Validem la variable State:
  states <- data[, 2]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  
  # Prenem només les files que tenen state value:  
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  rowNum <- which.min(vals)
  
  ## Retornem el nom de l'hospital en aquell estat amb el numero menor de 30-days-death-rate:
  data[rowNum, ]$name
  
}

#3. RANKING HOSPITALS BY OUTCOME IN A STATE
#Escriure una funció que tingui 3 arguments: state, outcome i el ranking de l'hospital
#en aquest state per aquest determinat outcome. Per exemple, rankhospital ("MD", "heart failure, 5)
#tornarà el nom del 5é hospital de l'estat "MD" amb número mes baix de heart failure.
#L'argument num pot pendre els valors de "best", "worst" o un numero.
#Si el numero donat es mes gran que el num d'hospitals en aquell estat, ha de retornar NA.
#Hospitals sense dades en aquell outcome han de ser exclosos.
#Si hi ha empat, es dona l'hospital mes ben classificat per ordre alfabetic.

#Escrivim la funció:
rankhospital <- function(state, outcome, num) {
  
  #Llegim les dades de l'arxiu:
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Filtrem i simplifiquem el nom de les columnes:
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  #Validem la variable de l'outcome:
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  # Validem la variable "state":
  states <- data[, 2]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  
  #Validem la variable "num":
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  # Prenem només les files que tenen state value: 
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  
  #Ordenem les dades.
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  # Processem l'argument num:
  vals <- data[, outcome]
  if( num == "best" ) {
    rowNum <- which.min(vals)
  } else if( num == "worst" ) {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ### Retornem el nom de l'hospital en aquell estat amb el numero menor de 30-days-death-rate:
  data[rowNum, ]$name
}

#4. RANKING HOSPITALS IN ALL STATES

#Escriure una funció que tingui 2 arguments: outcome i hospital ranking. 
#Retorna un data.frame de 2 columnes, i conté l'hospital a 
#cada estat que té el ranking d'un número específic.
#L'argument num pot pendre els valors de "best", "worst" o un numero.
#La funció dona un valor per a cada estat (que en algun cas pot ser NA)
#Si el numero donat es mes gran que el num d'hospitals en aquell estat, ha de retornar NA.
#La primera columna del data.frame es diu hospital, i la segona state.
#Hospitals sense dades per algun d'aquests outcomes han de quedar exclosos
#Si hi ha empat, es dona l'hospital mes ben classificat per ordre alfabètic.

#Escrivim la funció:
rankall <- function(outcome, num = "best") {
  
  #Llegim les dades de l'arxiu:
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Filtrem i simplifiquem el nom de les columnes:
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  #Validem la variable de l'outcome:
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  #Validem la variable "num":
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  # Prenem només les files que tenen dades al nostre outcome: 
  data <- data[data[outcome] != 'Not Available', ]
  
  #Ordenem les dades.
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Processem l'argument num
  getHospByRank <- function(df, s, n) {
    df <- df[df$state==s, ]
    vals <- df[, outcome]
    if( n == "best" ) {
      rowNum <- which.min(vals)
    } else if( n == "worst" ) {
      rowNum <- which.max(vals)
    } else {
      rowNum <- n
    }
    df[rowNum, ]$name
  }
  
  # Per a cada estat, trobem l'hospital amb un determinat ranking:
  states <- data[, 2]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for(st in states) {
    hosp <- getHospByRank(data, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  }
  
  # Retornem un data.frame amb els noms dels hospitals i el nom de l'estat.
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  newdata
}