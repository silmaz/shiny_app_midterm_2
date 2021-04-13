
###########


preparation_data_prov <- function(dataset){
  
  
  italy_map <- map_data("italy")
  
  dataset <- as.data.frame(dataset)
  dataset[is.na(dataset)] = 0
  DATE_province <- substring(as.character(dataset$data[1]), 1, 10)   
  
  dataset <- dataset[,-c(1,2,3,5,7,11,12,13,14)]
  
  dataset[is.na(dataset)] = 0
  dataset[dataset == 0.0 ] = NA
  dataset=na.omit(dataset)
  
  rownames(dataset) = c(1:length(dataset$denominazione_provincia))
  
  
  # Barletta-Andria-Trani in Bari
  dataset$totale_casi[69] <- dataset$totale_casi[69] + dataset$totale_casi[73]
  # Crotone in Catanzaro
  dataset$totale_casi[8] <- dataset$totale_casi[8] + dataset$totale_casi[10]
  # Fermo in Ascoli Piceno
  dataset$totale_casi[54] <- dataset$totale_casi[54] + dataset$totale_casi[55]
  # Lecco in Beragamo
  dataset$totale_casi[43] <- dataset$totale_casi[43] + dataset$totale_casi[48]
  # Lodo in Milano
  dataset$totale_casi[42] <- dataset$totale_casi[42] + dataset$totale_casi[49]
  # monza brianza in milano
  dataset$totale_casi[42] <- dataset$totale_casi[42] + dataset$totale_casi[50]
  # prato in Firenze
  dataset$totale_casi[91] <- dataset$totale_casi[91] + dataset$totale_casi[97]
  # rimini in forli
  dataset$totale_casi[24] <- dataset$totale_casi[24] + dataset$totale_casi[25]
  # sud sardegli in cagliari
  dataset$totale_casi[76] <- dataset$totale_casi[76] + dataset$totale_casi[78]
  # Verbano-Cusio-Ossola in novara
  dataset$totale_casi[62] <- dataset$totale_casi[62] + dataset$totale_casi[67]
  # Vibo Valentia in catanzaro
  dataset$totale_casi[8] <- dataset$totale_casi[8] + dataset$totale_casi[11]
  # biella in vercelli
  dataset$totale_casi[61] <- dataset$totale_casi[61] + dataset$totale_casi[66]
  
  dataset <- dataset[-c(73,67,10,11,55,48,49,50,97,25,78,66),]
  
  # da Forlì-Cesena a Forlì
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Forlì-Cesena","Forli'")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Reggio di Calabria","Reggio Calabria")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Reggio nell'Emilia","Reggio Emilia")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Bolzano","Bolzano-Bozen")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Massa Carrara","Massa-Carrara")
  
  
  rownames(dataset) = c(1:length(dataset$denominazione_provincia))
  
  
  new_order <- rep(NA, length(dataset$totale_casi))
  prov_order <- unique(italy_map$region)
  for(j in 1:length(prov_order)){
    for(i in 1:length(dataset$totale_casi)) {
      if(dataset$denominazione_provincia[i] == prov_order[j]){
        new_order[j] <- i
      }
    }
  }
  
  new_data_province <- data.frame(dataset[new_order,])
  rownames(new_data_province) = c(1:length(new_data_province$denominazione_provincia))
 
  return(new_data_province)
}





###############

preparation_data_regioni <- function(data_regioni){

  data_regioni <- data_regioni[,-c(2,3,16,21,23,24,29,30)]
  data_regioni[is.na(data_regioni)] = 0

  
  for (i in 5:22){
    data_regioni[13,i] <- sum(data_regioni[12,i],data_regioni[13,i])
  }
  data_regioni <- data_regioni[-c(12),]
  rownames(data_regioni) = c(1:20)
  
  it <- map_data("italy")
  
  popolazione <- c(1293941, 553254, 1894110, 5712143, 4464119,
                   1206216, 5755700, 1524826, 10027602, 1512672,
                   300516, 4311217, 3953305, 1611621, 4875290, 3692555,
                   1078069, 870165, 125034, 4879133)
  data_regioni <- data.frame(data_regioni, popolazione)
  
  DATE_regioni <- substring(as.character(data_regioni$data[1]), 1, 10)   
  data_regioni <- data_regioni[,-c(1)]

  Region = c("Abruzzo","Basilicata","Calabria","Campania",
             "Emilia-Romagna","Friuli-Venezia Giulia","Lazio",
             "Liguria","Lombardia","Marche","Molise","Piemonte",
             "Puglia","Sardegna","Sicilia","Toscana",
             "Trentino-Alto Adige","Umbria","Valle d\'Aosta","Veneto")

  data_regioni$denominazione_regione <- str_replace_all(data_regioni$denominazione_regione,"Friuli Venezia Giulia","Friuli-Venezia Giulia")
  data_regioni$denominazione_regione <- str_replace_all(data_regioni$denominazione_regione,"P.A. Trento","Trentino-Alto Adige")
  
  new_order <- rep(NA, length(data_regioni$denominazione_regione))
  for(j in 1:length(Region)){
    for(i in 1:length(data_regioni$denominazione_regione)) {
      if(data_regioni$denominazione_regione[i] == Region[j]){
        new_order[j] <- i
      }
    }
  }
  data_regioni <- data.frame(data_regioni[new_order,])
  rownames(data_regioni) = c(1:length(data_regioni$denominazione_regione))
  
  
  popolazione_per_casi <- rep(NA, length(data_regioni$totale_positivi))
  caso_per_10m <- rep(NA, length(data_regioni$totale_positivi))
  for(i in 1:length(data_regioni$totale_positivi)){
    popolazione_per_casi[i] <- as.integer(data_regioni$popolazione[i]/data_regioni$totale_positivi[i])
    caso_per_10m[i] <- as.integer(10000/popolazione_per_casi[i])
  }
  data_regioni <- data.frame(data_regioni, popolazione_per_casi,caso_per_10m)
  
  return(data_regioni)
}








