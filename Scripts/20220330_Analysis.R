#### Seed Analysis for CBS Undergraduate Research Conference #### 

# when you read the treatment
Treatment <- read.csv("Data/WE_Treatment.csv")

Treatment$Serpentine <- ifelse(Treatment$Plot %in% c(1,2,7:14,21,41:46,84), 
                               "HS", 
                               ifelse(Treatment$Serpentine == "N",
                                      Treatment$Serpentine,
                                      "LS"))
