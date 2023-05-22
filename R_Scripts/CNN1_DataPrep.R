library(dplyr)
library(readr)
library(lubridate)

# SET WORKING DIRECTORY
setwd("/Users/cristobal512/Desktop/geoaire")

# IMPORT MP10 DATA
MP10_MP25 <- read_csv("CerroNegroNorte/Data/calidad-aire-dia-hora_CNN1_01-09-2019_a_31-10-2022.csv",
                                                               col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

# CREAR COLUMNA CON FECHA Y HORA JUNTOS
MP10_MP25 <- MP10_MP25 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "),
                                  format = "%Y-%m-%d %H"))

# ELMINAR COLUMNAS: FECHA Y HORA (POR SEPARADO)
MP10_MP25 <-  MP10_MP25 %>% 
  select(-c(1:2))

# REORDENAR COLUMNAS
MP10_MP25 <-  MP10_MP25 %>% 
  relocate(DateAndTime)

# RENAME COLUMNS
MP10_MP25 <- MP10_MP25 %>% 
  rename(MP10 = `MP10 (µg/m³N)`,
         MP25 = `MP2,5 (µg/m³)`
         )

# IMPORT DATA METEOROLOGICA
Meteorologia <- read_csv("CerroNegroNorte/Data/Sep2019_Oct2022/DR_01-09-2019_a_31-10-2022.csv",
                         col_types = cols(Fecha = col_date(format = "%d/%m/%Y")),
                         locale = locale(decimal_mark = ",", grouping_mark = "."))


# CREAR COLUMNA CON FECHA Y HORA JUNTOS
Meteorologia <- Meteorologia %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%Y-%m-%d %H"))


# ELMINAR COLUMNAS: FECHA Y HORA (POR SEPARADO)
Meteorologia <-  Meteorologia %>% 
  select(-c(1:2, 4, 6))

# REORDENAR COLUMNAS
Meteorologia <-  Meteorologia %>% 
  relocate(DateAndTime)

# RENAME COLUMNS
Meteorologia <- Meteorologia %>% 
  rename(DV = `Direccion viento (°)`,
         HR = `Humedad Relativa (%)`,
         TEMP = `Temperatura (°C)`,
         VV = `Velocidad del viento (m/s)`)


# CREAR NUEVO DATAFRAME CON DATA COMBINADA DE METEO Y MP10
CNN1_Met_MP10_MP25 <- Meteorologia %>% 
  inner_join(MP10_MP25, by = "DateAndTime")

# ELIMINAR FILAS CON NA's en DateAndTime COLUMN
CNN1_Met_MP10_MP25 <- CNN1_Met_MP10_MP25 %>% 
  filter(!is.na(DateAndTime))

# CREAR UN DATAFRAME PARA CADA CONTAMINANTE (MP10 YY MP2.5)
CNN1_MP10 <- CNN1_Met_MP10_MP25 %>% 
  select(-c(7))

CNN1_MP25 <- CNN1_Met_MP10_MP25 %>% 
  select(-c(6))

# DESMENUZAR FECHA MP10
CNN1_MP10 <- CNN1_MP10 %>% 
  mutate(Year = year(DateAndTime),
         Month = month(DateAndTime),
         Day = day(DateAndTime),
         Hour = hour(DateAndTime))

# RELOCATE VARIABLES MP10
CNN1_MP10 <- CNN1_MP10 %>% 
  relocate(Year, .after = DateAndTime) %>% 
  relocate(Month, .after = Year) %>% 
  relocate(Day, .after = Month) %>% 
  relocate(Hour, .after = Day)

# DESMENUZAR FECHA MP25
CNN1_MP25 <- CNN1_MP25 %>% 
  mutate(Year = year(DateAndTime),
         Month = month(DateAndTime),
         Day = day(DateAndTime),
         Hour = hour(DateAndTime))

# RELOCATE VARIABLES MP25
CNN1_MP25 <- CNN1_MP25 %>% 
  relocate(Year, .after = DateAndTime) %>% 
  relocate(Month, .after = Year) %>% 
  relocate(Day, .after = Month) %>% 
  relocate(Hour, .after = Day)


####################################### REMOVE NA'S (OPTIONAL) ####################################### 

# SUM NUMBER OF NA's IN EACH COLUMNS
sum(is.na(CNN1_Met_MP10_MP25$DV))
# 5156
sum(is.na(CNN1_Met_MP10_MP25$HR))
# 7297
sum(is.na(CNN1_Met_MP10_MP25$TEMP))
# 7297
sum(is.na(CNN1_Met_MP10_MP25$VV))
# 6392
sum(is.na(CNN1_Met_MP10_MP25$MP10))
# 3220
sum(is.na(CNN1_Met_MP10_MP25$MP25))
# 3181

# CHECK HOW MANY ROWS CONTAIN AT LEAST 1 NA VALUE
sum(apply(CNN1_Met_MP10_MP25, 1, function(row) any(is.na(row))))

# CHECK HOW MANY ROWS CONTAIN MORE THAN TWO NA VALUES
sum(apply(CNN1_Met_MP10_MP25, 1, function(row) sum(is.na(row)) > 2))

# ONLY KEEP ROWS WITH 2 OR LESS NA VALUES
CNN1_Met_MP10_MP25_filtered <- CNN1_Met_MP10_MP25[rowSums(is.na(CNN1_Met_MP10_MP25)) <= 2, ]

# CHECK THE NUMBER OF NA's IN EACH COLUMN
colSums(is.na(CNN1_Met_MP10_MP25_filtered))

#####################################################################################################

# EXPORT BOTH FILES TO CSV

write_csv(CNN1_MP10, file = "CerroNegroNorte/Data/CNN1_MP10.csv", na = '')

write_csv(CNN1_MP25, file = "CerroNegroNorte/Data/CNN1_MP25.csv", na = '')

#####################################################################################################

### para el árbol

CNN1_MP10 <- CNN1_Met_MP10_MP25_filtered %>% 
  select(-c(7)) %>% 
  mutate(Hora = hour(DateAndTime))















































































































































































































































































