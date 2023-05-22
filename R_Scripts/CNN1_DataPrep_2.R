library(readr)
library(readxl)
library(tidyverse)



## DV

DV_EneroAbril2023 <- read_excel("Data/Enero_a_Abril_2023/Meteorologia/DireccionViento.xlsx")

DV_EneroAbril2023$Fecha <- as.Date(DV_EneroAbril2023$Fecha, format = '%d/%m/%Y')

DV_EneroAbril2023 <- DV_EneroAbril2023 %>% 
  pivot_longer(cols = -Fecha,
               names_to = 'Hora',
               values_to = 'DV',
               names_pattern = "(\\d+)")




## VV

VV_EneroAbril2023 <- read_excel("Data/Enero_a_Abril_2023/Meteorologia/VelocidadViento.xlsx")

VV_EneroAbril2023$Fecha <- as.Date(VV_EneroAbril2023$Fecha, format = '%d/%m/%Y')

VV_EneroAbril2023 <- VV_EneroAbril2023 %>% 
  pivot_longer(cols = -Fecha,
               names_to = 'Hora',
               values_to = 'VV',
               names_pattern = "(\\d+)")

## MP10 ADM

ADM_MP10 <- read_excel("Data/Enero_a_Abril_2023/MP10/ADM/ADM_MP10.xlsx")

ADM_MP10$Fecha <- as.Date(ADM_MP10$Fecha, format = '%d/%m/%Y')

ADM_MP10 <- ADM_MP10 %>% 
  pivot_longer(cols = -Fecha,
               names_to = 'Hora',
               values_to = 'MP10',
               names_pattern = "(\\d+)")


## MP10 BONE

Bone_MP10 <- read_excel("Data/Enero_a_Abril_2023/MP10/Bone/Bone_MP10.xlsx")

Bone_MP10$Fecha <- as.Date(Bone_MP10$Fecha, format = '%d/%m/%Y')

Bone_MP10 <- Bone_MP10 %>% 
  pivot_longer(cols = -Fecha,
               names_to = 'Hora',
               values_to = 'MP10',
               names_pattern = "(\\d+)")


## MP10 CHANCADO

Chancado_MP10 <- read_excel("Data/Enero_a_Abril_2023/MP10/Chancado/Chancado_MP10.xlsx")

Chancado_MP10$Fecha <- as.Date(Chancado_MP10$Fecha, format = '%d/%m/%Y')

Chancado_MP10 <- Chancado_MP10 %>% 
  pivot_longer(cols = -Fecha,
               names_to = 'Hora',
               values_to = 'MP10',
               names_pattern = "(\\d+)")


## COMBINE


























































































































































