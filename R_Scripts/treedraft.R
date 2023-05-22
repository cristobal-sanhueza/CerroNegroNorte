library(rsample)
library(ipred)
library(MLmetrics)
library(AmesHousing)
library(tree)

### IMPORTAR DATASET

CNN1_MP10 <- read_csv("CerroNegroNorte/Data/Sep2019_Oct2022/CNN1_MP10.csv")

# USANDO TREE -------------------------------------------------------------

### Ahora automatizamos para hacer lo mismo con todas las horas.

for (i in 0:23) {
  
  
  tree.dataframe <- tree(MP10 ~ DV + HR + TEMP + VV,
                    control = tree.control(nobs = nrow(CNN1_MP10 %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = CNN1_MP10 %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4097 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("GeoAire/CerroNegroNorte/TreeModels/CNN1_", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
}

################################################################################################################

wb = createWorkbook()

sheet = createSheet(wb, paste("sheet", i))

addDataFrame(dataframe1, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Sheet 2")

addDataFrame(dataframe2, sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, "My_File.xlsx")


for (i in 0:23) {
  
  sheet = createSheet(wb, paste("sheet", i))
  
  
  tree.dataframe <- tree(MP10 ~ DV + HR + TEMP + VV,
                         control = tree.control(nobs = nrow(CNN1_MP10 %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = CNN1_MP10 %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4097 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  addDataFrame(as.matrix(tree.dataframe), sheet=sheet, startColumn=1, row.names=FALSE)
  
}

saveWorkbook(wb, file = "GeoAire/CerroNegroNorte/TreeModels/CNN1.xlsx")

################################################################################################################





















# USANDO CTREE ------------------------------------------------------------


png(file = "decision_tree.png")


output.tree <- ctree(
  MP10 ~ `Direccion viento (°)` + `Humedad Relativa (%)` + `Precipitacion (mm)` +
    `Radiacion solar (Watt/m²)` + `Temperatura (°C)` + `Velocidad del viento (m/s)`, 
  data = E3Data)

plot(output.tree)

View(output.tree)

nrow(SG_00)
SG_00 <- na.omit(SG_00)

miarbol <- ctree(
  MP10 ~ DIR + TEM + VEL,
  data = SG_00
)

png(file = "mi_arbol.png")
plot(miarbol)

# USANDO RPART ------------------------------------------------------------

mycontrol <- rpart.control(minsplit = 5,
                           minbucket = 10)

myfit <- rpart(MP10 ~ DIR + TEM + VEL, data = SG_00, method ='anova', control = mycontrol)

rpart.plot(myfit)

plotcp(myfit)

summary(myfit)

meanvar(myfit)

summary(residuals(myfit))

myres <- residuals(myfit, type = "deviance")

sumsqres <- sum(myres^2)

# residual mean deviance = sumsqres divided by (total number of obs - number of terminal nodes)
sumsqres/(1089-5)

######### EJEMPLO AMES HOUSING DATA RPART #####


set.seed(123)

ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)


ames_train <- training(ames_split) ## 70% of data
ames_test  <- testing(ames_split)  ## 30% of data

m1 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova"
)


rpart.plot(m1)

m3 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova", 
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid)

nrow(hyper_grid)

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = Sale_Price ~ .,
    data    = ames_train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}



get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


optimal_tree <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova",
  control = list(minsplit = 6, maxdepth = 10, cp = 0.01)
)


pred <- predict(optimal_tree, newdata = ames_test)

RMSE(y_pred = pred, y_true = ames_test$Sale_Price)




















