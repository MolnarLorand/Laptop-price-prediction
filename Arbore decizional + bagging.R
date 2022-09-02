library(tidyverse)
library(readxl)
library(modelr)
library(zoo, quietly = TRUE)
library(cluster)
library(factoextra)
library(glmnet)
library(rpart)
library(randomForest)
library(dplyr)
library(rsample)
library(tidyverse)
library(dplyr)
library(rpart) 
library(rpart.plot)
library(caret)
library(gridExtra)
library(rsample)
library(rpart)
library(recipes)


#incarcare set de date
laptop.info <- read.csv("D:/Facultate an 3 sem 2/Big-Data/Proiect/Laptop db bigdata/Cleaned_Laptop_data.csv")

#descriere coloane
colnames(laptop.info)

#sumar date
summary(laptop.info)



#------ Curatarea setului de date pentru decision tree --------

#Modificarea denumirilor- pentru a putea fi recunoscute mai usor
laptop.info$ram_gb<-gsub(" GB GB","",as.character(laptop.info$ram_gb))
laptop.info$ssd<-gsub(" GB","",as.character(laptop.info$ssd))
laptop.info$hdd<-gsub(" GB","",as.character(laptop.info$hdd))
laptop.info$os_bit<-gsub("-bit","",as.character(laptop.info$os_bit))


print("Vizualizare tabelara a tuturor variabilelor categoriale")
names=colnames(laptop.info)


for (x in 1:17) {
  print(paste("Vizualizare tabelara pentru coloana: ", names[x]))
  print(table(laptop.info[,x]))
}


#Caut atributele egale cu 0, ==N/A
laptop.info[ laptop.info == "Missing" ] <- NA

print("Sumar pentru atributele egale cu 0")
colSums(is.na(laptop.info))



#Modificarea colonelor latest_price/old_price, din rupie indiana la USD pentru ca suntem mai familiari cu aceasta moneda
laptop.info$latest_price <- laptop.info$latest_price*0.013
laptop.info$old_price <- laptop.info$old_price*0.013


#Adaugarea unei coloane pentru a evidentia daca marimea display-ului a fost oferita sau nu, + media pentru artibutele na
laptop.info["display_size_given"] <- laptop.info$display_size
laptop.info$display_size_given[!is.na(laptop.info$display_size_given)] <- "Yes"
laptop.info$display_size_given[is.na(laptop.info$display_size_given)] <- "No"
laptop.info$display_size <- as.numeric(laptop.info$display_size) # chr -> num
laptop.info$display_size <- round(na.aggregate(laptop.info$display_size),2) #lib zoo pentru functia na.aggregate


#Grupare brad-uri in functie de producatori
laptop.info$brand <- tolower(laptop.info$brand)
laptop.info$brand <- ifelse(laptop.info$brand %in% c("acer",
                                                     "apple", "asus", "dell", 
                                                     "hp","lennovo", "msi"),
                            laptop.info$brand,"other")

#Grupare procesoare in functie de producator
laptop.info$processor_brand <- tolower(laptop.info$processor_brand)
laptop.info$processor_brand <- ifelse(laptop.info$processor_brand %in% c("amd",
                                                                         "intel"),
                                      laptop.info$processor_brand,"other")


#SSD & HDD & RAM subcategorii <1gb & >1gb  + <8bg & >8gb
laptop.info$ssd<-as.numeric(laptop.info$ssd) #chr -> num
laptop.info$ssd_cat <- ifelse(laptop.info$ssd < 1024.0,
                              "less_than_1gb","greater_than_1gb")

laptop.info$hdd<-as.numeric(laptop.info$hdd) #chr ->num
laptop.info$hdd_cat <- ifelse(laptop.info$hdd < 1024.0,
                              "low_hdd","high_hdd")

laptop.info$ram_gb_cat <- ifelse(as.numeric(laptop.info$ram_gb)<=8,
                                 "less_than_8","greater_than_8")



#---------------------Arbori de decizie---------------------------------

#afisam variabilele de tip numeric din setul de date
laptop.info %>% 
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

set.seed(123)
laptop_split <- initial_split(laptop.info, prop = 0.7)
laptop_train <- training(laptop_split)
laptop_test <- testing(laptop_split)

laptop.info %>%
  ggplot(aes(latest_price))+
  geom_density()

#pentru a vedea valorile exacte de la latest_price
view(laptop.info)

m1 <- rpart(
  formula = latest_price ~ ., 
  data = laptop_train,
  method = "anova"
)
m1 #afisare graf sub forma de text


rpart.plot(m1) #afisarea grafica a arborelui rezultat
plotcp(m1)
m1$cptable  # afisarea parametrilor alpha

mean(laptop_train$latest_price) #pentru root
sum((laptop_train$latest_price-mean(laptop_train$latest_price))^2)

m2 <- rpart(
  formula = latest_price ~ ., 
  data = laptop_train,
  method = "anova",
  control = list(cp = 0, xval = 10)  # se creste arborele pana la obtinerea valorii zero pentru parametrul alpha
) 
m2
plotcp(m2)
abline(v = 12, lty = "dashed")


# se obtine un arbore cu parametri minsplit si maxdepth specificati, testez iar mai apoi aplic pentru parametrii optimi
m3 <- rpart(
  formula = latest_price ~ .,
  data = laptop_train, 
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)
m3
plotcp(m3)

#cautam cele mai bune valori pentru parametri minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid)
models <- list()
#pentru fiecare combinatie min max,
for (i in 1:nrow(hyper_grid)) { 
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = latest_price ~. ,
    data = laptop_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  

#aleg cele 5 cu cea mai mica eroare 
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

view(mutated_grid)


optimal_tree <- rpart(
  formula = latest_price ~ .,
  data = laptop_train,
  method = "anova",
  control = list(minsplit = 20, maxdepth = 10, cp =0.01000000)
)

optimal_tree
plotcp(optimal_tree)

#pred not reco so i do this ->
rec <- 
  recipe(latest_price ~ ., data = laptop_train) %>% 
  step_other(processor_name) %>% # This is where we handle new categories
  prep()

new_train <- bake(rec, new_data = laptop_train)
new_test  <- bake(rec, new_data = laptop_test)

#Construiesc modelul din nou pentru datele de test cu mix,max,cp opti
model <- rpart(
  formula = latest_price ~ .,
  data = new_train,
  method = "anova",
  control = list(minsplit = 20, maxdepth = 10, cp =0.01000000)
)

# This works
pred <- predict(model, newdata = new_test)
RMSE(pred = pred, obs = laptop_test$latest_price)
model
rpart.plot(model)




#---------------------Bagging-----------------------------------
library(ipred)
set.seed(123)

bagged_m1 <- bagging(
  formula = latest_price ~ .,
  data = new_train, 
  coob = TRUE
)

#coob = true-> ii spun sa faca validarea pe instantele ramase afara, care nu au fost selectate in bag
bagged_m1

#assess 10-50 bagged trees
ntree <- 10:50
rmse <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging(
    formula = latest_price ~ .,
    data = new_train,
    coob = TRUE,
    nbagg = ntree[i]
  )
  rmse[i] = model$err #preiau rmse ca si eroarea modelului
}

plot(ntree, rmse, type ="l", lwd=2)
abline(v=25, col = "red", lty="dashed") #pana aici se intampla implicit

rmse







