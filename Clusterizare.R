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



#_------------------------

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


#Distributia variabilei tinta
d <- density(laptop.info$latest_price)
plot(d, main="Kernel Density pentru Latest Price")
polygon(d, col="yellow", border="blue")


#Adaugarea unei coloane pentru valoarea logaritmata a variabilei
laptop.info$log_latest_price <- log(laptop.info$latest_price)
d <- density(laptop.info$log_latest_price)
plot(d, main="Kernel Density pentru Log Latest Price")
polygon(d, col="yellow", border="blue")




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



laptop.info.final <-laptop.info[c(1,3,4,7,10,11,12,13,14,15,16,17,21,22,23,24,25,26,27,28)]


#-------Realizare clustering bazat pe atribute de performanta------- 
tmp.laptop.info.final <- laptop.info[,c(6,8,12)] #selectam doar elementele pe care ne axam: ram-ssd-graphic card

#modificam tipul din chr in num
tmp.laptop.info.final$ram_gb<- as.numeric(tmp.laptop.info.final$ram_gb)
tmp.laptop.info.final$ssd<- as.numeric(tmp.laptop.info.final$ssd)
tmp.laptop.info.final$graphic_card_gb<- as.numeric(tmp.laptop.info.final$graphic_card_gb)


#standardizam datele, scalam prin centrarea variabilei in jurul mediei 0
tmp.laptop.info.final <- scale(tmp.laptop.info.final)
rownames(tmp.laptop.info.final) <- paste(laptop.info.final$brand,",",
                                         c(1:length(laptop.info.final$brand)),sep="")


#calculez distanta
distance <- get_dist(tmp.laptop.info.final) #used by default the Euclidean distance
distance #?
#fviz_dist(distance) #incarca foarte greu


#clusterizare unde k = 2, impartite aleator
k2 <- kmeans(tmp.laptop.info.final, centers = 2, nstart = 25)
k2
fviz_cluster(k2, data = tmp.laptop.info.final, labelsize=6)

#tmp.laptop.info.final %>% as_tibble() %>% 
 #mutate(cluster = k2$cluster, brand = row.names(tmp.laptop.info.final), cluster = factor(cluster)) %>%
#ggplot(aes(ssd,graphic_card_gb, color = cluster, label = brand)) + geom_text()



k5 <- kmeans(tmp.laptop.info.final, centers = 3, nstart = 25)
k5
fviz_cluster(k5, data = tmp.laptop.info.final, labelsize=6)

#clusterizare unde k = 4, impartite aleator
k4 <- kmeans(tmp.laptop.info.final, centers = 4, nstart = 25)
k4 #vad centroidul la k means
fviz_cluster(k4, data = tmp.laptop.info.final, labelsize=6)


p2 <- fviz_cluster(k2, geom = "point", data = tmp.laptop.info.final) + ggtitle("k=2")
p3 <- fviz_cluster(k5, geom = "point", data = tmp.laptop.info.final) + ggtitle("k=3")
p4 <- fviz_cluster(k4, geom = "point", data = tmp.laptop.info.final) + ggtitle("k=4")


grid.arrange(p2, p3, p4, nrow = 2)


#Determinare nr optim de clustere cu ajutoru metodei elbow
set.seed(123)
fviz_nbclust(tmp.laptop.info.final, kmeans, method = "wss")

#Determinam calitatea clusterizarii cu ajutorul metodei silhouette
set.seed(123)
fviz_nbclust(tmp.laptop.info.final, kmeans, method = "silhouette")


#Determinare nr optim de clustere utiizand Metoda statisticii gap, sub ipoteza distributiei nule
gap_stat <- clusGap(tmp.laptop.info.final, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
gap_stat
gap_stat2 <- clusGap(tmp.laptop.info.final, FUN = kmeans, nstart = 25, K.max = 20, B = 50)
fviz_gap_stat(gap_stat2)


#print(gap_stat, method="firstmax")
#print(gap_stat2, method="firstmax")


#clusterizare k=10 -> metoda silhouette
#k10 <- kmeans(tmp.laptop.info.final, centers = 10, nstart = 25)
#k10
#fviz_cluster(k10, data = tmp.laptop.info.final, labelsize=6)


#clusterizare k=9 -> metoda statisticii gap
k3 <- kmeans(tmp.laptop.info.final, centers = 10, nstart = 25)
fviz_cluster(k3, data = tmp.laptop.info.final, labelsize=0)
k3
