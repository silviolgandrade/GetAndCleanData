# set the work directory
setwd("D:/jhds/jhds_03/Project/")

# load librarie
library(plyr)

# reading tables
DadosTeste = read.table("./UCI HAR Dataset/test/X_test.txt")
DadosTreino = read.table("./UCI HAR Dataset/train/X_train.txt")
NomesTeste = read.table("./UCI HAR Dataset/test/y_test.txt")
NomesTreino = read.table("./UCI HAR Dataset/train/y_train.txt")
SujeitosTeste = read.table("./UCI HAR Dataset/test/subject_test.txt")
SujeitosTreino = read.table("./UCI HAR Dataset/train/subject_train.txt")
ListaRecursos <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
Atividades <- read.table("./UCI HAR Dataset/activity_labels.txt")

# binding tables to made a dataset
Dados <- rbind(DadosTeste, DadosTreino)
Nomes <- rbind(NomesTeste, NomesTreino)
Sujeitos <- rbind(SujeitosTeste, SujeitosTreino)

# selected titles from table features
Recursos <- ListaRecursos$V2
Colunas <- grepl("(std|mean[^F])", Recursos, perl=TRUE)

# filtering data
Dados <- Dados[, Colunas]
names(Dados) <- Recursos[Colunas]
names(Dados) <- gsub("\\(|\\)", "", names(Dados))
names(Dados) <- tolower(names(Dados))

# insert names from var Atividades to dataset
Atividades[,2] = gsub("_", "", tolower(as.character(Atividades[,2])))
Nomes[,1] = Atividades[Nomes[,1], 2]
names(Nomes) <- "activity"
names(Sujeitos) <- "subject"

# building the tidy Dataset
Tabela1 <- cbind(Sujeitos, Nomes, Dados)
write.table(Tabela1, "Tabela1.txt", row.name=FALSE)

# building the tidy Dataset with the average for each unique variable and subject
UnicosSujeitos = unique(Sujeitos)[,1]
NumeroSujeitos = length(UnicosSujeitos)
NumeroAtividades = length(Atividades[,1])
NumeroColunas = length(names(Tabela1))
Tabela2 = Tabela1[ 1:(NumeroSujeitos*NumeroAtividades), ]

row = 1
for (sub in 1:NumeroSujeitos) {
  for (ativ in 1:NumeroAtividades) {
    Tabela2[row,1] = UnicosSujeitos[sub]
    Tabela2[row,2] = Atividades[ativ, 2]
    aux <- Tabela1[Tabela1$subject==sub & Tabela1$activity==Atividades[ativ,2],]
    Tabela2[row, 3:NumeroColunas] <- colMeans(aux[, 3:NumeroColunas])
    row = row + 1
  }
}

write.table(Tabela2, "Tabela2.txt", row.name=FALSE)

# end of code

