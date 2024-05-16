#________________________Universidade Federal do Pará________________________#
# Mestranda: Natally Celestino Gama
# Orientador: Prof. Dr. Deivison Venicio
#____________________________________________________________________________#
######## Cápitulo 1 - Dissertação de Mestrado 😁 ########
# Reconhecimento de plantas por meio de Visão Computacional: 
# Uma revisão sistemática 📈
#____________________________________________________________________________#

# Instalando e ativando pacote
# install.packages("bibliometrix")
library(bibliometrix)

# Carregar e converter dados exportados da Scopus e WoS
s <- convert2df('Data/scopus.bib', dbsource = "scopus", format = "bibtex")
w <- convert2df('Data/savedrecs.bib', dbsource = "wos", format = "bibtex")
w1<- convert2df('Data/savedrecs (1).bib', dbsource = "wos", format = "bibtex")


# Unir bases de dados
U <- mergeDbSources(s,w,w1, remove.duplicated = TRUE)
P <- U[,c("AU", "TI", "AB", "DE", "ID", "SO", "TC", "PY", "LA", "DT", "DI")]

# Converter para o formato csv e exportar
write.table(P, 'Outputs/Table/dadosbiblio.csv', sep = ";", row.names = FALSE) #Parcial
write.table(U, 'Outputs/Table/dadostotais.csv', sep = ";", row.names = FALSE) #Completo

# Sumarizar dados
R <- biblioAnalysis(U)
DS <- summary(object = R, k = 10)
plot(R, k = 10)

#Analisar dados no biblioshiny
biblioshiny()

