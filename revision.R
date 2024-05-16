#________________________Universidade Federal do Pará________________________#
# Mestranda: Natally Celestino Gama
# Orientador: Prof. Dr. Deivison Venicio
#____________________________________________________________________________#
######## Cápitulo 1 - Dissertação de Mestrado 😁 ########
# Reconhecimento de plantas por meio de Visão Computacional: 
# Uma revisão sistemática 📈
#____________________________________________________________________________#

#install.packages("tidyverse")
#install.packages("patchwork")

#Bibliotecas utilizadas
library(tidyverse)
library(summarytools)
library(patchwork)

#Carregando e analisando estrutura do banco de dados
(data <- readr::read_csv2('Data/Dadostriagem2.csv'))
data$accuracy <- as.numeric(data$accuracy)
data$N <- as.factor(data$N)
glimpse(data)


#Tabela de frequência por Estrutura da planta
data %>% 
  distinct(N, .keep_all = TRUE) %>%                                             #remover valores repetidos
  freq(var = morphological_structure, report.nas = FALSE, style = "rmarkdown")

#Tabela de frequência por Recursos extraídos
data %>% 
  distinct(N, .keep_all = TRUE) %>% 
  freq(var = extracted_features, report.nas = FALSE, style = "rmarkdown")

#Tabela de frequência por Tipo de formas de vida
data %>% 
  distinct(N, .keep_all = TRUE) %>% 
  freq(var = Life_forms, report.nas = FALSE, style = "rmarkdown")

#Tabela de frequência por país de origem das plantas
data %>% 
  distinct(N, .keep_all = TRUE) %>% 
  freq(var = country_of_origin, report.nas = FALSE, style = "rmarkdown")

#Frequência de classificadores
Class <- data %>% 
  distinct(N, .keep_all = TRUE) %>% 
  freq(var = classification_techniques, report.nas = FALSE, style = "rmarkdown")

#Frequência de descritores
Extract <- data %>% 
  distinct(N, .keep_all = TRUE) %>% 
  freq(var = extraction_techniques, report.nas = FALSE, style = "rmarkdown")

#_____________________________________________________________________________#
#GRÁFICOS
#Características extraídas das plantas para identificação

#Criando tabela de contagem de Principais estruturas botânicas(Top 7)
cont_eb <- data %>%
  distinct(N, .keep_all = TRUE) %>%
  group_by(morphological_structure) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 7)
#count(Estrutura_morfologica) %>% 
#arrange(desc(n))

#Gráfico de Estruturas botânicas
(A1 <- cont_eb %>% 
    ggplot(aes(label = scales::percent(pct)))+
    geom_bar(aes(x = reorder(morphological_structure, +cont), y = cont), 
             stat = "identity", fill = "#000004", width = .80) +
    geom_label(aes(x = morphological_structure,y = cont), 
               position =   position_dodge(width = .9),
               size = 2) +
    theme_classic(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Botanical structures") +
    ylab("N. of articles") +
    ggtitle("A")+
    coord_flip()
)

#Criando tabela de contagem de Tipos de formas de vida
cont_tv <- data %>%
  distinct(N, .keep_all = TRUE) %>%
  group_by(Life_forms) %>% 
  summarise(cont = n()) %>%
  mutate(pct = prop.table(cont)) %>%
  filter(Life_forms != "NI") %>%
  slice_max(cont, n = 7)
#count(Tipo_vegetacao) %>%
#arrange(desc(n)) 

#Gráfico de Tipos de formas de vida
(A2 <- cont_tv %>%
    ggplot(aes(label = scales::percent(pct))) +
    geom_bar(aes(x = reorder(Life_forms, +cont), y = cont),
             stat = "identity", fill = "#000004", width = .80) +
    geom_label(aes(x = Life_forms,y = cont), 
               position =   position_dodge(width = .9),
               size = 2) +
    theme_classic(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Types of life forms ") +
    ylab("N. of articles") +
    ggtitle("B")+
    coord_flip()
)

#Criando tabela de contagem de recursos extraídos (Top 5)
cont_re <- data %>%
  distinct(N, .keep_all = TRUE) %>%
  group_by(extracted_features) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 5)
#count(Recursos_extraidos) %>% 
#arrange(desc(n)) 

#Gráfico de Recursos Extraídos
(A3 <- cont_re %>%
    ggplot(aes(label = scales::percent(pct))) +
    geom_bar(aes(x = reorder(extracted_features, +cont), y = cont), 
             stat = "identity", fill = "#000004", width = .80) +
    geom_label(aes(x = extracted_features,y = cont), 
               position =   position_dodge(width = .9),
               size = 2) +
    theme_classic(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Types of features extracted ") +
    ylab("N. of articles") +
    ggtitle("B") +
    coord_flip()
)


#Criando tabela de contagem de origem das plantas
cont_orig <- data %>%
  distinct(N, .keep_all = TRUE) %>%
  filter(country_of_origin != "NI") %>%
  group_by(country_of_origin) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 5)
#count(Origem_plantas) %>%
#arrange(desc(n))

#Gráfico de Paises de origem das plantas
(A4 <- cont_orig %>% 
    ggplot(aes(label = scales::percent(pct))) +
    geom_bar(aes(x = reorder(country_of_origin, +cont), y = cont), 
             stat = "identity", fill = "#000004", width = .80) +
    geom_label(aes(x = country_of_origin, y = cont), 
               position =   position_dodge(width = .9),
               size = 2) +
    theme_classic(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Countries of origin for image sets ") +
    ylab("N. of articles") +
    ggtitle("A")+
    coord_flip()
)


#Criando tabela descritores
crono <- data %>% 
  mutate(Class = case_when((year <= 2008) ~ "1986-2008",
                             (year >= 2009 & year <= 2014) ~ "2009-2014",
                             (year > 2014) ~ "2015-2023")) %>% 
  distinct(N, .keep_all = TRUE) %>%
  select(extraction_techniques, Class) %>% 
  group_by(extraction_techniques, Class) %>% 
  summarise(n = n()) %>% 
  arrange(Class, n)

#Selecionando apenas os 5 principais descritores por período
crono <- crono %>%
  group_by(Class) %>% 
  slice_max(n, n=5)

#Gráfico de contagem de descritores
(B1 <- ggplot(crono) +
    geom_bar(aes(x = Class, fill = extraction_techniques, y= n), 
             position = "dodge", stat = "identity") +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Years") +
    ylab("Cited descriptors") +
    labs(fill = "Feature Descriptors") +
    scale_fill_viridis_d(option = "magma")
)

#ggsave("Image1.tiff", path = "Outputs/Figure", dpi = 800,width = 15, height = 12,
#     units = "cm")
#______________________________________________________________________________
#Criando Tabela de Técnicas de classificação
crono_clas <- data %>% 
  mutate(Class = case_when((year <= 2008) ~ "1986-2008",
                             (year >= 2009 & year <= 2014) ~ "2009-2014",
                             (year > 2014) ~ "2015-2023")) %>% 
  distinct(N, .keep_all = TRUE) %>%
  select(classification_techniques, Class, accuracy) %>% 
  group_by(classification_techniques, Class)

#Removendo valores nulos do dataframe e filtrando classes
crono_clas <- na.omit(crono_clas) 

crono_clas <- crono_clas %>%
  group_by(classification_techniques, Class) %>%
  summarise(mean_ac = round(mean(accuracy)), n = n()) %>% 
  arrange(Class, n)


#Gerando tabela com o Top 7 de Classificadores
crono_clas <- crono_clas %>%
  group_by(Class) %>% 
  slice_max(n, n=5)

#Gráfico de Contagem de classificadores
(B2 <- ggplot(crono_clas) +
    geom_bar(aes(x = Class, fill = classification_techniques, y = n), 
             position = "dodge", stat = "identity") +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab(NULL) +
    ylab("Classifiers cited") +
    labs(fill = "Classification Techniques") +
    scale_fill_viridis_d(option = "magma") +
    ggtitle("A")
)


#Gerando gráfico de média da acurácia por classificador
(B3 <- crono_clas %>% 
    ggplot(aes(label = scales::number(mean_ac)))+
    geom_bar(aes(x = Class, fill = classification_techniques, y = mean_ac), 
             position = "dodge", stat = "identity") +
    geom_text(aes(x = Class, group = classification_techniques, y = mean_ac), 
              position =   position_dodge(width = .9),
              size = 1.7,
              vjust = -0.3,
              hjust = 0.5) +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    scale_y_continuous(breaks = seq(0,100,20))+
    xlab("Years") +
    ylab("Average accuracy (%)") +
    labs(fill = "Classification Techniques")+
    scale_fill_viridis_d(option = "magma") +
    ggtitle("C")
)

#Gerando gráfico da evolução das CNNs ao longo dos anos (2014-2023)
cnn <- data %>% 
  distinct(N, .keep_all = TRUE) %>%
  select(classification_techniques, year, accuracy) %>% 
  filter(classification_techniques == "Convolutional Neural Networks (CNN)", 
         year != "2014") %>%
  group_by(year) %>% 
  summarise(cont = n())


(B4 <- cnn %>% 
    ggplot(aes(x = year, y = cont), stat = "identity") +
    geom_line() +
    #geom_point() +
    theme_classic(base_size = 6) +
    theme(axis.text.x=element_text(face = "bold")) +
    scale_fill_manual(values = c("#2c115f")) +
    xlab("Years") +
    ylab("CNN") +
    ggtitle("B")
)

#Gerando Combinação de gráficos
#Características botânicas e tipos de vegetação
A1 / A2
ggsave("Image2.png", path = "Outputs/Figure", dpi = 800,width = 15, height = 16,
       units = "cm")

#Recursos extraídos e tipos de descritores
A4 / A3
ggsave("Image3.png", path = "Outputs/Figure", dpi = 800,width = 17, height = 16,
       units = "cm")

#Classificadores e média de acuracia
(gclass <- B2 + inset_element(B4,
                              left = 0.05, bottom = 0.4, 
                              right = 0.6, top = 0.95))

gclass / B3 + plot_layout(guides = 'collect')
ggsave("Image4.tiff", path = "Outputs/Figure", dpi = 800, width = 16, height = 14,
       units = "cm")




