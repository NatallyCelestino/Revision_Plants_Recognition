library(bibliometrix)
library(summarytools)
library(tidyverse)
library(patchwork)
library(paletteer)


# Scientometric Analysis
## Loading data
  

s <- convert2df('scopus.bib', dbsource = "scopus", format = "bibtex")
w <- convert2df('savedrecs.bib', dbsource = "wos", format = "bibtex")
w1<- convert2df('savedrecs (1).bib', dbsource = "wos", format = "bibtex")


## Joining data and removing duplicates

U <- mergeDbSources(s,w,w1, remove.duplicated = TRUE)



## Summarize data

R <- biblioAnalysis(U)
DS <- summary(object = R, k = 10)
plot(R, k = 10)



# Revision

## Loading and analyzing database structure

(data <- read_delim("data/dadostriagem2.0.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE))(data <- read.csv2("data/dadostriagem2.0.csv"))
data$accuracy <- as.numeric(data$accuracy)
data$N <- as.factor(data$N)
glimpse(data)


## Graphical View

### Creating a count table of Main botanical structures (Top 7)

cont_eb <- data %>%
  group_by(morphological_structure) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 7)


### Botanical Structures Chart

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


### Creating life form types count table (Top 7)

cont_tv <- data %>%
  group_by(Life_forms) %>% 
  summarise(cont = n()) %>%
  mutate(pct = prop.table(cont)) %>%
  filter(Life_forms != "NI") %>%
  slice_max(cont, n = 7)


### Chart of Types of Life Forms

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


### Creating extracted features count table (Top 5)

cont_re <- data %>%
  group_by(extracted_features) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 5)


### Extracted Features Chart

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


### Creating plant origin count table (Top 5)

cont_orig <- data %>%
  filter(country_of_origin != "NI") %>%
  group_by(country_of_origin) %>% 
  summarise(cont = n()) %>% 
  mutate(pct = prop.table(cont)) %>% 
  slice_max(cont, n = 5)


### Chart of countries of origin of plants

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


### Creating descriptors table (Top 5)

crono <- data %>% 
  mutate(Class = case_when((year <= 2008) ~ "1986-2008",
                           (year >= 2009 & year <= 2014) ~ "2009-2014",
                           (year > 2014) ~ "2015-2023")) %>% 
  select(extraction_techniques, Class) %>% 
  group_by(extraction_techniques, Class) %>% 
  summarise(n = n()) %>% 
  arrange(Class, n)

crono <- crono %>%
  group_by(Class) %>% 
  slice_max(n, n=5)


### Descriptor Count Chart

(B1 <- ggplot(crono) +
    geom_bar(aes(x = Class, fill = extraction_techniques, y= n), 
             position = "dodge", stat = "identity") +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab("Years") +
    ylab("Cited descriptors") +
    labs(fill = "Feature Descriptors") +
    scale_fill_viridis_d(option = "D")
)



### Creating Table of Classification Techniques by Period (Top 5)

crono_clas <- data %>% 
  mutate(Class = case_when((year <= 2008) ~ "1986-2008",
                           (year >= 2009 & year <= 2014) ~ "2009-2014",
                           (year > 2014) ~ "2015-2023")) %>% 
  select(classification_techniques, Class, accuracy) %>% 
  group_by(classification_techniques, Class)

crono_clas <- na.omit(crono_clas) 

crono_clas <- crono_clas %>%
  group_by(classification_techniques, Class) %>%
  summarise(mean_ac = round(mean(accuracy)), n = n()) %>% 
  arrange(Class, n)

crono_clas <- crono_clas %>%
  group_by(Class) %>% 
  slice_max(n, n=5)



### Classifier Count Chart

(B2 <- ggplot(crono_clas) +
    geom_bar(aes(x = Class, fill = classification_techniques, y = n), 
             position = "dodge", stat = "identity") +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(face = "bold")) +
    xlab(NULL) +
    ylab("Classifiers cited") +
    labs(fill = "Classification Techniques") +
    scale_fill_paletteer_d("MetBrewer::Renoir") +
    ggtitle("A")
)


### Generating average accuracy graph per classifier

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
    scale_fill_paletteer_d("MetBrewer::Renoir") +
    ggtitle("C")
)


### Generating a graph of the evolution of CNNs over the years (2015-2023)

cnn <- data %>% 
  select(classification_techniques, year, accuracy) %>% 
  filter(classification_techniques == "Convolutional Neural Networks (CNN)", 
         year != "2014") %>%
  group_by(year) %>% 
  summarise(cont = n())

(B4 <- cnn %>% 
    ggplot(aes(x = year, y = cont), stat = "identity") +
    geom_line() +
    theme_bw(base_size = 6) +
    theme(axis.text.x=element_text(face = "bold")) +
    scale_fill_manual(values = c("#2c115f")) +
    xlab("Years") +
    ylab("CNN") +
    ggtitle("B")
)

(graf1 <-(B2 + inset_element(B4, 
                             left = 0.01, bottom = 0.4, 
                             right = 0.5, top = 0.9)))
(graf <- (graf1 / B3 + plot_layout(guides = "collect")))

ggsave("results/grafico1.png", dpi = 800, width = 6.0, height = 6.0)
