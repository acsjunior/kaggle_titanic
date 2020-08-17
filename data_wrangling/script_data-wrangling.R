
# Instalar, caso ainda não tenha feito:
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("fastDummies")
# install.packages("caret")


# Carregando as bibliotecas:
require(dplyr)
require(stringr)
require(fastDummies)
require(ggplot2)
require(caret)
source("utils/funcoes_uteis.R")


# Carregando os dados:
df_treino <- read.csv("data/train.csv", na.strings = c("", NA), stringsAsFactors = F)
df_teste <- read.csv("data/test.csv", na.strings = c("", NA), stringsAsFactors = F)


# Unindo os dados:
df_completo <- df_treino %>%
  bind_rows(df_teste)


#---------------------------------------------------------------------------------
# Raio X inicial:


# Estrutura do conjunto de dados:
str(df_completo)


# Convertendo os tipos de dados:
df_completo <- df_completo %>%
  mutate_at(c("Pclass", "Embarked", "Sex", "Survived"), factor)


# Primeiras observações:
head(df_completo) %>%
  View()


# Estatísticas gerais:
summary(df_completo)


# Verificando valores ausentes:
count_missing_values(df_completo)


#---------------------------------------------------------------------------------
# Sexo:

# Frequência:
custom_barPlot(df = df_completo, 
               var_x = "Sex", 
               lab_x = "Sexo", 
               lab_y = "Passageiros", 
               title = "Passageiros por sexo")


# Sobrevivência:
custom_colPlot(df = df_completo, 
               var_x = "Sex", 
               var_filter = "Survived", 
               criteria = 1, 
               lab_x = "Sexo", 
               lab_y = "Sobreviventes", 
               title = "Taxa de Sobreviventes por sexo")

#---------------------------------------------------------------------------------
# Classe de embarque:


# Frequência:
custom_barPlot(df = df_completo, 
               var_x = "Pclass", 
               lab_x = "Classe", 
               lab_y = "Passageiros", 
               title = "Passageiros por classe")


# Sobrevivência:
custom_colPlot(df = df_completo, 
               var_x = "Pclass", 
               var_filter = "Survived", 
               criteria = 1, 
               lab_x = "Classe", 
               lab_y = "Sobreviventes", 
               title = "Taxa de Sobreviventes por Classe")


prop.table(table(df_completo$Sex, df_completo$Pclass), 2)


#---------------------------------------------------------------------------------
# Tamanho da família:


# Criando a variável:
df_completo$Family_size <- with(df_completo, Parch + SibSp + 1)

# Frequência:
custom_barPlot(df = df_completo, 
               var_x = "Family_size", 
               lab_x = "Tamanho da família", 
               lab_y = "Passageiros", 
               title = "Passageiros por Tamanho de Família")


# Sobrevivência:
custom_colPlot(df = df_completo, 
               var_x = "Family_size", 
               var_filter = "Survived", 
               criteria = 1, 
               lab_x = "Tamanho da Família", 
               lab_y = "Sobreviventes", 
               title = "Taxa de sobreviventes por Tamanho de Família")


#---------------------------------------------------------------------------------
# Título:


# Extraindo o título do nome do passageiro e atribuindo a uma nova variável:
df_completo$Title <- df_completo$Name %>%
  str_extract("([A-z]+)\\.") %>%
  str_sub(end = -2)


# Verificando:
head(df_completo) %>%
  View()


# Frequências:
custom_barPlot(df = df_completo,
               var_x = "Title",
               lab_x = "Título",
               lab_y = "Passageiros",
               title = "Passageiros por título",
               angle_x = 90)


# Mrs = Mme (mulheres casadas)
# Miss = Mlle (mulheres solteiras)
# Ms (não indica estado civil)

df_completo %>%
  filter(Title == "Ms") %>%
  View()

# Agrupar:
# Mme >> Mrs
# Mlle e Ms >> Miss
# Capt, Col, Major >> Military
# Countess, Don, Dona, Sir, Lady, Jonkheer >> Nobility

# Criando função de agrupamento:
group_titles <- function(title) {
  out <- title
  if(title %in% c("Mlle", "Ms")) {
    out <- "Miss"
  } else if(title == "Mme") {
    out <- "Mrs"
  } else if(title %in% c("Capt", "Col", "Major")) {
    out <- "Military"
  } else if(title %in% c("Countess", "Don", "Dona", "Lady", "Sir", "Jonkheer")) {
    out <- "Nobility"
  }
  return(out)
}

# Agrupando os títulos:
df_completo$Title <- sapply(df_completo$Title, group_titles)

# Frequências:
custom_barPlot(df = df_completo,
               var_x = "Title",
               lab_x = "Título",
               lab_y = "Passageiros",
               title = "Passageiros por título (escala logaritmica)",
               angle_x = 90,
               log = T)


# Sobrevivência:
custom_colPlot(df = df_completo,
               var_x = "Title",
               var_filter = "Survived",
               criteria = 1,
               lab_x = "Título",
               lab_y = "Sobreviventes",
               title = "Taxa de sobreviventes por título")


# Master??
df_completo %>%
  filter(!is.na(Age)) %>%
  group_by(Title) %>%
  summarise(avg_age = mean(Age), min_age = min(Age), max_age = max(Age))



#---------------------------------------------------------------------------------
# Idade:

count_missing_values(df_completo)

# Montando o conjunto de dados
df_knn <- df_completo %>%
  dplyr::select(PassengerId, Age, Embarked,  Pclass, Fare, Sex, Title, Family_size) %>%
  filter(!is.na(Embarked), !is.na(Fare))


# Criando variáveis dummy
df_knn <- dummy_cols(df_knn, select_columns = c("Pclass", "Sex", "Title", "Embarked"), remove_selected_columns = T, remove_first_dummy = T)


# Criando o dataset para o treino do modelo
df_knn_train <- df_knn %>%
  filter(!is.na(Age))

# Padronizando as covariáveis
procValues_knn <- preProcess(df_knn_train[c(-1,-2)], method = c("center", "scale"))
df_knn_train <- predict(procValues_knn, df_knn_train)

# Encontrando o melhor k
grid_k <- data.frame(.k = seq(1,30, 2))

set.seed(10)
knn_model <- train(form = Age ~ .,
                   data = df_knn_train[-1],
                   method = "knn",
                   tuneGrid = grid_k,
                   trControl = trainControl(method = "cv", number = 5))

knn_model
# RMMSE: Root Mean Squaret Error - quanto menor, melhor
# Rsquared: R^2 (Coeficiente de determinação) - quanto maior, melhor
# MAE: Mean absolute error - quanto menor, melhor

k <- knn_model$bestTune[,1]
plot(knn_model, main = paste0("k = ", k))


# Criando o dataset para predição das idades
df_pred <- df_knn %>%
  filter(is.na(Age))

# Padronizando as covariáveis
df_pred <- predict(procValues_knn, df_pred)


# Predizendo as idades
df_pred$Age <- predict(knn_model$finalModel, newdata = df_pred[c(-1,-2)])

df_pred <- df_completo %>% 
  left_join(df_pred[c("PassengerId", "Age")], by = "PassengerId")
df_completo$Age <- coalesce(df_pred$Age.x, df_pred$Age.y)

count_missing_values(df_completo)


# Conferindo as estatísticas da idade:
summary(df_completo$Age)

# Comportamento da idade com relação à sobrevivência
custom_boxPlot(df = df_completo, 
               var_x = "Survived",
               var_y = "Age",
               title = "Boxplot da idade em relação à sobrevivência")

quantile(df_completo$Age)


#---------------------------------------------------------------------------------
# Porto de embarque:

count_missing_values(df_completo)
