library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(caTools)
library(ROSE)
library(smotefamily)
library(rpart)
library(rpart.plot)
library(magrittr)

tranzactions <- read_csv("creditcard.csv")

str(tranzactions)

tranzactions$Class <- factor(tranzactions$Class, levels = c(0, 1))

summary(tranzactions)

sum(is.na(tranzactions))

table(tranzactions$Class)

prop.table(table(tranzactions$Class))

labels <- c("normal", "fraud")
labels <-
  paste (labels, round(100 * prop.table(table(tranzactions$Class)), 2))
labels <- paste0(labels, "%")

pie(
  table(tranzactions$Class),
  labels,
  col = c("lightskyblue", "red"),
  main = "Diagrama de structura tranzactii cu cardul
      supuse fraudei bancare - pie chart"
)
# Distributia Classei in functie de timpul de procesare a tranzactiilor 

tranzactions.true <- tranzactions[tranzactions$Class == 0,]
tranzactions.false <- tranzactions[tranzactions$Class == 1,]

ggplot() %>% +
  geom_density(
  data = tranzactions.false,
  aes(x = Time),
  color = "red",
  fill = "red",
  alpha = 0.5
) +
  geom_density(
    data = tranzactions.true,
    aes(x = Time),
    color = "lightskyblue",
    fill = "lightskyblue",
    alpha = 0.5
  ) +
  scale_x_time()

#Distributia contravalorii tranzactiilor fraudate si analiza componentei Amount

 value_of_fraud_tranzactions <- filter(tranzactions, Class == 1)
 
 v_mean = mean(value_of_fraud_tranzactions$Amount)
 v_meadian = median(value_of_fraud_tranzactions$Amount)
 v_min = min(value_of_fraud_tranzactions$Amount)
 v_max = max(value_of_fraud_tranzactions$Amount)
 v_no_amount = nrow(value_of_fraud_tranzactions[value_of_fraud_tranzactions$Amount == 0,]) 
 
  ggplot(data = value_of_fraud_tranzactions) %>% +
    geom_density(
      aes(x=Amount),
      fill = "green",
      color = "green")+
    xlim(0, 2500) 
  
#Predicitia tranzactii fara a folosi model de predictie

predictions <- rep.int(0, nrow(tranzactions))
predictions <- factor(predictions, levels = c(0, 1))

confusionMatrix(data = predictions, reference = tranzactions$Class)
  
#Esantion 

set.seed(1)
tranzactionsMod <- tranzactions %>% sample_frac(0.1)
table(tranzactionsMod$Class)

ggplot(data = tranzactionsMod, aes(x = V1, y = V2, col = Class)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c('lightskyblue', 'red'))

#Creare date de invatare si test pentru modelul de predictie

set.seed(123)
tranz_sample = sample.split(tranzactionsMod$Class, SplitRatio = 0.80)
learning_data = subset(tranzactionsMod, tranz_sample == TRUE)
test_data = subset(tranzactionsMod, tranz_sample == FALSE)

dim(learning_data)
dim(test_data)

table(learning_data$Class)

#Supraesantionare tranzactii normale

n_normal <- 22750
pro_normal <- 0.50
total_n_normal <- n_normal / pro_normal

oversampling_result <- ovun.sample(
  Class ~ . ,
  data = learning_data,
  method = "over",
  N = total_n_normal,
  seed = 2020
)

oversampled_tranzactions <- oversampling_result$data

table(oversampled_tranzactions$Class)

ggplot(data = oversampled_tranzactions, aes(x = V1, y = V2, col = Class)) %>%
  +geom_point(position = position_jitter(width = 0.1)) +
  scale_color_manual(values = c('lightskyblue', 'red'))

table(learning_data$Class)

#Sub-esantionare tranzactii fraudate

n_fraud <- 35
pro_fraud <- 0.50
total_n_fraud <- n_fraud / pro_fraud

undersampling_result <- ovun.sample(
  Class ~ .,
  data = learning_data,
  method = "under",
  N = total_n_fraud,
  seed = 2019
)

undersampled_tranzactions <- undersampling_result$data

table(undersampled_tranzactions$Class)

ggplot(data = undersampled_tranzactions, aes(x = V1, y = V2, col = Class)) %>%
  +geom_point(position = position_jitter(width = 0.1)) +
  scale_color_manual(values = c('lightskyblue', 'red'))

#Supraesantionare si sub-esantionare

n_normal_fraud <- nrow(learning_data)
pro_normal_fraud <- 0.50

sampling_result <- ovun.sample(
  Class ~ .,
  data = learning_data,
  method = "both",
  N = n_normal_fraud,
  p = pro_normal_fraud,
  seed = 2019
)

sampled_tranzactions <- sampling_result$data

table(sampled_tranzactions$Class)

prop.table(table(sampled_tranzactions$Class))

ggplot(data = sampled_tranzactions, aes(x = V1, y = V2, col = Class)) %>%
  +geom_point(position = position_jitter(width = 0.2)) +
  scale_color_manual(values = c('lightskyblue', 'red'))

#SMOTE crearea noului set de date cu procent de 60% si 40%

table(learning_data$Class)

s_normal <- 22750
s_fraud <- 35
s_pro <- 0.60

stimes <- ((1 - s_pro) / s_pro) * (s_normal / s_fraud) - 1

smote_output = SMOTE(
  X = learning_data [,-c(1, 31)],
  target = learning_data$Class,
  K = 5,
  dup_size = stimes
)

tranzactions_smote <- smote_output$data

colnames(tranzactions_smote)[30] <- "Class"

prop.table(table(tranzactions_smote$Class))

#Compararea datelor initiale cu datele sintetice

ggplot(data = learning_data, aes(x = V1, y = V2, color = Class)) %>% +
  geom_point() +
  scale_color_manual(values = c('lightskyblue', 'red'))

ggplot(data = tranzactions_smote, aes(x = V1, y = V2, color = Class)) %>% +
  geom_point() +
  scale_color_manual(values = c('lightskyblue', 'red'))

#Arbore de decizie aplicate setului de date sintetic

CART_model <- rpart(Class ~ . , learning_data)

rpart.plot(CART_model,
           extra = 0,
           type = 5 ,
           tweak = 1.2)

#Predictie tranzactii sintetice de frauda
predicted_val <- predict(CART_model, test_data, type = 'class')

#Constructie matrice de confuzie valori prezise

confusionMatrix(predicted_val, test_data$Class)

#...........................

predicted_val <-
  predict(CART_model, tranzactionsMod[,-1], type = 'class')

confusionMatrix(predicted_val, tranzactionsMod$Class)

#...........................

#Arbori de decizie pentru clasificare aplicat pe setul de date sintetic

CART_model <- rpart(Class ~ . , tranzactions_smote)

rpart.plot(CART_model, extra = 0, type = 5, tweak = 1.2)

#Prezicem numarul date cu referinta date de test

predicted_val <- predict(CART_model, test_data[-1], type = "class")

confusionMatrix(predicted_val, test_data$Class)

#........Comparatie predictie finala .......

CART_model <- rpart(Class ~ . , learning_data[,-1])

predicted_val <-
  predict(CART_model, tranzactionsMod[,-1], type = 'class')
confusionMatrix(predicted_val, tranzactionsMod$Class)

#................................
