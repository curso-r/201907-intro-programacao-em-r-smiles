
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(rsample)
library(skimr)

set.seed(1313)

# Estratégias -------------------------------------------------------------

# Criando banco de dados 

criar_amostra <- function(n, perc_treino) {
  data_frame(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50),
    is_train = 1:n %in% sample.int(n, n*perc_treino)
  )
}

df <- criar_amostra(100, 0.5)

df_train <- df %>% filter(is_train)
df_test <- df %>% filter(!is_train)

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE)

# Ajustando o primeiro modelo 

modelo <- lm(y ~ x, data = df_train)
summary(modelo)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando um modelo mais complexo 

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE, 
              formula = y ~ poly(x, 50, raw = TRUE))


modelo <- lm(y ~ poly(x, 50, raw = TRUE), data = df_train)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando diversos modelos 

ajustar_polinomios <- function(df, grau) {
  
  erros <- NULL
  for(g in grau) {
    
    modelo <- lm(y ~ poly(x, g, raw = TRUE), data = df %>% filter(is_train))
    
    df$predicao <- predict(modelo, df)
    erros <- bind_rows(
      erros,
      df %>% 
        group_by(is_train) %>% 
        summarise(mse = mean((predicao - y)^2)) %>% 
        mutate(grau = g)
    )
    
  }
  erros
}

erros <- ajustar_polinomios(df, 1:50)
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 


# Por que variância alta?

n_vezes <- 100
df <- criar_amostra(10000, 0.5)
gg <- ggplot(df, aes(x = x, y = y)) +
  geom_point() 

for(i in 1:n_vezes) {
  gg <- gg + 
    geom_smooth(
      data = df %>% sample_n(50),
      alpha = 0.01,
      color = "red",
      method = "lm", se = FALSE, 
      formula = y ~ poly(x, 20, raw = TRUE)
    )
}

gg + coord_cartesian(ylim = c(0, 1000))


# Qual o efeito do tamanho da amostra?

df <- criar_amostra(1000, 0.5)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 


# Qual o efeito do tamanho da amostra de teste? 

df <- criar_amostra(1000, 0.99)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 

# Implementando validação cruzada 

df <- criar_amostra(100, 0.5)
k <- 100
df$fold <- 1:nrow(df)
erros <- NULL
for (i in 1:k) {
  df$is_train <- df$fold != i
  erro <- ajustar_polinomios(df, grau = 1:50)
  erro$fold <- i
  erros <- bind_rows(erros, erro)
}

erros2 <- erros %>% 
  group_by(grau, is_train) %>% 
  summarise(
    media = median(mse),
    minimo = min(mse),
    maximo = max(mse),
    quantil95 = quantile(mse, probs = 0.95),
    quantil5 = quantile(mse, probs = 0.05)
  )

ggplot(erros2, aes(x = grau, y = media)) + 
  geom_line() + 
  geom_point(
    data = erros2 %>% group_by(is_train) %>% filter(media == min(media)),
    size = 3
  ) +
  facet_wrap(~is_train) +
  #coord_cartesian(ylim = c(0, 5000)) +
  geom_ribbon(aes(ymin = quantil5, ymax = quantil95), alpha = 0.2)


# Recipes + Caret ---------------------------------------------------------


# exemplo 1

diamantes <- ggplot2::diamonds
glimpse(diamantes)
# Vamos transformar as variáveis ordinais em categóricas
diamantes <- diamantes %>%
  mutate(
    cut = as.character(cut),
    color = as.character(color),
    clarity = as.character(clarity)
  )


receita <- recipe(price ~ ., data = diamantes) %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors())
prep <- prep(receita, diamantes)
base_treino <- bake(prep, diamantes)
ajuste <- lm(price ~ ., base_treino)
summary(ajuste)

# exemplo 2

set.seed(20032019)
modelo <- train(
  receita, 
  diamantes, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)
modelo
# Para acessar o modelo final
summary(modelo$finalModel)
# Função para avaliar a "importância" de cada preditor
varImp(modelo)

# Valores obs vs pred
diamantes %>% 
  mutate(pred = predict(modelo, diamantes)) %>% 
  ggplot(aes(y = pred, x = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  theme_bw()

diamantes %>% 
  mutate(pred = predict(modelo, diamantes)) %>% 
  ggplot(aes(y = pred, x = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  theme_bw() +
  coord_cartesian(x = c(0, 20000), ylim = c(0, 25000))

diamantes %>% 
  mutate(pred = predict(modelo, diamantes)) %>% 
  arrange(desc(pred)) %>% 
  View()


# Regressão logística -----------------------------------------------------

data("credit_data")
glimpse(credit_data)

skim(credit_data)
credit_data %>% 
  group_by(Status) %>% 
  skim()

receita <- recipe(Status ~ ., data = credit_data) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors())
modelo <- train(
  receita, 
  credit_data, 
  method = "glm", 
  family = "binomial", 
  trControl = trainControl(method = "cv", number = 5)
)
modelo
varImp(modelo)


# Random Forest -----------------------------------------------------------

data("credit_data")

linhas_de_treino = runif(nrow(credit_data)) < 0.8

credit_data$base = if_else(linhas_de_treino, "treino", "teste")

credit_data_treino <- credit_data %>% 
  filter(base == "treino") %>% 
  select(-base)

credit_data_teste <-  credit_data %>% 
  filter(base == "teste") %>% 
  select(-base)

receita <- recipe(Status ~ ., data = credit_data_treino) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors())

train_control_rf <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = 1
)

grid_rf <- data.frame(
  mtry = c(2, 3, 4, 5, 6, 7)
)
modelo_rf <- train(
  receita, 
  credit_data_treino, 
  method ="rf",
  metric = "ROC",
  trControl = train_control_rf,
  tuneGrid = grid_rf
)
