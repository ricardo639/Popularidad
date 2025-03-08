
library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(jtools)
library(sjPlot)
library(broom)
library(readstata13)
library(haven)

popular <- read_dta()#insertar la base de datos 

View(popular)

popularidad <- as_tibble(popular)

n1 <- popularidad %>%
  group_by() %>%
  summarise(ocasiones = n()) %>%
  ungroup() %>%
  group_by(ocasiones) %>%
  summarise(frecuencia = n()) %>%
  ungroup() %>%
  mutate(porcentaje = round(100 * frecuencia / sum(frecuencia), 2))

knitr::kable(n1, align = "c")



# Modelo nulo 
M0 <- lmer(popular ~ 1 + (1|pupil), REML = FALSE, data = popularidad)
summary(M0)
summ(M0)

M1_ <- lmer(popular ~ texp + (1|pupil), REML = FALSE, data = popularidad)

# Modelo con interceptos y pendientes aleatorias 
# Modelo incondicional de crecimiento 
M1 <- lmer(popular ~ texp + (1+texp|pupil), REML = FALSE, data = popularidad)
summary(M1)
summ(M1)

# Grafica de las trayectorias de crecimiento 
popularidad$texp <- as.numeric(as.character(as_factor(popularidad$texp)))
popularidad$popular <- as.numeric(as.character(as_factor(popularidad$popular)))

popularidad$sex <- as_factor(popularidad$sex)

popularidad %>% 
  ggplot(aes(texp, popular)) +
  geom_line(aes(group = pupil, col = sex)) +
  facet_wrap(~sex) +
  scale_x_continuous(limits = c(1, 7),  # Cambia los límites de texp a 1-7
                     breaks = seq(1, 7, 1)) +  # Puntos de ruptura en el eje x, cada 1 unidad
  theme_nice() +
  labs(x = "Experiencia del Profesor", y = "Popularidad") +
  theme(legend.position = "none")

#Crecimiento cuadratico - Modelo con interceptos aleatorios 

M2 <- lmer(popular ~ texp + texp*texp + (1|pupil), REML = FALSE, data = popularidad)
summary(M2)

# Crecimiento cuadratico con efectos aleatorios 

M3 <- lmer(popular ~ texp + texp*texp + (1+texp|pupil), REML = FALSE, data = popularidad)
summary(M3)

# Comparacion de modelos 
sjPlot::tab_model(M2,M3)


# Modelo con predictores en nivel 2 

M4 <- lmer(popular ~ texp + texp*texp + sex + (1+texp|pupil), REML = FALSE, data = popularidad)
summary(M4)


# comparacion de modelos con pendientes aleatorias 

tab_model(M3,M4)


# creando columna texp_sex 
texp_sex <- popularidad %>%
  mutate(texp_sex = if_else(sex == "girl", texp, 0))
texp_sex <- as_tibble(texp_sex)


# Modelo con predictores en nivel 2 en la pendiente 
M5 <- lmer(popular ~ texp + texp*texp + sex + texp_sex + 
             (1+texp|pupil), REML = FALSE, data = texp_sex)
summary(M5)


# Comparacion de todos los modelos 

tab_model(M2,M3,M4,M5, dv.labels = c("Modelo 1","Modelo 2", "Modelo 3", "Modelo 4"))











