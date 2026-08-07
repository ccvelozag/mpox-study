################ ----------------------------

#### Graficos de perfiles

################ ----------------------------

# Cargar las librerías:

library(dplyr)
library(modeest)
library(ggplot2)
library(ggThemeAssist)
library(tidyr)
library(openxlsx)
library(readxl)
library(stringr)

# Cargar base de datos de los EA

evad<-read_xlsx("20250801_ea no serios.xlsx")

# Omitir los registros de los pacientes que tuvieron falla por selección:

evad <- evad %>%
  filter(!(codigo_participante %in% c("COL-CUC-243",
                                      "COL-ICL-339",
                                      "COL-ICL-545",
                                      "COL-CUC-641")))


# Codificar grupos

#evad$confirmacion_entidad[evad$confirmacion_entidad == "VIH"]<-"HIV"
#evad$confirmacion_entidad[evad$confirmacion_entidad == "Factor de riesgo"]<-"Risk factor"
evad$confirmacion_entidad[evad$confirmacion_entidad == "Prep"]<-"PrEP"
#
#evad$categorias_cd4[evad$categorias_cd4 == "VIH 200-349"]<-"HIV 200-349"
#evad$categorias_cd4[evad$categorias_cd4 == "VIH 350-499"]<-"HIV 350-499"
#evad$categorias_cd4[evad$categorias_cd4 == "VIH 500-749"]<-"HIV 500-749"
#evad$categorias_cd4[evad$categorias_cd4 == "VIH 750-999"]<-"HIV 750-999"
#evad$categorias_cd4[evad$categorias_cd4 == "VIH mayor 1000"]<-"HIV ≥ 1,000"
#evad$categorias_cd4[evad$categorias_cd4 == "Factor de riesgo"]<-"Risk factor"
#evad$categorias_cd4[evad$categorias_cd4 == "Prep"]<-"PrEP"
#
#evad$intensidad_cat[evad$intensidad_cat == "Leve"]<-"Mild"
#evad$intensidad_cat[evad$intensidad_cat == "Moderado"]<-"Moderate"
#evad$intensidad_cat[evad$intensidad_cat == "Severo"]<-"Severe"
#
#evad$intensidad_cat2[evad$intensidad_cat2 == "Menor a 3"]<-"< 3"
#evad$intensidad_cat2[evad$intensidad_cat2 == "Mayor o igual a 3"]<-"≥ 3"
#
#evad$causalidad_cat[evad$causalidad_cat == "Relacionado"]<-"Related"
#evad$causalidad_cat[evad$causalidad_cat == "No relacionado"]<-"Not related"


levels(as.factor(evad$tipo))

colnames(evad)

############## --------------------------- Locales ---------------------

locales <- evad %>%
  filter(tipo == "local") %>%
  filter(local_events_cat != "Otro")%>%
  dplyr::select(codigo_participante,formato,institucion_participante,
                confirmacion_entidad,categorias_cd4,
                intensidad_cat,intensidad_cat2,causalidad_cat,
                local_events_cat)%>%
  mutate(cat_hiv = ifelse(confirmacion_entidad == "VIH","VIH +","VIH -"))


levels(as.factor(locales$local_events_cat))

locales$local_events_cat[locales$local_events_cat == "Dolor en el sitio de la inyección"]<-"Dolor"
locales$local_events_cat[locales$local_events_cat == "Edema del sitio de inyección"]<-"Edema"
locales$local_events_cat[locales$local_events_cat == "Enrojecimiento en el sitio de inyección"]<-"Eritema"
locales$local_events_cat[locales$local_events_cat == "Induración del sitio de inyección"]<-"Induración"
locales$local_events_cat[locales$local_events_cat == "Prurito"]<-"Prurito"
#locales$local_events_cat[locales$local_events_cat == "Otro"]<-"Other"


levels(as.factor(locales$local_events_cat))
levels(as.factor(locales$intensidad_cat))
levels(as.factor(locales$intensidad_cat2))

levels(as.factor(locales$cat_hiv))

levels(as.factor(locales$confirmacion_entidad))
levels(as.factor(locales$causalidad_cat))


locales <- locales %>%
  mutate(
    local_events_cat = factor(local_events_cat, levels = c("Dolor", "Eritema", "Edema", 
                                                           "Induración", "Prurito","Take")),
    cat_hiv = factor(cat_hiv, levels = c("VIH +", "VIH -")),
    confirmacion_entidad = factor(confirmacion_entidad,levels = c("VIH",
                                                                  "PrEP",
                                                                  "Factor de riesgo")),
    intensidad_cat = factor(intensidad_cat,levels=c("Moderado","Leve")),
    causalidad_cat = factor(causalidad_cat,levels=c("Relacionado",
                                                    "No relacionado"))
  )


# Graficar

# Forzar el orden de apilado: primero Mild, luego Moderate
locales$intensidad_cat <- factor(locales$intensidad_cat, levels = c("Leve","Moderado"))

# Gráfico
# HIV+ y HIV-
ggplot(locales, aes(x = cat_hiv, fill = intensidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ local_events_cat, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Mild" = "#6A9EDA", "Moderate" = "#D0338B")
  ) +
  labs(
    title = "Local adverse events in HIV+ and HIV- Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 90))+
  guides(fill = guide_legend(reverse = TRUE))


# - QUEDE AQUIII: Agrupar 'locales' por cat_vih y local_events_cat. Luego 
# hacer otra agrupación agregando insendiad_cat

locales_ <- locales %>%
  group_by(cat_hiv,local_events_cat)%>%
  summarise(cantidad=n())%>%
  mutate(total = ifelse(cat_hiv == "VIH +",282,240),
         prop = round((cantidad/total)*100,2))%>%
  as.data.frame()

locales_inten <- locales %>%
  group_by(cat_hiv,local_events_cat,intensidad_cat)%>%
  summarise(cantidad_inten = n())%>%
  as.data.frame()
  

# Adjuntar tablas:

locales_pacientes <- locales_ %>%
  left_join(locales_inten,by = c("cat_hiv","local_events_cat"))%>%
  mutate(prop_inten = round((cantidad_inten/total)*100,2))


#write.xlsx(locales_pacientes,"20251124_EA locales.xlsx")



ggplot(locales_pacientes, aes(
  x = cat_hiv,
  y = prop_inten,
  fill = intensidad_cat
)) +
  geom_col(position = "stack", width = 0.85) +   # stat = "identity"
  
  # === ETIQUETAS SOLO DE UNA FILA POR BARRA ===
  geom_text(
    data = locales_pacientes %>% filter(intensidad_cat == "Leve"),
    aes(
      x = cat_hiv,
      y = prop,
      label = format(prop, big.mark=".", decimal.mark=",", nsmall=2)
    ),
    vjust = -0.4,
    fontface = "bold",
    size = 3.5
  )+
  #geom_text(
  #  data = locales_pacientes %>% filter(intensidad_cat == "Mild"),
  #  aes(label = format(prop, big.mark = ".", decimal.mark = ",", nsmall = 2)),
  #  vjust = -0.6,
  #  fontface = "bold",
  #  size = 3.5
  #) +
  
  facet_wrap(~ local_events_cat, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Leve" = "#9FC66D", "Moderado" = "#5E88C3")
  ) +
  labs(
    title = "Eventos adversos locales en participantes con VIH+ o VIH-, según severidad",
    y = "Porcentaje de participantes",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, max(locales_pacientes$prop_inten)*1.2)) +
  guides(fill = guide_legend(reverse = TRUE))








# HIV, PrEP and Risk factors
ggplot(locales, aes(x = confirmacion_entidad, fill = intensidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ local_events_cat, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Mild" = "#6A9EDA",
               "Moderate" = "#D0338B")  # Puedes agregar Moderate si lo necesitas
  ) +
  labs(
    title = "Local adverse events in HIV, PrEP and Risk Factor \n Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,90))+
  guides(fill = guide_legend(reverse = TRUE))




table(locales$causalidad_cat)

locales$causalidad_cat <- factor(locales$causalidad_cat, levels = c("Not related","Related"))


# HIV+ y HIV-
ggplot(locales, aes(x = cat_hiv, fill = causalidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ local_events_cat, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Related" = "lightskyblue",
               "Not related" = "deepskyblue4")  
  ) +
  labs(
    title = "Local adverse events in HIV+ and HIV- Populations by Causality",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,90))+
  guides(fill = guide_legend(reverse = TRUE))


# HIV, PrEP and Risk factors
ggplot(locales, aes(x = confirmacion_entidad, fill = causalidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ local_events_cat, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Related" = "lightskyblue",
               "Not related" = "deepskyblue4")  
  ) +
  labs(
    title = "Local adverse events in HIV, PrEP and Risk Factor \n Populations by Causality",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,90))+
  guides(fill = guide_legend(reverse = TRUE))



############## --------------------------- Sistemicos ---------------------

sistemicos <- evad %>%
  filter(tipo == "sistemicos") %>%
  filter(systemic_events_cat != "Otro")%>%
  dplyr::select(codigo_participante,formato,institucion_participante,
                confirmacion_entidad,categorias_cd4,
                intensidad_cat,intensidad_cat2,causalidad_cat,
                systemic_events_cat)%>%
  mutate(cat_hiv = ifelse(confirmacion_entidad == "VIH","VIH +","VIH -"))


levels(as.factor(sistemicos$systemic_events_cat))

sistemicos$sistemicos_categoria<-ifelse(sistemicos$systemic_events_cat == "Fatiga",
                                        "Fatiga",
                                        ifelse(sistemicos$systemic_events_cat == "Linfadenopatías axilares",
                                               "Linfadenopatías",
                                               ifelse(sistemicos$systemic_events_cat == "Dolor de cabeza",
                                                      "Dolor de cabeza",
                                                      ifelse(sistemicos$systemic_events_cat == "Fiebre",
                                                             "Fiebre",
                                                             ifelse(sistemicos$systemic_events_cat == "Respiratorios",
                                                                    "Respiratorios",
                                                                    ifelse(sistemicos$systemic_events_cat == "Gastrointestinales",
                                                                           "Gastrointestinales",ifelse(sistemicos$systemic_events_cat == "Dermatológicos",
                                                                                                     "Dermatológicos","Other")))))))


sistemicos <- sistemicos %>%
  filter(sistemicos_categoria != "Other")



levels(as.factor(evad$intensidad_cat))
levels(as.factor(sistemicos$intensidad_cat))
levels(as.factor(sistemicos$intensidad_cat2))

levels(as.factor(sistemicos$cat_hiv))

levels(as.factor(sistemicos$confirmacion_entidad))
levels(as.factor(sistemicos$causalidad_cat))

sistemicos <- sistemicos %>%
  mutate(
    sistemicos_categoria = factor(sistemicos_categoria, levels = c("Fatiga", 
                                                                   "Linfadenopatías", 
                                                                   "Dolor de cabeza", 
                                                                   "Fiebre", 
                                                                   "Respiratorios",
                                                                   "Gastrointestinales",
                                                                   "Dermatológicos")),
    cat_hiv = factor(cat_hiv, levels = c("VIH +", "VIH -")),
    confirmacion_entidad = factor(confirmacion_entidad,levels = c("VIH",
                                                                  "PrEP",
                                                                  "Factor de riesgo"))
  )


# Graficar

sistemicos$intensidad_cat <- factor(sistemicos$intensidad_cat, levels = c("Severo",
                                                                          "Moderado","Leve"))



sistemicos_ <- sistemicos %>%
  group_by(cat_hiv,sistemicos_categoria)%>%
  summarise(cantidad=n())%>%
  mutate(total = ifelse(cat_hiv == "VIH +",282,240),
         prop = round((cantidad/total)*100,2))%>%
  as.data.frame()

sistemicos_inten <- sistemicos %>%
  group_by(cat_hiv,sistemicos_categoria,intensidad_cat)%>%
  summarise(cantidad_inten = n())%>%
  as.data.frame()


# Adjuntar tablas:

sistemicos_pacientes <- sistemicos_ %>%
  left_join(sistemicos_inten,by = c("cat_hiv","sistemicos_categoria"))%>%
  mutate(prop_inten = round((cantidad_inten/total)*100,2))


#write.xlsx(locales_pacientes,"20251124_EA locales.xlsx")

labels_tot <- sistemicos_pacientes %>%
  group_by(sistemicos_categoria, cat_hiv) %>%
  summarise(total = sum(prop_inten), .groups = "drop")


ggplot(sistemicos_pacientes, aes(
  x = cat_hiv,
  y = prop_inten,
  fill = intensidad_cat
)) +
  geom_col(position = "stack", width = 0.85) +   # stat = "identity"
  
  # === ETIQUETAS SOLO DE UNA FILA POR BARRA ===
  geom_text(
    data = sistemicos_pacientes %>% filter(intensidad_cat == "Moderado"),
    aes(
      x = cat_hiv,
      y = prop,
      label = format(prop, big.mark=".", decimal.mark=",", nsmall=2)
    ),
    vjust = -0.4,
    fontface = "bold",
    size = 3.5
  )+
  #geom_text(
  #  data = sistemicos_pacientes %>% filter(intensidad_cat == "Moderate"),
  #  aes(label = format(prop, big.mark = ".", decimal.mark = ",", nsmall = 2)),
  #  vjust = -0.6,
  #  fontface = "bold",
  #  size = 3.5
  #) +
  
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Leve" = "#9FC66D", 
               "Moderado" = "#5E88C3",
               "Severo" = "#E59F3E")
  ) +
  labs(
    title = "Eventos adversos sistémicos en participantes con VIH+ o VIH-, según severidad",
    y = "Porcentaje de participantes",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, max(sistemicos_pacientes$prop_inten)*1.1)) +
  guides(fill = guide_legend(reverse = TRUE))









# HIV+ y HIV-
ggplot(sistemicos, aes(x = cat_hiv, fill = intensidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Mild" = "#6A9EDA",
               "Moderate" = "#D0338B",
               "Severe" = "#00BFFF")  # Puedes agregar Moderate si lo necesitas
  ) +
  labs(
    title = "Systemic adverse events in HIV+ and HIV- Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))


# HIV, PrEP and Risk factors

ggplot(sistemicos, aes(x = confirmacion_entidad, fill = intensidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Mild" = "#6A9EDA",
               "Moderate" = "#D0338B",
               "Severe" = "#00BFFF")  # Puedes agregar Moderate si lo necesitas
  ) +
  labs(
    title = "Systemic adverse events in HIV, PrEP and Risk Factor \n Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))




sistemicos$intensidad_cat2 <- factor(sistemicos$intensidad_cat2, levels = c("≥ 3",
                                                                          "< 3"))

# HIV+ y HIV-
ggplot(sistemicos, aes(x = cat_hiv, fill = intensidad_cat2)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("< 3" = "#6A9EDA",
               "≥ 3" = "#D0338B")  
  ) +
  labs(
    title = "Systemic adverse events in HIV+ and HIV- Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))


# HIV, PrEP and Risk factors

ggplot(sistemicos, aes(x = confirmacion_entidad, fill = intensidad_cat2)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("< 3" = "#6A9EDA",
               "≥ 3" = "#D0338B")  
  ) +
  labs(
    title = "Systemic adverse events in HIV, PrEP and Risk Factor \n Populations by Severity",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))




sistemicos$causalidad_cat <- factor(sistemicos$causalidad_cat, levels = c("Not related","Related"))


# HIV+ y HIV-
ggplot(sistemicos, aes(x = cat_hiv, fill = causalidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Related" = "lightskyblue",
               "Not related" = "deepskyblue4")  
  ) +
  labs(
    title = "Systemic adverse events in HIV+ and HIV- Populations by Causality",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))


# HIV, PrEP and Risk factors
ggplot(sistemicos, aes(x = confirmacion_entidad, fill = causalidad_cat)) +
  geom_bar(position = "stack", width = 0.7) +
  facet_wrap(~ sistemicos_categoria, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c("Related" = "lightskyblue",
               "Not related" = "deepskyblue4")  
  ) +
  labs(
    title = "Systemic adverse events in HIV, PrEP and Risk Factor \n Populations by Causality",
    y = "Number of adverse events", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "grey60"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_y_continuous(limits = c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))






