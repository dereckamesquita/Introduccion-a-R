agroup <- read_delim("proyecto real/agroup.csv", 
                       +     ";", escape_double = FALSE, trim_ws = TRUE)
colnames(agroup)[colnames(agroup) == "total"] <- "Keyword organicas"

pairs(agroup[,5:8])
agroup
cor(agroup[,5:8])
agroup$`Organic traffic`
library(dplyr)
agroup <- agroup %>%
  mutate(pagesper = pages/dominios)
agroup$pagesper
cor(agroup[,6:9])

ggplot(data=agroup,
       mapping = aes(pagesper, `Organic traffic`))+
  geom_point()+
  labs(x="paginas por dominio", y="Organic traffic", 
       title ="Relacion paginas por dominio con el trafico organico" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(agroup$`Organic traffic`, agroup$`Keyword organicas`)
