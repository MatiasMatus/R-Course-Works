library(tidyverse)
library(readr)

#1
datos <- read_csv("../R-homework2/dataset/googleplaystore.csv")
datos %>%  filter(is.na(Rating) == FALSE)%>% 
  ggplot(aes(x=Rating),!is.na(datos$Rating)) + 
  geom_density(color="darkblue", fill="lightblue")
#2
datos %>%  group_by(Category) %>%  count() %>% 
  ggplot(aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "#02A907") +
  geom_text(aes(label= paste0(n, " apps"), vjust= 0.3, hjust=-0.1), size =2.7) +
  scale_y_continuous(limits = c(0,2000), labels = NULL) + 
  labs(x=NULL, title = "Aplicaciones por categoria") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 7.5),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_flip()
#3
datos %>% filter(is.na(`Content Rating`) == FALSE)%>% 
  ggplot(aes(x=`Content Rating`, y= Rating)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552")+scale_x_discrete(name = "Rango Etario") +
  scale_y_continuous(name = "Ranting")

#4
datos$Type <- as.factor(datos$Type)
googleps2 <- datos %>% mutate(Type=recode_factor(Type, "1" = "Paid",
                                                 "0" = "Free"))

googleps2 %>% ggplot(aes(x = Type, y = Rating, fill = Type)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  scale_y_continuous(limits = c(0,5)) + 
  labs(x = NULL,    
       y = "Rating") +
  scale_fill_manual("", values = c("#8d1414","#377EB8")) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line())
#5
library(stringr)
library(dplyr)
googleps12 <- read_csv("../R-homework2/dataset/googleplaystore.csv")
googleps12 <-googleps12 %>%  mutate(year=str_sub(googleps12$`Last Updated`, start = -4))
googleps12 %>% group_by(year) %>% filter(year %in% c(2010:2018)) %>% summarise(sumreview=sum(Reviews)) %>%
  mutate(pctgreview=sumreview/sum(sumreview))
datos <-datos %>%  mutate(year=str_sub(datos$`Last Updated`, start = -4))
datos<-datos[!(datos$year %in% c("2010","2011","2012","2013","2014","2015","2016")),]
datos$pctg <- (datos$Reviews/sum(datos$Reviews, na.rm = TRUE)*100)
datos2 <- data.frame("Fecha" = datos$year, 
                  "Review" = datos$Reviews) 
datos2$pctg <- (datos2$Review/sum(datos2$Review, na.rm = TRUE)*100)

data5 <-datos2 %>% filter(Fecha %in% c("2017","2018")) %>% group_by(Fecha) %>%
  summarise(sumreview = sum(Review)) %>% mutate(PctgReview=sumreview/sum(sumreview)*100)

ggplot(data5, aes(x="", y=PctgReview, fill=Fecha)) + geom_bar(stat="identity", width=1)+
 coord_polar("y", start=0) + geom_text(aes(label = paste0(round(PctgReview), "%")), position = position_stack(vjust = 0.5))



#ITEM2  

#1
precio<-tq_get(symbols,
               get="stock.prices",
               from="2000-01-01",
               to="2018-12-31",
               periodicity="daily")
precio

#2
precio_mensual <- tq_get(symbols,
                         get = "stock.prices",
                         from = "2000-01-01",
                         to = "2018-12-31",
                         periodicity = "monthly") %>% 
  select(date, symbol, close) %>% 
  spread(symbol, close)

retorno_mensual_tbl <- precio_mensual %>% 
  gather(symbols, close, -date) %>% 
  group_by(symbols) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "returns_monthly" ) %>%
  slice(-1) %>% 
  spread(symbols, returns_monthly)

retorno_acumulado_tbl <- retorno_mensual_tbl %>% 
  gather(symbols, returns_monthly, -date) %>% 
  group_by(symbols) %>% 
  mutate(returns_acum = cumsum(returns_monthly)) %>%
  select(-returns_monthly) %>% 
  spread(symbols, returns_acum)

retorno_mensual_tbl  %>%
  gather(symbols, returns, -date) %>% 
  ggplot(mapping = aes(x = returns, fill = symbols)) + 
  geom_density(alpha = 0.5) +
  labs(title = "Retornos Activos", subtitle = "Oracle (ORCL), Amazon (AMZN), Nvidia (NVDA), Amd(AMD)",
       x = "Retornos Mensuales", y = "Densidad") + 
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ symbols, ncol = 2) + 
  guides(fill=guide_legend(title="Activos:"))

retorno_acumulado_tbl %>%
  gather(symbols, returns_acum, -date) %>% 
  ggplot(mapping = aes(x = date, y = returns_acum, color = symbols)) +
  geom_line(size=1) + 
  labs(title = "Retornos Acumulado", subtitle = "Oracle (ORCL), Amazon (AMZN), Nvidia (NVDA), Amd(AMD)",
       x = "Periodo", y = "Retorno Acumulado") + 
  theme_tq() +
  scale_fill_tq() + 
  guides(color = guide_legend(title="Activos:")) + 
  scale_y_continuous(labels = scales::percent, breaks=seq(-5,5,1))

#3

qqnorm(retorno_mensual_tbl$AMD, datax=FALSE, main="AMD Q-Q Plot")
qqline(retorno_mensual_tbl$AMD)

qqnorm(retorno_mensual_tbl$AMZN, datax=FALSE, main="AMZN Q-Q Plot")
qqline(retorno_mensual_tbl$AMZN)

qqnorm(retorno_mensual_tbl$ORCL, datax=FALSE, main="ORCL Q-Q Plot")
qqline(retorno_mensual_tbl$ORCL)

qqnorm(retorno_mensual_tbl$NVDA, datax=FALSE, main="NVDA Q-Q Plot")
qqline(retorno_mensual_tbl$NVDA)

#4
end<- "2018-12-28"
precio %>% filter(symbol =="AMD") %>% 
  ggplot(aes(x=date, y=close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +                                   
  geom_bbands(ma_fun = SMA, sd = 2, n = 30) +   
  labs(title = "Standard & Poor 500 Candlestick Chart", 
       subtitle = "BBands con SMA", 
       y = "Precio de Cierre", x = "Fecha") + 
  coord_x_date(xlim = c("2018-11-01", end),
               ylim=c(10,32))+
  theme_tq()

precio  %>% filter(symbol=="AMZN") %>% 
  ggplot(aes(x=date, y=close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +                                   
  geom_bbands(ma_fun = SMA, sd = 2, n = 30) +   
  labs(title = "Standard & Poor 500 Candlestick Chart", 
       subtitle = "BBands con SMA", 
       y = "Precio de Cierre", x = "Fecha") + 
  coord_x_date(xlim = c("2018-11-01", end),
               ylim=c(1340,1800))+
  theme_tq()

precio  %>% filter(symbol=="ORCL") %>% 
  ggplot(aes(x=date, y=close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +                                   
  geom_bbands(ma_fun = SMA, sd = 2, n = 30) +   
  labs(title = "Standard & Poor 500 Candlestick Chart", 
       subtitle = "BBands con SMA", 
       y = "Precio de Cierre", x = "Fecha") + 
  coord_x_date(xlim = c("2018-11-01", end),
               ylim=c(42.5,52.5))+
  theme_tq()

precio  %>% filter(symbol=="NVDA") %>% 
  ggplot(aes(x=date, y=close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +                                   
  geom_bbands(ma_fun = SMA, sd = 2, n = 30) +   
  labs(title = "Standard & Poor 500 Candlestick Chart", 
       subtitle = "BBands con SMA", 
       y = "Precio de Cierre", x = "Fecha") + 
  coord_x_date(xlim = c("2018-11-01", end),
               ylim=c(100,250))+
  theme_tq()

#5
library("IntroCompFinR")
mean <- apply(retorno_mensual_tbl[2:5],2, mean)
sd <- apply(retorno_mensual_tbl[2:5],2, sd)
cov  <- cov(retorno_mensual_tbl[2:5]) 

port<-globalMin.portfolio(mean,cov,shorts=TRUE) 
port
ggplot() + geom_point(aes(port$sd, port$er, color = "1"), size=5)+
  geom_point(aes(sd, mean, color = "2"), size=5)+
  geom_text(aes(sd, mean, color = "black", label=paste0(round(mean*100,2),"%")), size=3, vjust=-1.4)+
  geom_text(aes(port$sd, port$er, color = "black", label=paste0(round(port$er*100,2),"%")), size=3, vjust=-1.4)+
  scale_y_continuous(breaks = seq(0,0.2, 0.003),limits = c(0,0.02))+
  scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0.06,0.2))+
  scale_color_manual("", values = c("blue", "red", "black"), labels = c("Minima Varianza", "Stocks=1"), breaks=c(1,2))+
  theme_bw() + xlab("Desviacion Estandar") + ylab("Retorno Esperado")+
  ggtitle("Set Minima Varianza")

retorno_mensual_tbl %>% gather(symbols, close, -date) %>% 
  mutate(close = round(close,4)*100) %>% 
  ggplot(aes(x=date, y=close)) + 
  geom_bar(stat="identity", fill = "red") + 
  labs(title = "Retornos Portfolio",
       subtitle = "Data Viz. usando ggplot2", 
       x = "Fecha", y = "Retornos (%)") +  
  theme_tq()
