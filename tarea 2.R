library(tidyquant)
library(ggthemes)
#1
symbols<-c("ORCL","AMZN","NVDA","AMD")
precio<-tq_get(symbols,
               get="stock.prices",
               from="2000-01-01",
               to="2018-12-31",
               periodicity="daily")
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
               col_rename = "returns_monthly",
  ) %>%
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
gretorno_mensual<-retorno_mensual_tbl  %>%
  gather(symbols, returns, -date)
qqnorm(gretorno_mensual$returns)
qqline(gretorno_mensual$returns)



