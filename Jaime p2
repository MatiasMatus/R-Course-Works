#pregunta 2
googleps %>%  group_by(Category) %>%  count() %>% 
  ggplot(aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "#02A907") +
  geom_text(aes(label= paste0(n, " apps"), vjust= 0.3, hjust=-0.1), size =2.7) +
  scale_y_continuous(limits = c(0,2000), labels = NULL) + 
  labs(x=NULL, title = "Aplicaciones por categoría") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 7.5),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_flip()
