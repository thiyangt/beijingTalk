library(tidyverse)
library(gganimate)

timedf <- data.frame(Temp=cumsum(rnorm(4000)),  grp=rep(1:200, each=20), Day=rep(1:20,200))
p <- ggplot(
  timedf,
  aes(Day, Temp, color = factor(grp))
) +
  geom_line()+
  scale_color_viridis_d() +
  labs(x = "", y = "") +
  theme(legend.position = "none", axis.text.x=element_blank())
anim <- p +
  transition_reveal(Day)
anim_save("anim.gif", anim)
