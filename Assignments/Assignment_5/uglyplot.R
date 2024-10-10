library(tidyverse)
library(ggimage)

bill <- ggplot(billboard, aes(x=artist, y=wk1)) +
  geom_point(color="red", size=10, shape=8) +
  labs(title="pqweR$%Trg.q/><jj;l1j2340GGE!vnafl;j", # Title and Labels
       subtitle="Dog",
       y=".59$6&@#eyj;lij>>>>>WK1",
       x=">89054^&%^GI(Gjgl;rltkj>>>>>ARTIST",
       caption="Billboard") +
  theme(axis.title.x = element_text(angle=180, size = 24),
        axis.title.y = element_text(angle=80),
        axis.title = element_text(color = "green"),
        plot.title = element_text(family = "Courier", size = 24, color = "red"),
        plot.subtitle = element_text(size = 32, color = "green"),
        panel.grid = element_line(color="green"),
        plot.caption = element_text(color = "purple", size = 40),
        axis.text.x = element_text(color = "red", size=10, angle=25))

ggbackground(bill, "./images/funny_dog.jpeg")
