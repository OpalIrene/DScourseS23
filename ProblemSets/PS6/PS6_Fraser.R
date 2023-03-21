library(readxl)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(gridExtra)

GenderStats <- read_excel("ProblemSets/PS6/GenderStats.xlsx")

x <- GenderStats$Country
m <- GenderStats$`Financial institution account,male(% age 15+)`
f <- GenderStats$`Financial institution account,female(% age 15+)` 

# create the first plot using ggplot(1)
M <- ggplot(GenderStats, aes(x=x, y=m, color=x)) +
  geom_point(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x="Country", y="Male(% age 15+) with Financial account", color="Country") +
  theme(legend.position = "bottom") 

# save the plot as a PNG file
ggsave("Males.png", M, width=7, height=4.75, dpi=300)

# plot second plot using ggplot(2)
F <- ggplot(GenderStats, aes(x=x, y=f, color=x)) +
  geom_point(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x="Country", y="Female(% age 15+) with Financial account", color="Country") +
  theme(legend.position = "bottom")

# save the plot as a PNG file
ggsave("Females.png", F, width=7, height=4.75, dpi=300)

# created plot1
plot1 <- ggplot(GenderStats, aes(x=x, y=m, color=x)) +
  geom_bar(stat="identity") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x="Country", y="Male(% age 15+) with Financial account")

# created plot2
plot2 <- ggplot(GenderStats, aes(x=x, y=f, color=x))+
  geom_bar(stat="identity") +
  theme_dark() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x="Country", y="Female(% age 15+) with Financial account")

# combine the plots into one
combined_plot2 <- grid.arrange(plot1, plot2, ncol=2)

# save the combined plot as a PNG file
ggsave("combined_plot.png", combined_plot2, width=10, height=5, dpi=300)









