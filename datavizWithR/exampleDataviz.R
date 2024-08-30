### This works well in RStudio, not in R Jupyter Notebook so much

library(readxl)
top50 <- read_excel("src/maths_notes/datavizWithR/top_50_cities_by_homicide_2018.xlsx")

# the rankings were incorrect
top50$Rank = rank(top50$Rate)

# make them ascending
library(forcats)
top50$City <- fct_reorder(top50$City, top50$Rank, .desc = FALSE)

# units to millions
top50$Population <- top50$Population/1000000
table(top50$Country)

# group several smaller countries into 1

top50$Country_2<- fct_collapse(top50$Country,
                               Brazil = "Brazil",
                               Mexico = "Mexico",
                               United_States = "United States",
                               Venezuela = "Venezuela",
                               South_Africa = "South Africa",
                               Other = c("Colombia", "El Salvador","Guatemala","Honduras","Jamaica")
)

table(top50$Country_2)

# order and label (factor variable)

top50$Country_2 <- factor(top50$Country_2,
                          levels = c("Mexico",
                                     "Brazil",
                                     "Venezuela",
                                     "Other",
                                     "United_States",
                                     "South_Africa"
                          ), 
                          labels = c("Mexico",
                                     "Brazil",
                                     "Venezuela",
                                     "Other*",
                                     "United States",
                                     "South Africa"
                          ))
table(top50$Country_2)

# this shows that rows represent observations (and single cities).

library(data.table)
top50
length(top50)
length(top50$City)

# ggplot likes having long data, so arrange it long

library(tidyr)
top50_long <- gather(top50, key = "Variable", value = "Value", Homicides:Rate)

top50_long
length(top50_long)
length(top50_long$City)

# add a rank variable

top50_long$Rank_labels <- c(rank(-top50$Homicides,ties.method = "min"),
                            rank(-top50$Population,,ties.method = "min"),
                            rank(-top50$Rate,ties.method = "min"))

# make factors presentable for visualisation

top50_long$Variable <- factor(top50_long$Variable,
                              levels = c("Rate","Homicides","Population"),
                              labels = c("Homicides per 100,000", 
                                         "Total Homicides",
                                         "Population Size (Millions)"))



# let's see it

library("ggplot2")

p <- ggplot(data = top50_long, 
            aes(x = City, y = Value, fill = Country_2)) +
  geom_bar(stat = "identity") + coord_flip() + 
  facet_grid(.~Variable, scales = "free")
p

# add some ranks

p <- p + 
  geom_text(aes(label = Rank_labels, y = 0), 
            fill = "gray", hjust = "top", family="Georgia")
p

# chart title and other details

p <- p +
  labs(title = "Top 50 Most Dangerous Cities Ranked - 2018",
       subtitle  = "Cities from Mexico and Brazil dominate the top 50 and smaller cities are not necessarily safer",
       caption = "Source: Citizen Council for Public Security and Criminal Justice AC (2018) - http://seguridadjusticiaypaz.org.mx/files/Metodologia.pdf 
       *Other countries include Colombia (Palmira, Cali), El Salvador (San Salvador), Guatemala (Guatemala City), Honduras (San Pedro Sula, Distrito Central/Tegucigalpa), Jamaica (Kingston)")
p

# stylistic changes

background <- "#EFF1F0"

pal <- c("#89141cff",
         "#b16264ff",
         "#d8b0acff",
         "#d8d0c1ff",
         "#3a506bff",
         "#29265bff"
)

install.packages("extrafont")
library(extrafont)
font_import()

p <- p + 
  theme_gray() + 
  scale_fill_manual(values = pal) +
  theme(plot.background = element_rect(fill = background),
        panel.background = element_rect(fill = background),
        legend.background = element_rect(fill = background),
        text=element_text(family="Georgia"),
        title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())
p