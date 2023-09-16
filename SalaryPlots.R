library(tidyverse)
#library(lubridate)
library(plotly)
library(scales)


Orig <- read_csv("C:\\Users\\acarmichael\\Desktop\\SaleryData.csv")


NS <- Orig %>%
  filter(Province == "NS", FacultyType == "Full Professor") %>%
  group_by(Institution) %>%
  summarise(MaxSalary = mean(Ceiling), MinSalary = mean(Floor)) %>%
  data.frame()


 NS2 <- NS %>%
   mutate(Institution = as.factor(NS$Institution)) %>%
   rename("variable" = 1, "BarBottom" = 2, "BarTop" = 3) %>%
   mutate(Order = case_when(
                            str_detect(variable,"NSCAD") ~ 1,
                            TRUE ~ 0
   )) %>%
   arrange(desc(Order)) %>%
   mutate(Order = row_number(), variable = fct_reorder(variable, Order)) %>%
   mutate(Colour = case_when(
                            str_detect(variable,"NSCAD") ~"#131f6b",
                            TRUE ~ "#d3d3d3"
   ))
 
 
# # Floating Bar Chart with horizontal bar showing NSCAD min/max to aid in comparing 
# # NSCAD to other institutions
#  
#  ggplot(NS2) +
#    aes(ymin = BarBottom,
#        ymax = BarTop,
#        xmin = as.numeric(variable) - 0.3,  # Setting school bar thickness about the tick marks.
#        xmax = as.numeric(variable) + 0.3
#    ) +
#    geom_rect(fill = 'light grey', col = 'grey') +
#    #scale_x_discrete(labels = levels(NS2$variable), str_wrap(x, width = 15))
#    scale_x_continuous(labels = levels(NS2$variable),
#                     breaks = 1:nlevels(NS2$variable)) +
#    geom_rect(
#      data = data.frame(
#        xmin = min(as.integer(as.factor(NS2$variable))) - 1,
#        xmax = max(as.integer(as.factor(NS2$variable))) + 1,
#        ymin = 100459, 
#        ymax = 130219),
#      aes(xmin = xmin, xmax = xmax + 0.25, ymin = ymin, ymax = ymax),
#      alpha = 0.05, fill = "#131f6b") +
#    coord_cartesian(xlim=c(.5,10)) +
#   # scale_x_discrete(labels = levels(NS2$variable), str_wrap(x, width = 15))
#    theme_classic()
#  
 
 
 # The below code works. Issues:
 # - min/max values for horiz. rect. are hard coded
 # - University names overlap, move to  nicknames? 
ggplot(NS2) +
   aes(ymin = BarBottom,
       ymax = BarTop,
       xmin = as.numeric(variable) - 0.3,  # Setting school bar thickness about the tick marks.
       xmax = as.numeric(variable) + 0.3
       ) +
   geom_rect(fill = NS2$Colour, col = NS2$Colour) +
   scale_x_continuous(labels = str_wrap(levels(NS2$variable),8), breaks = 1:nlevels(NS2$variable)) +
   scale_y_continuous(n.breaks = 7,labels = label_dollar(scale_cut = cut_short_scale())) + 
   geom_rect(
     data = data.frame(
       xmin = min(as.integer(as.factor(NS2$variable))),  # Had a "- 1" here to extend the horizontal bar to the left.
       xmax = max(as.integer(as.factor(NS2$variable))) + 1,
       ymin = 104517.50, 
       ymax = 135483.50),
     aes(xmin = xmin, xmax = xmax + 0.25, ymin = ymin, ymax = ymax),
     alpha = 0.15, fill = "#131f6b") +
   coord_cartesian(xlim=c(0.5,nrow(NS))) +
   ggtitle("Faculty Salary Range", subtitle =  "Atlantic") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
 
 # 
 # 
 # #Testing dynamic vals for retangle.
 # ggplot(NS2) +
 #   aes(ymin = BarBottom,
 #       ymax = BarTop,
 #       xmin = as.numeric(variable) - 0.3,  # Setting school bar thickness about the tick marks.
 #       xmax = as.numeric(variable) + 0.3
 #   ) +
 #   geom_rect(
 #     data = data.frame(
 #       xmin = min(as.integer(as.factor(NS2$variable))) - 1,
 #       xmax = max(as.integer(as.factor(NS2$variable))) + 1,
 #       ymin = 100459, 
 #       ymax = 130219),
 #     aes(xmin = xmin, xmax = xmax + 0.25, ymin = ymin, ymax = ymax),
 #     alpha = 0.15, fill = "#131f6b")
 # 
 # 