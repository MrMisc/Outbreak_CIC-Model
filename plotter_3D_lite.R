
#! /usr/bin/Rscript

f<-file("stdin")
open(f)
record<-c()
while(length(line<-readLines(f,n=1))>0){
  #write(line,stderr())
  record<-c(record,line)  
}


numbers<-record[1:length(record)-1]
numbers_<-c()


print(getwd())
load("echarty_themes.R")
coordinates <- strsplit(record, " ")
# Plot heatmap
# library(ggplot2)
library(pandoc)
library(plotly)

library("ggplot2")
library("plotly")
library("breakDown")
library(dplyr)
# library(ggdark)
library(pracma)
library(comprehenr)
library(ggridges)
library(tidyverse)
library(ggplot2)
library(plotly)
library(thematic)
library(extrafont)
library(pandoc)
# Extract x, y coordinates

print("Beginning read...")


data<-read.csv("output.csv",header = FALSE)
colnames(data) <- c(
  "ContaminantPct1","TotalSamples1_","ContaminantSamples1",
  "HitPct1", "TotalSamples1", "HitSamples1",
  "HitPct2", "TotalSamples2", "HitSamples2","HitPct3","HitSamples3","HitPct4", "TotalSamples4", "HitSamples4","Zone"
)

#Round up values
data$ContaminantSamples1<-ceiling(data$ContaminantSamples1) #Contaminated Hosts
data$HitSamples1<-ceiling(data$HitSamples1) #Infected Hosts
data$HitSamples2<-ceiling(data$HitSamples2) #Eggs
data$HitSamples3<-ceiling(data$HitSamples3) #Colonized Hosts
data$HitSamples4<-ceiling(data$HitSamples4) #Faeces
# Scatter plot for the first 2 sets of data
# Define custom theme colors
thematic_on(bg = "#FCE9D7", fg = "orange", accent = "purple",font = "Yu Gothic")
# 



# Create a unique identifier for each time unit
no_of_zones<-length(unique(data$Zone))
data$TimeUnit <- rep(seq_len(nrow(data) / no_of_zones), each = no_of_zones)





# Farm

fig_dots <- data %>%
  plot_ly(type = "scatter",
          mode = "markers+lines", line = list(width = 0.35))%>%
  add_trace(x = ~TimeUnit,
            y = ~ContaminantPct1,
            frame = ~Zone,  # Use TimeUnit for animation frames
            color = "Contaminated Hosts",
            colors = c("#2A6074", "#00C9B1"),
            size = ~TotalSamples1_,
            customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$ContaminantSamples1, "out of ", zone_data$TotalSamples1_, " hosts @ ",zone_data$TimeUnit," hours")
            },
            hovertemplate = "%{y} % of motile hosts <br> are contaminated  <br> ie %{customdata}") %>%
  add_trace(x = ~TimeUnit,
            y = ~HitPct1,
            frame = ~Zone,  # Use TimeUnit for animation frames
            color = "Infected Hosts",
            colors = c("#2A6074", "#00C9B1"),
            size = ~TotalSamples1,
            customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples1, "out of ", zone_data$TotalSamples1, " hosts @ ",zone_data$TimeUnit," hours")
            },
            hovertemplate = "%{y} % of motile hosts <br> are infected  <br> ie %{customdata}") %>%
  add_trace(x = ~TimeUnit,
            y = ~HitPct3,
            frame = ~Zone,  # Use TimeUnit for animation frames
            color = "Colonized Hosts",
            colors = c("#2A6074", "#00C9B1"),
            size = ~TotalSamples1,
            customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples3, "out of ", zone_data$TotalSamples1, " colonized hosts @ ",zone_data$TimeUnit," hours")
            },
            hovertemplate = "%{y} % of motile hosts <br> are colonized  <br> ie %{customdata}") %>%            
  add_trace(
    x = ~TimeUnit,
    y = ~HitPct2,
    frame = ~Zone,  # Use TimeUnit for animation frames
    color = "Deposits",
    colors = c("#FFF184", "#FFDD80"),
    size = ~TotalSamples2,
    customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples2, "out of ", zone_data$TotalSamples2, " edible/consumable deposits @ ",zone_data$TimeUnit," hours")
            },
    hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
    line = list(width = 0.35)
  )%>%
  add_trace(x = ~TimeUnit,
            y = ~HitPct4,
            frame = ~Zone,  # Use TimeUnit for animation frames
            color = "Faeces",
            colors = c("#2A6074", "#00C9B1"),
            size = ~TotalSamples4,
            customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples4, "out of ", zone_data$TotalSamples4, " faecal matter @ ",zone_data$TimeUnit," hours")
            },
            hovertemplate = "%{y} % of faecal matter <br> are infected  <br> ie %{customdata}") %>%
  layout(title = "Infection Trend within cultivation",
         plot_bgcolor = '#FFF8EE',
         xaxis = list(
           title = "Time (Hours)",
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0'),
         yaxis = list(
           title = "Percentage of Infected",
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0')) %>%
  animation_slider(
    currentvalue = list(font = list(color = "darkgreen"))
  ) %>%
  animation_opts(mode = "next",
                 easing = "exp-in", redraw = FALSE
  )


# Save the animation
htmlwidgets::saveWidget(fig_dots, "scatter.html", selfcontained = TRUE)


#Echarty impl
library(reshape)
library(echarty)
names(data)

names(data)<-c("%Contaminated","Total Hosts","No contaminated",
               "%Infected","Total Hosts(repeated)","No infected",
               "%Eggs Infected","Eggs Amt","Eggs Amt Infected",
               "%Colonized","No Colonized","%Faeces infected",
               "Faeces Amt","Faeces Amt Infected","Zone","TimeUnit")

df<-melt(data,c("Total Hosts","No contaminated","Total Hosts(repeated)","No infected","Eggs Amt","Eggs Amt Infected",
                "No Colonized","Faeces Amt","Faeces Amt Infected","TimeUnit","Zone"))

setting <- list(show = T,type= "scroll",orient= "horizontal", pageButtonPosition= 'start',
                right= "30%",top = 30,width = 470, icon = 'circle', align= 'left', height='85%')
tmp <- df |> group_by(Zone) |> group_split()
cns <- lapply(seq_along(tmp), \(i) { as.list(unique(tmp[[i]]$variable)) })




df %>% mutate(value = round(value,1))%>%
  group_by(Zone) |> 
  ec.init(
    title= list(text= 'Temporal Trends: Contamination/Infection/Colonization Rates Across Hosts, Eggs, and Faeces '),
    xAxis = list(name = 'Time',nameLocation = 'start',
                 nameTextStyle = list(fontWeight ='bolder'),
                 axisLabel = list(rotate = 346,width = 65,
                                  overflow = 'truncate')),
    yAxis = list(max = 100,name = "% compromised",nameLocation = 'start',
                 nameTextStyle = list(fontWeight ='bolder')),
    dataZoom= list(list(type= 'slider',orient = 'vertical'
                   ,left = '2%'),list(type= 'slider',orient = 'horizontal'
                                      ,right = '2%',top='1%', width = '20%')),
    tl.series = list(type  ='line',
                     encode = list(x = 'TimeUnit',y = 'value'), groupBy= 'variable',
                     emphasis= list(focus= 'series',
                                    itemStyle=list(shadowBlur=10,
                                                   shadowColor='rgba(0,0,0,0.5)'),
                                    label= list(position= 'right',
                                                rotate = 350,
                                                show=TRUE))),
    tooltip = list(show = T, trigger = 'axis'))|>
  ec.upd({legend<-setting
  options <- lapply(seq_along(options), \(i) {  
    tita<-title
    tita$text <- paste(tita$text, options[[i]]$title$text)
    options[[i]]$title <- tita   # here we set a title for each timeline step    
    options[[i]]$legend$data <- cns[[i]]  # fine-tune legends: data by continent
    options[[i]] 
  })
  }) |> ec.theme("thing",westeros)

# #Collection

# fig_dots<-data%>%plot_ly(type="scatter",
#           mode = "markers+lines",line = list(width=0.35))%>%
#   add_trace(x = time,
#           y = ~HitPct3,
#           color ="Host",
#           colors=c("#2A6074","#00C9B1"),
#           size = ~TotalSamples3,
#           customdata = ~paste(HitSamples3, "out of ", TotalSamples3," hosts"),
#           hovertemplate="%{y} % of motile hosts <br> are infected  <br> ie %{customdata}")


# fig_dots<-fig_dots %>%
#   add_trace(
#     x = ~time,
#     y = ~HitPct4,
#     color = "Deposits",
#     colors = c("#FFF184", "#FFDD80"),  # Reversed color order
#     size = ~TotalSamples4,
#     customdata = ~paste(HitSamples4, "out of ", TotalSamples4," deposits"),
#     hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
#     line = list(width = 0.35)
#   ) %>%
#   layout(title = "Infection Trend within collection",
#          plot_bgcolor = '#FFF8EE',
#          xaxis = list(
#           title = "Time (Hours)",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'),
#          yaxis = list(
#           title = "Percentage of Infected",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'))


# htmlwidgets::saveWidget(fig_dots, "scatter_plot_2.html", selfcontained = TRUE)




# #Overall

# # print(data$HitSamples1)
# # print(data$HitSamples3)
# # print(data$HitSamples1+data$HitSamples3)

# data$totalhits_motile<-data$HitSamples1+data$HitSamples3
# data$totalhits_sessile<-data$HitSamples2+data$HitSamples4

# data$Total_motile<-data$TotalSamples1+data$TotalSamples3
# data$Total_sessile<-data$TotalSamples2+data$TotalSamples4

# data$totalperc_motile<-data$totalhits_motile/data$Total_motile*100
# data$totalperc_sessile<-data$totalhits_sessile/data$Total_sessile*100

# fig_dots<-data%>%plot_ly(type="scatter",
#           mode = "markers+lines",line = list(width=0.35))%>%
#   add_trace(x = time,
#           y = ~totalperc_motile,
#           color ="Host",
#           colors=c("#2A6074","#00C9B1"),
#           size = ~Total_motile,
#           customdata = ~paste(totalhits_motile, "out of ", Total_motile," hosts"),
#           hovertemplate="%{y} % of motile hosts <br> are infected  <br> ie %{customdata}")


# fig_dots<-fig_dots %>%
#   add_trace(
#     x = ~time,
#     y = ~totalperc_sessile,
#     color = "Deposits",
#     colors = c("#FFF184", "#FFDD80"),  # Reversed color order
#     size = ~Total_sessile,
#     customdata = ~paste(totalhits_sessile, "out of ", Total_sessile," deposits"),
#     hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
#     line = list(width = 0.35)
#   ) %>%
#   layout(title = "Infection Trend across population",
#          plot_bgcolor = '#FFF8EE',
#          xaxis = list(
#           title = "Time (Hours)",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'),
#          yaxis = list(
#           title = "Percentage of Infected",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'))


# htmlwidgets::saveWidget(fig_dots, "scatter_plot_final.html", selfcontained = TRUE)















###DO NOT RUN LATER
rm(data)
library(echarts4r)
library(echarts4r.assets)
library(dplyr)
setwd("E:/Outbreak_CIC_Model/")
extra<-read.csv("extra.csv")
S_<-read.csv("infections_sample.csv")
# Rearranging the data
S_rearranged <- S_ %>%
  mutate(dates = time) %>%
  select( -time) %>%
  rename(groups = interaction, values = count)
data<-S_rearranged
# # Creating the river plot
# data<-subset(S_rearranged, groups != "marker" && groups != "Host 0[*]")
# 
# data|>group_by(groups)|>
#   e_charts(dates) |>
#   e_line(values, stack = "grp2") |>
#   e_tooltip() |>
#   e_theme("vintage")|>
#   e_datazoom()
# 



# Convert 'dates' column to numeric (if it's not already numeric)
data$dates <- as.numeric(data$dates)

# Create a template dataframe with all unique combinations of groups and dates
all_combinations <- expand.grid(
  groups = unique(data$groups),
  dates = unique(data$dates)
)

# Merge the template with the existing data
filled_data <- merge(all_combinations, data, by = c("groups", "dates"), all = TRUE)

# Replace missing values with 0
filled_data[is.na(filled_data$values), "values"] <- 0

# e_theme(
#   e,
#   name = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk", "cool",
#            "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom", "dark",
#            "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green", "halloween",
#            "helianthus", "infographic", "inspired", "jazz", "london", "macarons", "macarons2",
#            "mint", "purple-passion", "red-velvet", "red", "roma", "royal", "sakura", "shine",
#            "tech-blue", "vintage", "walden", "wef", "weforum", "westeros", "wonderland")
# )



# Sort the combined data by 'groups' and 'dates'
filled_data <- filled_data[order(filled_data$groups, filled_data$dates), ]
filled_data|>group_by(groups)|>
  e_charts(dates) |>
  e_area(values,
         emphasis = list(
           focus = "self"
         )) |> 
  e_y_axis(min = 0)|>
  e_tooltip()  |>
  e_theme("westeros")|>
  e_datazoom(
    type = "slider",
    toolbox = TRUE,
    bottom = 10
  )|>
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")
  
summed_data <- filled_data %>%
  group_by(groups, dates) %>%
  summarise(total_values = sum(values)) %>%
  ungroup() %>%
  group_by(groups) %>%
  summarise(values = sum(total_values))

subset(summed_data,groups!="marker")|> e_charts(groups)|>e_pie(values, roseType = "radius")|>
  e_legend(show = FALSE)


##COMBINE
filled_data|>group_by(groups)|>
  e_charts(dates) |>
  e_area(values,
         emphasis = list(
           focus = "self"
         )) |> 
  e_y_axis(min = 0)|>
  e_tooltip()  |>
  e_theme("westeros")|>
  e_datazoom(
    type = "slider",
    toolbox = TRUE,
    bottom = 10
  )|>
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")|>
  e_data(summed_data,values)|>e_pie(values, roseType = "radius")|>
  e_legend(show = FALSE)
 

