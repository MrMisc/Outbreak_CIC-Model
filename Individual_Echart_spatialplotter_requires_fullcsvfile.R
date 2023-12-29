
library(ggplot2)
library(pandoc)
library(plotly)
library(echarts4r)
library(echarts4r.assets)
library("ggplot2")
library("plotly")
library("breakDown")
library(dplyr)
library(ggdark)
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

data<-read.csv("sample.csv")
minimum<-min(data$time)
maximum<-max(data$time)
for (separate_zone in unique(data$zone)){

  data_<-subset(data, zone == separate_zone)

  f<-max(max(data$x),max(data$y),max(data$z))/200
  f_<-0.3*f
  
  df<-data_
  df$label<-df$interaction

  my_scale <- function(x) scales::rescale(x, to = c(minimum, maximum))
  print(paste("Minimum and maxima of data are as",min(df$time),"vs",max(df$time),"while for the entire dataset it is",min(data$time),"and",max(data$time)))
  fig<-df |> group_by(interaction) |> e_charts(x) |> 
    e_scatter_3d(y,z,time,label)|>
    e_tooltip() |>
    e_visual_map(time,type = "continuous",inRange = list(symbol = "diamond",symbolSize = c(45,8), colorLightness = c(0.6,0.35)),scale = my_scale,dimension = 3,height = 100) |>
    e_legend(show = TRUE) |>
    e_title(paste("Infection Plot | Zone",separate_zone,sep = " "), "CIC Model")|>
    e_theme_custom("MyEChartsTheme2.json")
  htmlwidgets::saveWidget(fig, paste("Eanimation",separate_zone,".html",sep = "_"), selfcontained = TRUE)
  
  
  
  

  rm(fig)
}

print("First section generation complete!")

S_ <- data %>%
  group_by(interaction,time,zone) %>%
  summarise(count = n()) %>%
  ungroup()
data <- S_ %>%
  mutate(dates = time) %>%
  select( -time) %>%
  rename(groups = interaction, values = count)

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
filled_data <- filled_data[order(filled_data$groups, filled_data$dates), ]

start_date <- as.Date("2023-12-27")
e1<-filled_data %>%   mutate(
  dates = as.POSIXct(paste(start_date, dates), format = "%Y-%m-%d %H")
) |>group_by(groups)|>
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
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))),type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")


#River

e2<-filled_data %>%   mutate(
  dates = as.POSIXct(paste(start_date, dates), format = "%Y-%m-%d %H")
) |>group_by(groups)|>
  e_charts(dates) |>
  e_river(values,
          emphasis = list(
            focus = "self"
          )) %>% 
  e_theme("westeros")|>
  e_tooltip(trigger = "axis") %>% 
  e_datazoom(
    type = "slider",
    toolbox = TRUE,
    bottom = 40,
    height = 10
  )|>
  e_legend(right = 25,top = 10,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))),type = "scroll",width = 600,orient = "horizontal")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")




#Morph
cb<-"() => {
  let x = 0;
  document.getElementById('toggle')
  .addEventListener('click',(e) => {
    x++
    chart.setOption(opts[x % 2], true);
  });
}"

e_morph(e1,e2,callback = cb) %>% 
  htmlwidgets::prependContent(
    htmltools::tags$button("Toggle",id = "toggle")
  )


#Bind??
# 
# filled_data %>%   mutate(
#   dates = as.POSIXct(paste(start_date, dates), format = "%Y-%m-%d %H")
# ) |>group_by(groups)|>
#   e_charts(dates) |>
#   e_area(values,bind = dates,
#          emphasis = list(
#            focus = "self"
#          )) |> 
#   e_y_axis(min = 0)|>
#   e_tooltip() %>% 
#   e_data(filled_data,values) %>% 
#   e_river(values,bind = dates,
#           emphasis = list(
#             focus = "self"
#           )) %>% 
#   e_theme("westeros") %>% 
#   e_grid(right = 40, top = 100, width = "30%") 
# 

library(echarty)
setting <- list(show = T,type= "scroll",orient= "horizontal", pageButtonPosition= 'start',
                right= "30%",top = 30,width = 470, icon = 'circle', align= 'left', height='85%')
#Below affects the ordering of the categories in the x-axis (including dates)
tmp <- filled_data |> group_by(zone,dates) |> summarize(ss= n()) |>
  ungroup() |> inner_join(filled_data) |>  arrange(dates) |> group_by(zone) |> group_split()
# fine-tune legends: data by interactions (called groups in this dataset)
cns <- lapply(seq_along(tmp), \(i) { as.list(unique(tmp[[i]]$groups)) })
xax <- lapply(seq_along(tmp), \(i) { as.list(unique(tmp[[i]]$dates)) })

subset(filled_data,is.na(zone) ==  FALSE) %>% 
  mutate(dates = as.factor(dates)) %>% 
  group_by(zone) |> 
  ec.init(
    xAxis = list(name = 'Interaction',nameLocation = 'end',max = max(filled_data$dates),
                 nameTextStyle = list(fontWeight ='bolder'),
                 axisLabel = list(rotate = 346,width = 65,
                                  overflow = 'truncate')),
    yAxis = list(name = "Count",nameLocation = 'start',
                 nameTextStyle = list(fontWeight ='bolder')),
    dataZoom= list(type= 'slider',orient = 'vertical'
                   ,left = '2%'),
    tl.series = list(type  ='bar',stack = "grp",
                     encode = list(x = 'dates',y = 'values'), groupBy= 'groups',
                     emphasis= list(focus= 'series',
                                    itemStyle=list(shadowBlur=10,
                                                   shadowColor='rgba(0,0,0,0.5)'),
                                    label= list(position= 'right',
                                                rotate = 350,
                                                show=TRUE)),
                     title = list(list(left = "80%",top = "1%"),
                                  list(text = "Outbreak incidents by year", 
                                  left = "10%", top = 10, textStyle = list(fontWeight = "normal", fontSize = 20),
                                  text = "WAHIS Dataset", 
                                  left = "10%", top = 17, textStyle = list(fontWeight = "normal", fontSize = 14))) ),
    tooltip = list(show = T))|>
  ec.upd({
    options <- lapply(options, \(oo) {
      dix <- oo$series[[5]]$datasetIndex  # from tl.series (bar)
      oo$series <- append(oo$series, 
                          list(
                            list(type='pie', name='pop.',
                                 datasetIndex= dix,
                                 encode= list(value='values', itemName='groups'), 
                                 center= c('15%', '25%'), radius= '14%', 
                                 label= list(show=T), labelLine= list(length=5, length2=0))
                          ))
      oo
    })
  }) %>% 
  ec.upd({legend<-setting
  options <- lapply(seq_along(options), \(i) {  
    options[[i]]$legend$data <- cns[[i]]  # fine-tune legends: data by interactions
    options[[i]]$xAxis$data <- xax[[i]] 
    options[[i]] 
  })
  }) %>% 
  ec.theme("something",westeros)


