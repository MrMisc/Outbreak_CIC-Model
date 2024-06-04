
#! /usr/bin/Rscript

f<-file("stdin")
open(f)
record<-c()
while(length(line<-readLines(f,n=1))>0){
  #write(line,stderr())
  record<-c(record,line)  
}

somethingSomethingplotly<-FALSE

numbers<-record[1:length(record)-1]
numbers_<-c()


print(getwd())
# Set the CRAN mirror
cran_mirror <- "https://cran.r-project.org"  # Replace with a mirror of your choice if needed
options(repos = cran_mirror)

# Check if the package is installed
if (!requireNamespace("echarty", quietly = TRUE)) {
  # If not installed, attempt to install it
  install.packages("echarty")
}


# Plot heatmap
library(ggplot2)
library(pandoc)
library(plotly)
library(echarts4r)
library(echarty)
library(echarts4r.assets)
# library(echarty)
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
coordinates <- strsplit(record, " ")
print("Beginning read...")
# Extract x, y, and time values
x <- as.numeric(sapply(coordinates, "[[", 1))
y <- as.numeric(sapply(coordinates, "[[", 2))
altitude <- as.numeric(sapply(coordinates, "[[", 3))
interaction <- as.numeric(sapply(coordinates, "[[", 4))
time <- as.numeric(sapply(coordinates, "[[", 5))
zone<- as.numeric(sapply(coordinates, "[[", 6))
print("Zone check")
print(unique(zone))
# print(zone[length(zone)-5:length(zone)])
print("Finished read")
# print(z)
transfer_distance<-0.7 ##Manually place this information


# step_x<-x[length(x)-1]
# step_y<-y[length(y)-1]
# step_z<-altitude[length(altitude)-1]

# x_large<-x[length(x)]
# y_large<-y[length(y)]
# z_large<-altitude[length(altitude)]

scaling_factor<-6

# x<-x[-c(length(x)-1,length(x))]
# y<-y[-c(length(y)-1,length(y))]
# altitude<-altitude[-c(length(altitude)-1,length(altitude))]
# interaction<-interaction[-c(length(interaction)-1,length(interaction))]
# time<-time[-c(length(time)-1,length(time))]
# zone<-zone[-c(length(zone)-1,length(zone))]
# Create a data frame for each unique zone value
zone_unique <- unique(zone)
# data_frames <- list()

# for (z in zone_unique) {
#   data_frames[[as.character(z)]] <- data.frame(x = x[zone == z], y = y[zone == z])
# }


# custom_colors <- c("#FFE7D3", "#FFBEC3", "#FF878E", "#C4515C")

# print(unique(interaction))
# Create the ggplot2 plot with geom_tile and frame aesthetic




print("Zone unique is ..")
print(zone_unique)
N<-length(unique(zone_unique))/2

print("Zone check")
print(unique(zone))
count<-1

step_x<-rep(1,N)
step_y<-rep(1,N)
step_z<-rep(1,N)

x_large<-rep(1,N)
y_large<-rep(1,N)
z_large<-rep(1,N)


for (separate_zone in 1:N){
  step_x[separate_zone]<-x[length(x)-(N-separate_zone)]-100000
  step_y[separate_zone]<-y[length(y) - (N-separate_zone)]-100000
  step_z[separate_zone]<-altitude[length(altitude) - (N-separate_zone)]-100000
  #interaction, time, zone
  x_large[separate_zone]<-interaction[length(interaction) - (N-separate_zone)]-100000
  y_large[separate_zone]<-time[length(time) - (N-separate_zone)]-100000
  z_large[separate_zone]<-zone[length(zone) - (N-separate_zone)]-100000

  print("Step X coord extract is..")
  print(x[length(x)-(N-separate_zone)])
}






print("Step parameters are ...")
print(step_x)
print(step_y)
print(step_z)

print("Max param sizes are...")
print(x_large)
print(y_large)
print(z_large)

print("Number of elements that we are removing is")
print(N)
# print("Before removing elements, zone info is...")
# print(zone)

x<-x[-c((length(x) - N+1):length(x))]
y<-y[-c((length(y) - N+1):length(y))]
altitude<-altitude[-c((length(altitude) - N+1):length(altitude))]
interaction<-interaction[-c((length(interaction) - N+1):length(interaction))]
time<-time[-c((length(time) - N+1):length(time))]
zone<-zone[-c((length(zone) - N+1):length(zone))]

# print(zone)


data <- data.frame(x = x, y = y, z = altitude,interaction = factor(interaction), time = time, zone = zone)
zone_unique2 <- unique(zone)
print("ZONES being used...")
print(zone_unique2)
minimum<-min(data$time)
maximum<-max(data$time)
for (separate_zone in zone_unique2){
  # print(typeof(separate_zone))
  data_<-subset(data, zone == separate_zone)
  
  data_ <- data_ %>% 
      mutate(interaction = case_when(
        interaction == 1000 ~ "marker",
          interaction == 0 ~ "Host 0[*]",
          interaction == 1 ~ "Host <-> Host[*]",
          interaction == -2 ~ "Host -> Egg[*]",
          interaction == -3 ~ "Host -> Faeces[*]",
          interaction == 4 ~ "Egg <-> Egg[*]",
          interaction == -6 ~ "Egg -> Faeces[*]",
          interaction == 9 ~ "Faeces <-> Faeces[*]",
          interaction == 2 ~ "Egg -> Host[*]",
          interaction == 3 ~ "Faeces -> Host[*]",
          interaction == 6 ~ "Faeces -> Egg[*]",
          interaction == 10~ "Feed infection",
          interaction == 11~ "Eviscerator -> Host",
          interaction == 12~ "Host -> Eviscerator",
          interaction == 13~ "Eviscerator Mishap/Explosion",
          interaction == 100 ~ "Host 0",
          interaction == 101 ~ "Host <-> Host",
          interaction == -102 ~ "Host -> Egg",
          interaction == -103 ~ "Host -> Faeces",
          interaction == 104 ~ "Egg <-> Egg",
          interaction == -106 ~ "Egg -> Faeces",
          interaction == 109 ~ "Faeces <-> Faeces",
          interaction == 102 ~ "Egg -> Host",
          interaction == 103 ~ "Faeces -> Host",
          interaction == 106 ~ "Faeces -> Egg",          
          TRUE ~ as.character(interaction)
      ))

  custom_marker_symbols <- list(
    "Host <-> Host" = "circle",
    "Host <-> Egg" = "star",
    "Host <-> Faeces" = "hexagram",
    "Egg <-> Egg" =  "x",
    "Egg <-> Faeces" = "star-x",
    "Faeces <-> Faeces" = "asterisk"
    # Add more symbols for other unique 'interaction' values if needed
  )


  # fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, symbol = ~interaction, color = ~time, mode = "markers")
  print("Maximum data time is")
  print(max(data_$time))
  print("Minimum data time is")
  print(min(data_$time))
  if (somethingSomethingplotly){
    fig <- plot_ly(data_, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", symbol = ~interaction, text = ~paste(
                            '<br>Time:', time, 'hours ','<br> Infection Event Type:',interaction),
                  marker = list(
                    color = ~time,
                    cmin = min(data_$time),
                    cmax = max(data_$time),                  
                    size = transfer_distance*scaling_factor,
                    opacity = 0.9,
                    colorscale=list(c(0, 1), c("#C34A36", "#2F4858")),                 
                    colorbar = list(
                    title = 'Time',
                    x = 0,
                    y = 0.5,
                    thickness = 5,
                    dtick = 12,
                    tick0 = 0
                  ),
                    showscale = TRUE
                  ))
    print("Using x coord as follows")
    print("Max x")
    print(x_large[count])
    print("Out from ")
    print(x_large)
    print("Step x")
    print(step_x[count])
    print("Out from")
    print(step_x)
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'X-Axis',dtick = step_x[count],range = list(0,x_large[count])),
                                    yaxis = list(title = 'Y-Axis',dtick = step_y[count],range = list(0,y_large[count])),
                                    zaxis = list(title = 'Z-Axis',dtick = step_z[count],range = list(0,z_large[count])),
                                    aspectmode = "manual",aspectratio = list(x = x_large[count]/f,y = y_large[count]/f,z = z_large[count]/f)))

    fig<-fig%>%
    animation_opts(mode = "next",
                  easing = "elastic-in", redraw = FALSE
    )

    htmlwidgets::saveWidget(as_widget(fig), paste("animation",separate_zone,".html",sep = "_"), selfcontained = TRUE)
    rm(fig)

    fig <- plot_ly(data_, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", frame = ~time, symbol=~interaction,text = ~paste(
                            '<br>Time:', time, 'hours ','<br> Infection Event Type:',interaction, '<br> Zone: ',zone),
                  marker = list(
                    color = "#15798C",
                    size = transfer_distance*scaling_factor,
                    opacity = 0.9,
                    colorscale = 'Inferno'
                  ))

    # Apply the custom color scale to the plot
    # fig$x$data[[1]]$marker$colorscale <- custom_color_scale
    # fig <- fig %>% add_markers()
    # fig <- fig %>% layout(scene = list(xaxis = list(title = 'X-Axis',dtick = step_x[separate_zone],range = list(0,x_large[separate_zone])),
    #                                 yaxis = list(title = 'Y-Axis',dtick = step_y[separate_zone],range = list(0,y_large[separate_zone])),
    #                                 zaxis = list(title = 'Z-Axis',dtick = step_z[separate_zone],range = list(0,z_large[separate_zone])),
    #                                 aspectmode = "manual",aspectratio = list(x = x_large[separate_zone],y = y_large[separate_zone],z = z_large[separate_zone])))
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'X-Axis',dtick = step_x[count],range = list(0,x_large[count])),
                                    yaxis = list(title = 'Y-Axis',dtick = step_y[count],range = list(0,y_large[count])),
                                    zaxis = list(title = 'Z-Axis',dtick = step_z[count],range = list(0,z_large[count])),
                                    aspectmode = "manual",aspectratio = list(x = x_large[count]/f,y = y_large[count]/f,z = z_large[count]/f)))  

    fig<-fig%>%
    animation_opts(frame = 300,transition = 150,mode = "next",
                  easing = "elastic-in", redraw = TRUE
    )

    htmlwidgets::saveWidget(as_widget(fig), paste("animation",separate_zone,"time_series",".html",sep = "_"), selfcontained = TRUE)  


  }


  # Apply the custom color scale to the plot
  # fig$x$data[[1]]$marker$colorscale <- custom_color_scale
  # fig <- fig %>% add_markers()
  #Factor
  f<-max(x_large[count],y_large[count],z_large[count])/200
  f_<-0.3*f

  df<-data_
  df$label<-df$interaction
  # df$Time<-df$time
  my_scale <- function(x) scales::rescale(x, to = c(minimum, maximum))
  print(paste("Minimum and maxima of data are as",min(df$time),"vs",max(df$time),"while for the entire dataset it is",min(data$time),"and",max(data$time)))
  fig<-df |> group_by(interaction) |> e_charts(x) |> 
    e_scatter_3d(y,z,time,label)|>
    e_tooltip() |>
    e_visual_map(time,type = "continuous",inRange = list(symbol = "diamond",symbolSize = c(45,8), colorLightness = c(0.6,0.35)),scale = my_scale,dimension = 3,height = 100) |>
    e_x_axis_3d(min = 0,max = x_large[count],interval = step_x[count])|>
    e_y_axis_3d(min = 0,max = y_large[count],interval = step_y[count])|>
    e_z_axis_3d(min = 0,max = z_large[count],interval = step_z[count], name = "Z / Altitude")|>
    e_grid_3d(boxWidth = x_large[count]/f_,boxHeight = z_large[count]/f_,boxDepth = y_large[count]/f_)|>
    e_legend(show = TRUE) |>
    e_title(paste("Infection Plot | Zone",separate_zone,sep = " "), "CIC Model | by Irshad Ul Ala")|>
    e_theme_custom("MyEChartsTheme2.json")
  htmlwidgets::saveWidget(fig, paste("Eanimation",separate_zone,".html",sep = "_"), selfcontained = TRUE)




  count<-count+1
  rm(fig)
}

print("First section generation complete!")


if (somethingSomethingplotly){
  fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", frame = ~zone,text = ~paste(
                            '<br>Time:', time, 'hours ','<br> Infection Event Type:',interaction),
                marker = list(
                  color = ~time,
                  size = transfer_distance*scaling_factor,
                  opacity = 0.9,
                  colorscale = 'Inferno',
                  colorbar = list(
                    title = 'Time',
                    x = 0,
                    y = 0.5,
                    thickness = 5,
                    dtick = 12,
                    tick0 = 0
                  )
                ))

  # Apply the custom color scale to the plot
  # fig$x$data[[1]]$marker$colorscale <- custom_color_scale
  # fig <- fig %>% add_markers()
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'X-Axis',dtick = max(step_x),range = list(0,max(x_large))),
                                  yaxis = list(title = 'Y-Axis',dtick = max(step_y),range = list(0,max(y_large))),
                                  zaxis = list(title = 'Z-Axis',dtick = max(step_z),range = list(0,max(z_large))),
                                  aspectmode = 'manual',aspectratio = list(x = x_large,y = y_large,z = z_large)))


  htmlwidgets::saveWidget(as_widget(fig), "animation__byzones.html", selfcontained = TRUE)



}



# summary_data <- data %>%
#   group_by(interaction, time) %>%
#   summarise(count = n()) %>%
thematic::thematic_on(bg = "#fff6eb", fg = "#005B4B", accent = "#005B4B", font = "Yu Gothic")
print(unique(data$interaction))
data <- data%>% 
    mutate(interaction = case_when(
      interaction == 1000 ~ "marker",
        interaction == 0 ~ "Host 0[*]",
        interaction == 1 ~ "Host <-> Host[*]",
        interaction == -2 ~ "Host -> Egg[*]",
        interaction == -3 ~ "Host -> Faeces[*]",
        interaction == 4 ~ "Egg <-> Egg[*]",
        interaction == -6 ~ "Egg -> Faeces[*]",
        interaction == 9 ~ "Faeces <-> Faeces[*]",
        interaction == 2 ~ "Egg -> Host[*]",
        interaction == 3 ~ "Faeces -> Host[*]",
        interaction == 6 ~ "Faeces -> Egg[*]",
        interaction == 10~ "Feed infection",
        interaction == 11~ "Eviscerator -> Host",
        interaction == 12~ "Host -> Eviscerator",
        interaction == 13~ "Eviscerator Mishap/Explosion",
        interaction == 100 ~ "Host 0",
        interaction == 101 ~ "Host <-> Host",
        interaction == -102 ~ "Host -> Egg",
        interaction == -103 ~ "Host -> Faeces",
        interaction == 104 ~ "Egg <-> Egg",
        interaction == -106 ~ "Egg -> Faeces",
        interaction == 109 ~ "Faeces <-> Faeces",
        interaction == 102 ~ "Egg -> Host",
        interaction == 103 ~ "Faeces -> Host",
        interaction == 106 ~ "Faeces -> Egg",          
        TRUE ~ as.character(interaction)
    ))

print("Names in data are")
print(unique(data$interaction))

fig <- ggplot(data, aes(x = time, fill = as.factor(interaction))) +
  geom_bar(position = "dodge") + facet_wrap(.~zone)+
  labs(title = "Bar Plot of Interaction Counts Over Time", x = "Time", y = "Count")

fig <- ggplotly(fig, dynamicTicks = TRUE)

htmlwidgets::saveWidget(fig, "Histogram.html", selfcontained = TRUE)

# write.csv(data,"fuckyouR.csv")
# data<-read.csv("fuckyouR.csv", header = T)
S <- data %>%
  group_by(interaction,zone,time) %>%
  summarise(count = n()) %>%
  ungroup()

S_ <- data %>%
  group_by(interaction,time) %>%
  summarise(count = n()) %>%
  ungroup()

fig <- ggplot(S, aes(y = count,x = time, color = as.factor(interaction))) +
  geom_line() + facet_wrap(.~zone) +
  labs(title = "Interaction Counts Over Time", x = "Time", y = "Count")

fig <- ggplotly(fig, dynamicTicks = TRUE)

htmlwidgets::saveWidget(fig, "Line.html", selfcontained = TRUE)
rm(fig)
# write.csv(S_,"infections_sample.csv", row.names = FALSE)
# write.csv(S,"extra.csv", row.names = FALSE)
# write.csv(data,"full.csv", row.names = FALSE)
saved_data<-data
data <- S_ %>%
  mutate(dates = time) %>%
  select( -time) %>%
  rename(groups = interaction, values = count)

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



# data <- S_%>%
#   mutate(dates = time) %>%
#   select( -time) %>%
#   rename(groups = interaction, values = count)
# 
# data$dates <- as.numeric(data$dates)

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

fig<-e_morph(e1,e2,callback = cb) %>% 
  htmlwidgets::prependContent(
    htmltools::tags$button("Toggle",id = "toggle")
  )
htmlwidgets::saveWidget(fig, "InfectionTypeDistribution.html", selfcontained = TRUE)
rm(fig)
print("What is saved data consisting of?1")
print(names(saved_data))

#Infection granular 
S_ <- saved_data %>%
  group_by(interaction,time,zone) %>%
  summarise(count = n()) %>%
  ungroup()
data <- S_ %>%
  mutate(dates = time) %>%
  select( -time) %>%
  mutate(groups = interaction, values = count)



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

# print("WHAT ARE THE COLUMN NAMES OF FILLED DATA?")
# print(names(filled_data))

time_seq<-seq(min(filled_data$dates),max(filled_data$dates),1)
no_of_zones<-length(unique(filled_data$zone))

add<-data.frame(groups = rep("marker",length(time_seq)*no_of_zones), dates = rep(time_seq,no_of_zones),zone = rep(unique(filled_data$zone),each = length(time_seq)),values = rep(1,length(time_seq)*no_of_zones))
add<-subset(add,is.na(zone) == FALSE)

dd<-rbind(filled_data %>% dplyr::select(-count) %>% dplyr::select(-interaction),add)

dd_copy<-dd
# print("WHAT ARE THE COLUMN NAMES OF DD?")
# print(names(dd))

# dd<-rbind(filled_data,add)
setting <- list(show = T,type= "scroll",orient= "horizontal", pageButtonPosition= 'start',
                right= "30%",top = 30,width = 470, icon = 'circle', align= 'left', height='85%')

tmp <- dd |> group_by(zone,dates) |> summarize(ss= n()) |>
  ungroup() |> inner_join(dd) |>  arrange(dates) |> group_by(zone) |> group_split()
# fine-tune legends: data by interactions (called groups in this dataset)
cns <- lapply(seq_along(tmp), \(i) { as.list(unique(tmp[[i]]$groups)) })
xax <- lapply(seq_along(tmp), \(i) { as.list(unique(tmp[[i]]$dates)) })

source('echarty_themes.R')

gran<-subset(dd,is.na(zone) ==  FALSE) %>% 
  mutate(dates = as.factor(dates)) %>% 
  group_by(zone) |> 
  ec.init(
    xAxis = list(name = 'Hour',nameLocation = 'end',max = max(filled_data$dates),
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
                                  list(text = "Infection pathway analytics", 
                                       left = "10%", top = 10, textStyle = list(fontWeight = "normal", fontSize = 20),
                                       text = "@ Zone", 
                                       left = "10%", top = 17, textStyle = list(fontWeight = "normal", fontSize = 14))) ),
    tooltip = list(show = T))|>
  ec.upd({
    options <- lapply(options, \(oo) {
      dix <- oo$series[[1]]$datasetIndex  # from tl.series (bar)
      # oo$series <- append(oo$series, 
      #                     list(
      #                       list(type='pie', name='pop.',
      #                            datasetIndex= dix,
      #                            encode= list(value='values', itemName='groups'), 
      #                            center= c('15%', '25%'), radius= '11%', 
      #                            label= list(show=T), labelLine= list(length=5, length2=0))
      #                     ))
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
  ec.theme("something",westeros3)

htmlwidgets::saveWidget(gran, "InfectionTypeDistribution_Granular.html", selfcontained = TRUE)


# S_rearranged <- saved_data %>%
#   mutate(dates = time) %>%
#   select( -time) %>%
#   rename(groups = interaction, values = count)
# 
# S_rearranged<-saved_data %>%
#   group_by(interaction,time,zone) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   mutate(dates = time) %>%
#   select( -time) %>%
#   rename(groups = interaction, values = count)
# 
# S_rearranged<-S_
# 
# 
# data<-data_copy

# Convert 'dates' column to numeric (if it's not already numeric)
# data$dates <- as.numeric(data$dates)
# 
# # Create a template dataframe with all unique combinations of groups and dates
# all_combinations <- expand.grid(
#   groups = unique(data$groups),
#   dates = unique(data$dates)
# )
# 
# 
# # Merge the template with the existing data
# filled_data <- merge(all_combinations, data, by = c("groups", "dates"), all = TRUE)
# 
# # print("[DIAG] HERE?0")
# # print(names(filled_data))
# 
# # Replace missing values with 0
# filled_data[is.na(filled_data$values), "values"] <- 0
# 


# print("[DIAG] HERE?")
# print(names(filled_data))

# filled_data <- filled_data %>%

#######################################
#   group_by(zone) %>%
#   complete(dates = seq(min(dates), max(dates), by = 1), fill = list(groups = 'marker', values = 1)) %>%
#   ungroup()
filled_data<-dd_copy
length_per_zone<-length(seq(min(filled_data$dates),max(filled_data$dates),1))
tot_length<-length_per_zone*length(unique(filled_data$zone))

add<-data.frame(groups = rep('marker',tot_length),dates = rep(seq(min(filled_data$dates),max(filled_data$dates),1),length(unique(filled_data$zone))),zone = rep(unique(filled_data$zone),each = length_per_zone),values = rep(1,tot_length))

# print("[DIAG] HERE?2")
# print(names(filled_data))
# print("What about add?")
# print(names(add))

filled_data<-rbind(filled_data,add)
# filled_data<-dd

colors <- c('#516b91','#93b7e3','#edafda','#3D4856','#a5e7f0','#cbb0e3','#3F4756','#009D93','#0095FA','#7D6643','#6363A4','#00498B','#EDAB98')
lsetting <- list(type= "scroll",orient= "horizontal",
                 right= "30%",left = "50%",top= 30, icon= 'circle')
plist <- list(type='pie', name='group',
              center= c('77%', '30%'), radius= '24%', 
              label= list(show=T), labelLine= list(length=6, length2=0))


# print("[DIAG] HERE?")
# rm(options)
filled_data <- na.omit(filled_data) 

print("[DIAG] HERE?3")
print(names(filled_data))

udg <- unique(filled_data$groups)
options <- list()
zones <- sort(na.omit(unique(filled_data$zone)))
pie <- list(type='pie', name='group',
            center= c('77%', '30%'), radius= '24%', 
            label= list(show=T), labelLine= list(length=6, length2=0))
df <- filled_data |> count(zone, groups, dates, values) |> 
  mutate(colr= colors[match(groups, udg)])
zz <- unique(df$zone)
iz <- 1
# print("Got to csv")
# write.csv(filled_data,"ExampleToShow.csv", row.names = FALSE)

print("Original data names for df are")
print(names(df))

#Try thing for granular 2 plot
result<-tryCatch({
  max_y<-max((filled_data%>% group_by(zone,dates) %>% summarize(no = sum(values)))$no)
  thing<-filled_data |> group_by(zone) |> ec.init( color=colors,
                                 yAxis = list(max=max_y),
                                 tooltip= list(s=T), xAxis= list(type='category'),
                                 emphasis= list(focus= 'series',
                                                itemStyle= list(shadowBlur=10, shadowColor='rgba(0,0,0,0.5)'),
                                                label= list(position= 'right', rotate = 350, show=T)),
                                 dataZoom= list(type= 'slider', orient= 'vertical', left= '2%'),
                                 title= list(text = "Infection pathway analytics", 
                                             left= "10%", top= 10, textStyle= list(fontSize = 20)),
                                 timeline= list(axisType= 'category'),
                                 series.param= list(type= 'bar', stack='grp', 
                                                    encode= list(x='dates', y='values'), groupBy= 'groups' )
)  |> 
  ec.upd({
    options <- lapply(options, \(oo) {  # by zone
      # serie name has been preset to 'groups' column
      oo$series <- lapply(oo$series, \(ss) {
        ss$itemStyle= list(color= colors[match(ss$name, udg)])
        ss
      })
      tmp <- df |> filter(zone==zz[iz]) |> group_by(groups,colr) |> 
        summarize(value= sum(values)) |> rename(name= groups)
      tmp <- ec.data(tmp, 'names')   # convert data.frame to list
      tmp <- lapply(tmp, \(rr) {     # change colr to itemStyle
        rr$itemStyle <- list(color= rr$colr); rr$colr <- NULL; rr
      })
      pie$data <- lsetting$data <- tmp    # same colors for legend and pie
      oo$legend <- lsetting
      oo$series <- append(oo$series, list(pie))
      iz <<- iz+1
      oo
    })
  })  
  htmlwidgets::saveWidget(thing, "InfectionTypeDistribution_Granular2.html", selfcontained = TRUE)
}, error = function(e){
  print("was not able to run the additional granular generating html code")
})

#library(pandoc)
#Get dem custom fonts
# font_import()
# loadfonts(device = "win")
# actual_pars<-as.data.frame(actual_pars)

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


#Standardize


hosts<-max((df$`Total Hosts`))
eggs<-max(df$`Eggs Amt`)
faeces<-max(df$`Faeces Amt`)

out<-df %>% mutate(value = round((value),1))%>%
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
                     symbolSize = ec.clmn(sprintf("function(v, pp) {
                     var minVal = 0;       
                     var maxsize = 25;
                     if (['%%Contaminated', '%%Infected', '%%Colonized'].includes(pp.seriesName))
                       return (pp.data[2] - minVal) / (%f - minVal) *maxsize;
                     else if (['%%Eggs Infected'].includes(pp.seriesName))
                       return (pp.data[4] - minVal) / (%f - minVal) * maxsize;
                     else if (['%%Faeces infected'].includes(pp.seriesName))
                       return (pp.data[7] - minVal) / (%f - minVal) * maxsize;
                     else
                       return 90;
                   }",hosts,eggs,faeces)),
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


htmlwidgets::saveWidget(out, "e_scatterplot.html", selfcontained = TRUE)


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
