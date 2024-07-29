
#! /usr/bin/Rscript
library(pandoc)
library(echarty)
library(echarts4r)
library(dplyr)
# setwd()
# setwd("E:/Outbreak_CIC_Model/")
filled_data<-read.csv("ExampleToShow.csv")
filled_data <- na.omit(filled_data)   # clear all zone==NA
colors <- c('#516b91','#93b7e3','#edafda','#93b7e3','#a5e7f0','#cbb0e3','#3F4756','#009D93','#0095FA','#7D6643','#6363A4','#00498B','#EDAB98')
lsetting <- list(type= "scroll",orient= "horizontal",
                 right= "30%",left = "50%",top= 30, icon= 'circle')
plist <- list(type='pie', name='group',
              center= c('77%', '30%'), radius= '24%', 
              label= list(show=T), labelLine= list(length=6, length2=0))

opts <- list()
zones <- sort(unique(filled_data$zone))

for(zz in zones) {
  df <- filled_data |> filter(zone==zz)
  xax <- sort(unique(df$dates))
  udg <- unique(df$groups)
  lsetting$data <- lapply(seq_along(udg), \(i) {
    list(name= udg[[i]], itemStyle= list(color=colors[[i]])) })
  pied <- df |> group_by(groups) |> summarize(value= sum(values)) |> rename(name= groups)
  plist$data <- ec.data(pied, 'names')
  ic <- 0
  oo <- ec.init( 
    xAxis = list(name= 'Interaction', data= xax,
                 nameTextStyle= list(fontWeight ='bolder'), nameLocation= 'end',
                 axisLabel= list(rotate= -15, width= 65) ),
    yAxis = list(name= "Count", nameLocation= 'start', nameTextStyle= list(fontWeight='bolder')),
    series= lapply(df |> group_by(groups) |> group_split(), \(dfg) {
      ic <<- ic+1
      dd <- lapply(1:nrow(dfg), \(r) { list(
        value= c(as.character(dfg[r,'dates']), unname(unlist(dfg[r,'values']))),
        itemStyle= list(color= colors[[ic]]) )
      })          
      list(type='bar', stack='grp', data= dd, name= unique(dfg$groups))
    }),
    emphasis= list(focus= 'series',
                   itemStyle=list(shadowBlur=10,
                                  shadowColor='rgba(0,0,0,0.5)'),
                   label= list(position= 'right', rotate = 350, show=TRUE)),    
    legend= lsetting,
    dataZoom= list(type= 'slider', orient= 'vertical', left= '2%'),
    title= list(text = "Infection pathway analytics", 
                left= "10%", top= 10, textStyle= list(fontSize = 20)),
    tooltip= list(show= T) ) |>
    ec.upd({
      series <- append(series, list(plist))
    })
  
  opts <- append(opts, list(oo$x$opts))
}

ec.init(preset=F, options= opts, color= colors,
        timeline= list(axisType='category', data=zones, replaceMerge='series') 
)
# htmlwidgets::saveWidget(out, "Diditwork.html", selfcontained = TRUE)



filled_data <- read.csv("ExampleToShow.csv")
filled_data <- na.omit(filled_data)   # clear all zone==NA
colors <- c('#516b91','#93b7e3','#edafda','#93b7e3','#a5e7f0','#cbb0e3','#3F4756','#009D93','#0095FA','#7D6643','#6363A4','#00498B','#EDAB98')
max_y<-max((filled_data%>% group_by(zone,dates) %>% summarize(no = sum(values)))$no)
udg <- unique(filled_data$groups)
lsetting <- list(type= "scroll",orient= "horizontal",
                 right= "30%",left = "15%",top= 30, icon= 'circle')
pie <- list(type='pie', name='group',
            center= c('77%', '30%'), radius= '24%', 
            label= list(show=T), labelLine= list(length=6, length2=0))
df <- filled_data |> count(zone, groups, dates) |> 
  mutate(colr= colors[match(groups, udg)])
zz <- unique(df$zone)
iz <- 1
df |> group_by(zone) |> ec.init( color=colors,
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
                                                    encode= list(x='dates', y='n'), groupBy= 'groups' )
)  |> 
  ec.upd({
    options <- lapply(options, \(oo) {  # by zone
      # serie name has been preset to 'groups' column
      oo$series <- lapply(oo$series, \(ss) {
        ss$itemStyle= list(color= colors[match(ss$name, udg)])
        ss
      })
      tmp <- df |> filter(zone==zz[iz]) |> group_by(groups,colr) |> 
        summarize(value= sum(n)) |> rename(name= groups)
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
