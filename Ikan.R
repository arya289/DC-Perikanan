#Global.R
#------------------
#Library Needed

#Data Preparation
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
#Shiny Dashboard
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
#Map
library(leaflet)
library(mapview)
#Graph
library(rCharts)
library(highcharter)
library(forecast)


#Open File with Multiple Sheet
a <- loadWorkbook("Ikan.xlsx")
sheetNames <- sheets(a)
for(i in 1:length(sheetNames))
{
  assign(sheetNames[i],readWorkbook(a,sheet = i))
}

#Data for Scatter Chart  
pembudidayaikannasionalx <- pembudidayaikannasional %>%
  group_by(NamaProvinsi) %>%
  summarize(mean_Jumlah_Pembudidaya = mean(Jumlah), mean_Produksi_Budidaya = mean(Produksi))
pembudidayaikannasionalx <- merge(pembudidayaikannasionalx, daftarnamadaerah, by = 'NamaProvinsi')

#Data for Map Produksi  
produksibudidayanasionalx <- produksibudidayanasional %>%
  group_by(NamaProvinsi) %>%
  summarize(mean_Volume_Budidaya = mean(Volume), mean_Nilai_Budidaya = mean(Nilai))
produksibudidayanasionalx <- merge(produksibudidayanasionalx, daftarnamadaerah, by = 'NamaProvinsi')

produksibudidayanasionaly <- produksibudidayanasional %>%
  group_by(Tahun) %>%
  summarize(mean_Volume_Budidaya = mean(Volume)) 

produksibudidayanasionalz <- produksibudidayanasional %>%
  group_by(NamaProvinsi) %>%
  summarize(mean_Volume_Budidaya = mean(Volume), mean_Nilai_Budidaya = mean(Nilai))

#Data for Produk Olahan Ikan
produkolahanikanx <- produkolahanikan %>%
  group_by(NamaProvinsi) %>%
  summarize(mean_Volume_Olahan = mean(Volume))
produksibudidayanasionalv <- merge(produksibudidayanasionalx, produkolahanikanx, by = 'NamaProvinsi')

#Data for Perlakuan
produksiperlakuanikannasional <- merge(produksiperlakuanikannasional, daftarnamadaerah, by = 'NamaProvinsi')

#Coloring
color_pal <- colorNumeric(palette = c("red", "orange", "yellow3","green3"), domain = pembudidayaikannasionalx$mean_Produksi_Budidaya, reverse = F)
color_pal1 <- colorNumeric(palette = c("red", "orange", "yellow3","green3"), domain = produksibudidayanasionalx$mean_Volume_Budidaya, reverse = F)

#Server.R
#--------------------
server <- function(input, output, session) {
  
#Common Usage
  click_marker <- eventReactive(input$map_marker_click, {
    
    x <- input$map_marker_click
    return(x$id)
  })
  
###--Tab Konsumsi Ikan--###
#Penyediaan Ikan untuk Konsumsi Secara Total Chart
  a11 <- angkakonsumsiikan %>%
        filter(ParamKonsumsiIkan == "Penyediaan ikan untuk konsumsi (total)")
  
  output$a1 <- renderHighchart({
               hchart(a11, "column", hcaes(x = Tahun, y = Nilai, group = ParamKonsumsiIkan))
  })  

#Penyediaan vs Konsumsi Ikan per Kapita Chart
  a21 <- angkakonsumsiikan[!angkakonsumsiikan$ParamKonsumsiIkan=="Penyediaan ikan untuk konsumsi (total)",]
  a21 <- a21[!a21$ParamKonsumsiIkan=="Ketersediaan Kalori per kapita dari ikan",]
  a21 <- a21[!a21$ParamKonsumsiIkan=="Ketersediaan Protein per kapita dari ikan",]
  a21 <- a21[!a21$ParamKonsumsiIkan=="Ketersediaan Lemak per kapita dari ikan",]
  
  output$a2 <- renderHighchart({
    hchart(a21, "column", hcaes(x = Tahun, y = Nilai, group = ParamKonsumsiIkan))
  })  
  

#Ketersediaan Kalori per Kapita dari Ikan Chart
  a31 <- angkakonsumsiikan %>%
    filter(ParamKonsumsiIkan == "Ketersediaan Kalori per kapita dari ikan")
  
  output$a3 <- renderHighchart({
    hchart(a31, "column", hcaes(x = Tahun, y = Nilai, group = ParamKonsumsiIkan))
  })      

#Ketersediaan per Kapita dari Ikan Chart
  a41 <- angkakonsumsiikan[!angkakonsumsiikan$ParamKonsumsiIkan=="Penyediaan ikan untuk konsumsi (total)",]
  a41 <- a41[!a41$ParamKonsumsiIkan=="Penyediaan ikan untuk konsumsi per kapita",]
  a41 <- a41[!a41$ParamKonsumsiIkan=="Konsumsi ikan per kapita",]
  a41 <- a41[!a41$ParamKonsumsiIkan=="Ketersediaan Kalori per kapita dari ikan",]
  
  output$a4 <- renderHighchart({
    hchart(a41, "column", hcaes(x = Tahun, y = Nilai, group = ParamKonsumsiIkan))
  })   


###--Tab Produksi Benih--###    
#Ketersediaan Kalori per Kapita dari Ikan Chart
  output$b1 <- renderHighchart({
    hchart(jumlahproduksibenih, "column", hcaes(x = Tahun, y = Jumlah, group = Benih)) %>%
    hc_yAxis(min = 0)  
  })
  
#Ketersediaan Kalori per Kapita dari Ikan Chart
  output$b2 <- renderHighchart({
    hchart(jumlahproduksibenih, "column", hcaes(x = Tahun, y = Jumlah, group = Budidaya)) %>%
    hc_yAxis(min = 0)
  })   

  
###--Pembudidaya Ikan--### 



  
#Plot Map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = pembudidayaikannasionalx,
        lng = ~longitude, lat = ~latitude,
        radius = ~mean_Jumlah_Pembudidaya/30000,
        color = ~color_pal(mean_Produksi_Budidaya),
        stroke = T, fillOpacity = 0.8,
        label=~as.character(NamaProvinsi), 
        layerId=~as.character(NamaProvinsi),
        popup = popupTable(pembudidayaikannasionalx))
    
  })

#Reactive Value Click Marker    
  data_for_chart <- reactive({
    pembudidayaikannasional2 <- pembudidayaikannasional[pembudidayaikannasional$NamaProvinsi == click_marker(),]
    return(pembudidayaikannasional2)
  })  
  
  
#Pembudidaya vs Volume Chart
  output$c1 <- renderHighchart({
               hchart(pembudidayaikannasionalx, "scatter", hcaes(x = mean_Jumlah_Pembudidaya, y = mean_Produksi_Budidaya, group = NamaProvinsi))
  })
  
#Jumlah Pertumbuhan Pembudidaya Chart
  output$c2 <- renderHighchart({
    pembudidayaikannasional2 <- pembudidayaikannasional[pembudidayaikannasional$NamaProvinsi == click_marker(),]
    hchart(pembudidayaikannasional2, "column", hcaes(x = Tahun, y = Jumlah, group = NamaProvinsi))
  })
  
#Jumlah Pertumbuhan Produksi Budidaya Chart
  output$c3 <- renderHighchart({
    pembudidayaikannasional2 <- pembudidayaikannasional[pembudidayaikannasional$NamaProvinsi == click_marker(),]
    hchart(pembudidayaikannasional2, "column", hcaes(x = Tahun, y = Produksi, group = NamaProvinsi))
  })
  

###--Produksi Budidaya Ikan Nasional--###   

#Plot Map
output$map1 <- renderLeaflet({
  
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = produksibudidayanasionalx,
      lng = ~longitude, lat = ~latitude,
      radius = ~mean_Nilai_Budidaya/100000000,
      color = ~color_pal1(mean_Volume_Budidaya),
      stroke = T, fillOpacity = 0.8,
      label=~as.character(NamaProvinsi), 
      layerId=~as.character(NamaProvinsi),
      popup = popupTable(produksibudidayanasionalx))
  
})  
        

#Reactive Value Click Marker    
data_for_chart1 <- reactive({
  produksiperlakuanikannasional2 <- produksiperlakuanikannasional[produksiperlakuanikannasional$NamaProvinsi == click_marker(),]
  return(produksiperlakuanikannasional2)
})  

#Komoditas
output$d1 <- renderHighchart({
  hchart(produksibudidayanasional, "column", hcaes(x = Tahun, y = Volume, group = NamaIkan)) %>%
    hc_yAxis(min = 0)  
})


#Tabel Produksi
output$d2 = DT::renderDataTable({
                  produksibudidayanasionalz})


#Tren Budidaya Ikan
output$d3 <- renderHighchart({
    hchart(produksibudidayanasionaly, "line", hcaes(x = Tahun, y = mean_Volume_Budidaya))
})

#Pembudidaya vs Volume Chart
output$d4 <- renderHighchart({
    hchart(produksibudidayanasionalx, "scatter", hcaes(x = mean_Volume_Budidaya, y = mean_Nilai_Budidaya, group = NamaProvinsi))
})


#Produksi vs Hasil Olahan
output$d5 <- renderHighchart({
  hchart(produksibudidayanasionalv, "scatter", hcaes(x = mean_Volume_Budidaya, y = mean_Volume_Olahan, group = NamaProvinsi))
})


#Perlakuan
output$d6 <- renderHighchart({
  produksiperlakuanikannasional2 <- produksiperlakuanikannasional[produksiperlakuanikannasional$NamaProvinsi == click_marker(),]
  hchart(produksiperlakuanikannasional, "column", hcaes(x = Tahun, y = Volume, group = JenisPerlakuan)) %>%
    hc_yAxis(min = 0)  
})





#closing server parantheses
}



#ui.R
#--------------------
ui <- fluidPage(
  useShinyjs(),
  navbarPage(title = "eFishery Data Analytics Dashboard",
             
             # Pick a bootstrap theme from https://rstudio.github.io/shinythemes/
             theme = shinytheme("flatly"),
             
             # Angka Konsumsi Ikan -------------------------------------------------- 
             tabPanel("Konsumsi Ikan", 
                      fluidRow(
                        
                        column(12,
                               fluidRow(
                                         column(6, 
                                                h4("Penyediaan Ikan untuk Konsumsi Secara Total (1000 Ton)", align="left"),
                                                box(
                                                    width = 12,
                                                    height = "400px",
                                                    highchartOutput('a1')
                                                    )
                                                 ),
                                         column(6, 
                                                h4("Penyediaan vs Konsumsi Ikan per Kapita (Kg/Kapita/Tahun)", align="left"),
                                                box(
                                                    width = 12,
                                                    height = "400px",
                                                    highchartOutput('a2')
                                                    )
                                                 )
                                         ),
                               fluidRow(
                                        column(6, 
                                              h4("Ketersediaan Kalori per Kapita dari Ikan (Kkal/kapita/hari)", align="left"),
                                              box(
                                                  width = 12,
                                                  height = "400px",
                                                  highchartOutput('a3')
                                                   )
                                               ),
                                        column(6, 
                                              h4("Ketersediaan per Kapita dari Ikan (gram/kapita/hari)", align="left"),
                                              box(
                                                  width = 12,
                                                  height = "400px",
                                                  highchartOutput('a4')
                                                  )
                                              )
                                        )
                              
                              )
                              )
                      ),         
             
             tabPanel("Produksi Benih", 
                      fluidRow(
                               column(12,
                                         h3("Jumlah Produksi Benih by Jenis Benih", align="left"),
                                         highchartOutput('b1')
                                         )
                               ),
                      
                      fluidRow(
                               column(12, 
                                         h3("Jumlah Produksi Benih by Jenis Budidaya", align="left"),
                                         highchartOutput('b2')
                                         )
                                        )
                                      
                                      ),
             
             tabPanel("Pembudidaya Ikan",
                      fluidRow(
                        column(12, 
                               fluidRow(
                                 h3("Peta Pembudidaya Ikan Nasional", align="left")),
                               fluidRow(
                                 box(
                                   width = 12,
                                   leafletOutput("map"))
                                        ),
                               fluidRow(
                                 column(6, 
                                        h4("Korelasi Rataan Pembudidaya (Orang) vs Rataan Produksi Budidaya (Ton)", align="left"),
                                        box(
                                          width = 12,
                                          height = "400px",
                                          highchartOutput('c1')
                                        )
                                 ),
                                 column(6, 
                                        h4("Pertumbuhan per Provinsi", align="left"),
                                        tabBox(
                                          width = 12,
                                          id = "tabset1",
                                          height = "400px",
                                          tabPanel("Jumlah Pembudidaya (Orang)", highchartOutput('c2')),
                                          tabPanel("Jumlah Produksi (Ton)", highchartOutput('c3'))
                                        )
                                 )
                               )
                              )
                              )
                       ),
             
             tabPanel("Produksi Budidaya Ikan",
                      fluidRow(
                        column(12, 
                               fluidRow(
                                 h3("Peta Produksi Ikan Nasional", align="left")),
                               fluidRow(
                                 box(
                                   width = 12,
                                   leafletOutput("map1"))
                               ),
                               fluidRow(
                                        column(12, 
                                                  h3("Visual Analysis", align="left"),
                                                  tabBox(
                                                    width = 12,
                                                    id = "tabset1",
                                                    height = "500px",
                                                    tabPanel("Komoditas", highchartOutput('d1')),
                                                    tabPanel("Tabel Produksi", DT::dataTableOutput("d2")),
                                                    tabPanel("Tren Budidaya Ikan", highchartOutput('d3')),
                                                    tabPanel("Volume (Ton) vs Nilai (Rp)", highchartOutput('d4')),
                                                    tabPanel("Produksi (Ton) vs Olahan (Ton)", highchartOutput('d5')),
                                                    tabPanel("Perlakuan Produksi", highchartOutput('d6'))
                                        )
                                 )
                               )
                        )
                      )
             )
             
#closing navbar parantheses               
  )
  
  #closing ui parantheses
)             

                               
shinyApp(ui = ui, server = server)             
             
             
             
             
             
             
             
             


