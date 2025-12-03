library(shiny);library(data.table);library(magrittr);library(leaflet);library(bslib);library(leaflet.extras);library(sf);library(dplyr)
library(DT);library(lubridate);library(ggplot2)
# Set theme for ggplot ----
theme_set(theme(panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.text = element_text(size = 12),
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                legend.position = "bottom",
                plot.title = element_text(hjust = 0.5), 
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                strip.background = element_rect(fill = "white"),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.key = element_rect(fill = "NA",color = "white"),
                legend.background = element_rect(color = "white"),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 90),
                axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))))


# Read in data ----
marin_df <- data.table::fread("data/marin.csv",
                              sep = ";", dec = ",",
                              select = c("StedID","Stedtekst","Medie", "Vandområde","Dato","Prøvetype","Målested, x-koordinat", "Målested, y-koordinat",
                                         "Dybde (m)","Faktiske dybder (m)","Analysefraktion","Stofparameter","Resultat-attribut",
                                         "Resultat","Enhed","Detektionsgrænse LD","Kvantifikationsgrænse LQ","Kvalitetsmærke"))

lake_df <- data.table::fread("data/sø.csv", sep = ";", dec = ",",
                             select = c("StedID","Stedtekst","Vandområde","Dato","Medie","Prøvetype","Dybde (m)","Målested, x-koordinat", 
                                        "Målested, y-koordinat","Faktiske dybder (m)","Analysefraktion","Stofparameter","Resultat-attribut",
                                        "Resultat","Enhed","Detektionsgrænse LD","Kvantifikationsgrænse LQ","Kvalitetsmærke"))

stream_df <- data.table::fread("data/vandløb.csv",sep = ";", dec = ",",
                               select = c("StedID","Stedtekst","Vandområde","Dato","Prøvetype","Målested, x-koordinat","Medie",
                                          "Målested, y-koordinat","Analysefraktion","Stofparameter","Resultat-attribut",
                                          "Resultat","Enhed","Detektionsgrænse LD","Kvantifikationsgrænse LQ","Kvalitetsmærke")) 
unit_changer <- data.table::fread("data/Units.csv")

combined_df <- data.table::rbindlist(list(marin_df,
                                          lake_df,
                                          stream_df),
                                     fill = T) %>% 
  setnames(., c("Målested, x-koordinat","Målested, y-koordinat","Dybde (m)"),c("x","y","Dybde")) %>% 
  .[, c("long","lat","Dybde","Dato") := list(oce::utm2lonlat(x,y, zone = 32)$longitude,
                                             oce::utm2lonlat(x,y, zone = 32)$latitude,
                                             fcase(is.na(Dybde), 0.01,
                                                   rep(TRUE, .N), Dybde),
                                             Dato = dmy_hms(Dato))] %>% 
  .[unit_changer, on = c('Stofparameter', 'Enhed'), 
    ':='(
      Multiplier = fcase(is.na(Multiplier), 1,
                         rep(TRUE, .N), Multiplier),
      Enhed_new  = fcase(is.na(Enhed_new), Enhed,
                         rep(TRUE, .N), Enhed_new),
      Enhed = Enhed_new,
      Resultat = Multiplier*Resultat)] %>% 
  .[, c("Enhed_new","Multiplier","Detektionsgrænse LD","Kvantifikationsgrænse","Kvalitetsmærke") := NULL] %>% 
  setnames(., "Dybde", "Dybde (m)")

combined_df %>% 
  dplyr::distinct(StedID,Stedtekst,Medie,long,lat) -> distinct_locations

distinct_locations %>% 
  filter(Medie == "Marin") -> marin_distinct
distinct_locations %>% 
  filter(Medie == "Sø") -> lake_distinct
distinct_locations %>% 
  filter(Medie == "Vandløb") -> stream_distinct

st_as_sf(distinct_locations, coords = c("lat","long"), crs = st_crs(4326)) -> distinct_locations_sf

## Oxygen df ----

combined_df %>% 
  filter(Stofparameter %in% c("Oxygen indhold","Oxygenmætning")) %>% 
  mutate(uge = week(Dato),
         col = case_when(Stofparameter == "Oxygen indhold" & Resultat < 2 ~ "#A50026",
                         Stofparameter == "Oxygen indhold" & Resultat < 4 ~ "#D73027",
                         Stofparameter == "Oxygen indhold" & Resultat < 6 ~ "#F46D43",
                         Stofparameter == "Oxygen indhold" & Resultat < 8 ~ "#D9EF8B",
                         Stofparameter == "Oxygen indhold" & Resultat < 10 ~ "#66BD63",
                         Stofparameter == "Oxygen indhold" & Resultat > 10 ~ "#006837",
                         
                         Stofparameter == "Oxygenmætning" & Resultat < 20 ~ "#A50026",
                         Stofparameter == "Oxygenmætning" & Resultat < 40 ~ "#D73027",
                         Stofparameter == "Oxygenmætning" & Resultat < 60 ~ "#F46D43",
                         Stofparameter == "Oxygenmætning" & Resultat < 80 ~ "#D9EF8B",
                         Stofparameter == "Oxygenmætning" & Resultat < 100 ~ "#66BD63",
                         Stofparameter == "Oxygenmætning" & Resultat > 100 ~ "#006837",
         )) -> ilt_df


# Define UI for application that draws a histogram
ui <- fluidPage(
  page_navbar(
    id = "inTabset",
    title = "Sø kemiske data",
    bg = "#2D89C8",
    inverse = TRUE,
    nav_panel(title = "Introduktion", 
              HTML("
                   <h1>Velkommen til denne hjemmeside om de kemiske forhold i sø, marint og vandløb</h1>
                   <h2> Inspirationen til denne side </h2>
                   <p>
                   Denne hjemmeside er lavet for at muliggøre alle at følge den kemiske udvikling i de danske søer.
                   I en verden af misinformation og 'alternative'-fortællinger føler jeg at det er nødvendigt at sørge for at tal og undersøgelser om den danske natur er offentligt tilgængelige.
                   Ikke blot i form af tabeller for faglærte at undersøge, men også for lægmanden der brænder for den natur som vi færdes i, og kan følge udviklingen på tæt hold.
                   Det er ofte at passioneret ildsjæle har en meget bedre forståelse for de økologiske nicher som de kender som deres baglomme.
                   Netop fordi de har færdes der i mange timer, på forskellige tidspunkter på året. Har set området gro, vokse, blomstre, forsvinde og gå i forfald. 
                   Jeg håber at jeg kan være med til at styrke samarbejdet mellem de lidenskabelige og videnskabelige ved at lave dette værktøj i form af denne side. 
                   </p>
                   <h2> How-to </h2>
                   <p>
                   Start med at vælge næste fane i toppen af siden, den der hedder 'Valg af sø'. 
                   Her vil du finde de søer og målepunkter som der findes data fra. Et kort vil komme frem efter et par sekunder. 
                   På kortet kan du vælge den sø du er interesseret i ved at klikke på den.
                   Nedenfor vil der komme en tabel med alle de kemiske undersøgelser der er foretaget her.
                   Endvidere, kan du trykke på knappen for at få en grafisk fremstilling. Ved at trykke på knappen rykkes du til næste fane. 
                   Her kan du få en grafisk fremstilling af dataet fra søen, hvor du kan vælge hvilke parametre og tidsinterval du er intereseret i.
                   <br>
                   God fornøjelse.
                   </p>
                   <h2> De ferske vande i Danmark </h2>
                   <p>
                   I Danmark findes der mere end 180.000 søer og damme over 1 hektar. På trods af dette er det et fåtal af disse der er i god økologisk tilstand. 
                   Vi er i Danmark forpligtet til at opfylde EU's vandrammedirektiv, hvori et af kravene er at alle landets søer har mindst god økologisk tilstand i 2027.
                   I den seneste opgørelse af de økologiske og kemiske tilstande af søerne fra 2023, hvor 986 søer blev undersøgt var kun 5 i god tilstand, og rapporten spår en dyster fremtid for søerne.
                   Jævnfør en rapport fra 2019, spås det at ca. 56% af søerne ikke når målet om god økologisk tilstand inden 2027.
                   I 2023 konkluderede man mere end 30% af søerne er i ringe økologisk tilstand, samt at hhv. 23% er i dårlig og moderat tilstand.
                   Denne ringe kvalitet i vores søer skyldes hovedsagligt høje tilførelser af næringsstofferne, fosfor (P) og kvælstof (N),
                   </p>
                   <h2> Datasættet </h2>
                   <p>
                   Det kemiske datasæt der er brugt på denne side, stammer fra <a href='https://kemidata.miljoeportal.dk'>Miljøportalen</a>, hvor det kan hentes af alle, dog kun i tabel-form. 
                   Endvidere, er det muligt at hente data for mange flere parametre end blot de kemiske, også de biologiske og til dels fysiske. 
                   For at sikre at dataet er ajour hentes det en gang om måneden. 
                   <br>
                   <br>
                   Denne hjemmeside er lavet af <a href='mailto:Jonassoe@biology.sdu.dk'</a> Jonas Stage Sø</a>, Ph.D., ferskvandsbiolog, Postdoc ved Syddansk Universitet.
                   <br>
                   <br>
                   <br>
                   <br>
                   <br>
                   </p>
                   ")
    ),
    nav_panel(title = "Valg af område", 
              mainPanel(width = 12,
                        HTML("
                   <h2>På denne side kan du vælge hvilket område du vil undersøge nærmere</h2> 
                   <br>
                   <p>
                   Siden kan tage et par sekunder om at indlæse.
                   Vælg den sø du gerne vil se data fra.
                   Du kan undersøge kortet for hvilke søer der ligger data for, ved at forstørre kortet.
                   Når du har fundet en sø eller målepunkt du gerne vil undersøge, klik da på det. 
                   Søerne som fremstår som blå på kortet er de søer hvor der ligger kemiske data tilgængelige.
                   <br>
                   I nogle tilfælde overlapper målepunkterne ikke med en sø, disse vil derfor blive vidst som en prik på kortet. 
                   <br>
                   </p>
                   "
                        ),
                        actionButton("delete",label="Fjern markeret stationer og indtegnet områder"),
                        br(),br(),
                        leafletOutput("map", width = "100%", height = 600),
                        br(),
                        tags$script('$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'),
                        actionButton("page_change",label="Når du har valgt en station eller tegnet et område, kan du skifte til grafisk fremstilling ved at trykke her."),
                        tags$style(type='text/css', "#page_change { vertical-align: middle; height: 50px; width: 100%;}"),
                        br(),
                        HTML("
                   <br>
                   <p>
                   Her nedenfor vil du se en tabel med alle de målinger der er lavet for den sø du har valgt.
                   Du kan også sortere, filtrere eller søge i målingerne.
                   </p>
                   "),
                        DT::DTOutput("outText"),
              )),
    nav_panel(title = "Grafisk illustration af data", 
              value = "graph_page",
              HTML("
                 <p>
                 Her kan du se en grafisk fremstilling af den/de stationer du valgte på foregående side. 
                 Du skal starte med at vælge hvilke stofparametre du vil undersøge før den grafiske fremstilling kommer frem. 
                 <br>
                 Du kan i venstre side vælge hvilke stofparametre og tidsinterval du vil undersøge nærmere. <br>
                 </p>
                 "),
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  checkboxGroupInput("analysis_plot_select", "Vælg stofparametre at undersøge:"),
                  checkboxGroupInput("medie_plot_select", "Vælg hvilket medie du vil undersøge:"),
                  checkboxGroupInput("prøvetype_select", "Vælg hvilken prøvetype du vil inkludere i dataet:"),
                  sliderInput("depth_plot_select", "Vælg vanddybder at undersøge:", min =0, max =100, value =c(0,100)),
                  sliderInput("chemistry_year_select", "Vælg tidsintervallet at undersøge:", min = 1900, max = 2025,value = c(1900,2025), sep = "")
                ),
                mainPanel(
                  plotOutput("chem_plot_output", height = 800)
                )
              )
    ),
    nav_panel(title = "Ilt kort", 
              value = "oxygen_page",
              HTML("
                       <p>
                       Her kan du se en grafisk fremstilling af den/de stationer du valgte på foregående side. 
                       Du skal starte med at vælge hvilke stofparametre du vil undersøge før den grafiske fremstilling kommer frem. 
                       <br>
                       Du kan i venstre side vælge hvilke stofparametre og tidsinterval du vil undersøge nærmere. <br>
                       </p>
                       "),
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  checkboxGroupInput("o2_medie_plot_select", "Vælg hvilket medie du vil undersøge:", choices = unique(ilt_df$Medie), selected = NULL),
                  checkboxGroupInput("o2_prøvetype_select", "Vælg hvilken prøvetype du vil inkludere i dataet:", choices = unique(ilt_df$Prøvetype), selected = unique(ilt_df$Prøvetype)),
                  radioButtons("o2_type", "Vælg om du vil se ilt koncentration (mg/l) eller mætning (%)", choices = c("Koncentration" = "Oxygen indhold","Mætning" = "Oxygenmætning")),
                  sliderInput("o2_depth_plot_select", "Vælg vanddybder at undersøge:", min =min(ilt_df$`Dybde (m)`), max =max(ilt_df$`Dybde (m)`), value =c(min(ilt_df$`Dybde (m)`),max(ilt_df$`Dybde (m)`))),
                  sliderInput("o2_year_select", "Vælg hvilket år du vil undersøge:", min = min(year(ilt_df$Dato)), max = max(year(ilt_df$Dato)),value = max(year(ilt_df$Dato)), sep = ""),
                  sliderInput("o2_week_select", "Vælg hvilken uge du vil se data fra:", min = 1, max = 52,value = 26)
                ),
                mainPanel(
                  leafletOutput("o2_plot_output", height = 800),
                  fluidRow(
                    actionButton("previous_week", "Forrige uge", width = "50%"),
                    actionButton("next_week", "Næste uge", width = "50%"))
                )
              )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Leaflet ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellit") %>% 
      addCircleMarkers(data = marin_distinct, 
                       lng = ~long, 
                       lat = ~lat,
                       stroke =F, 
                       radius = 3,
                       label = ~Stedtekst,
                       fillColor = "royalblue",
                       fillOpacity = 1,
                       layerId = ~StedID,
                       clusterId = "Marin",
                       group = "Marin",
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11)) %>%
      addCircleMarkers(data = lake_distinct, 
                       lng = ~long, 
                       lat = ~lat,
                       stroke =F, 
                       radius = 3,
                       label = ~Stedtekst,
                       fillColor = "forestgreen", 
                       fillOpacity = 1,
                       layerId = ~StedID,
                       clusterId = "Sø",
                       group = "Sø",
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11)) %>%
      addCircleMarkers(data = stream_distinct, 
                       lng = ~long, 
                       lat = ~lat,
                       stroke =F, 
                       radius = 3,
                       label = ~Stedtekst,
                       fillColor = "darkorange", 
                       fillOpacity = 1,
                       layerId = ~StedID,
                       clusterId = "Vandløb",
                       group = "Vandløb",
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11)) %>%
      addLegend(position = "bottomleft",
                labels = c("Sø","Marin","Vandløb"),
                colors = c("forestgreen", "royalblue","darkorange"),
                opacity = 1) %>% 
      addDrawToolbar(polylineOptions=F,
                     markerOptions = F,
                     circleOptions = F,
                     rectangleOptions = F,
                     circleMarkerOptions = F,
                     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.3,
                                                                                       color = 'black',
                                                                                       weight = 3))) %>% 
      addResetMapButton() %>% 
      webmap::add_cluster_button(cluster_id = "Sø") %>% 
      webmap::add_cluster_button(cluster_id = "Marin") %>% 
      webmap::add_cluster_button(cluster_id = "Vandløb") %>% 
      addLayersControl(baseGroups = c('CartoDB','Satellit'),
                       overlayGroups = c("Sø","Marin","Vandløb")) 
    
  })
  
  # Leaflet polygon ----
  
  values <- reactiveValues(select_id = NULL)
  
  observeEvent(input$map_draw_new_feature,{
    
    n_points = length(input$map_draw_new_feature$geometry$coordinates[[1]])
    
    point_df = data.frame()
    
    for(i in 1:n_points) {
      
      punkt <- data.frame(lat = input$map_draw_new_feature$geometry$coordinates[[1]][[i]][[1]], 
                          long = input$map_draw_new_feature$geometry$coordinates[[1]][[i]][[2]])
      
      rbind(point_df,punkt) -> point_df
    }
    
    point_df %>%
      st_as_sf(coords = c("long","lat"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON") -> drawed_polygon
    
    st_filter(distinct_locations_sf,drawed_polygon) %>% 
      pull(StedID) -> values$stedid_in_polygon
  })
  
  # Select values that is pushed on
  observeEvent(input$map_marker_click, {
    values$select_id <- rbind(input$map_marker_click$id,
                              values$select_id)
    
    print(values$select_id)
  })
  
  # Filter values inside polygon ----
  
  filtered_data <- reactive({
    req(isTruthy(values$stedid_in_polygon) || isTruthy(values$select_id))
    
    combined_df %>% 
      filter(StedID %in% values$stedid_in_polygon | 
               StedID %in% values$select_id) %>% 
      mutate(Dato = ymd_hms(Dato)) -> return_data
    
    updateCheckboxGroupInput(inputId = "analysis_plot_select", choices = sort(unique(return_data[,Stofparameter])))
    updateCheckboxGroupInput(inputId = "prøvetype_select", choices = sort(unique(return_data[,Prøvetype])), selected = unique(return_data[,Prøvetype]))
    updateCheckboxGroupInput(inputId = "medie_plot_select", choices = sort(unique(return_data[,Medie])), selected = unique(return_data[,Medie]))
    updateSliderInput(inputId = "depth_plot_select", min = min(return_data[,`Dybde (m)`]), max = max(return_data[,`Dybde (m)`]))
    updateSliderInput(inputId = "chemistry_year_select", min = year(min(return_data[,Dato])), max = year(max(return_data[,Dato])))
    
    return(return_data) 
  }) 
  
  # Data table output ---- 
  
  output$outText <- renderDataTable(filtered_data()[, .(Stedtekst,Vandområde,Dato = as.Date(Dato),Medie,Prøvetype, `Dybde (m)`, `Faktiske dybder (m)`, Stofparameter= as.factor(Stofparameter),`Resultat-attribut`, Resultat, Enhed)], filter = "top")
  
  # Delete polygons from map ----
  
  observeEvent(input$delete, {
    #process
    leafletProxy("map")%>%
      removeDrawToolbar(clearFeatures=TRUE) %>% 
      addDrawToolbar(polylineOptions=F,
                     markerOptions = F,
                     circleOptions = F,
                     rectangleOptions = F,
                     circleMarkerOptions = F,
                     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.3
                                                                                       ,color = 'black'
                                                                                       ,weight = 3)))
    
    values$stedid_in_polygon <- NULL
    values$select_id <- NULL
  })
  
  # Change page to graphical page
  
  observeEvent(input$page_change, {
    updateNavlistPanel(
      session = getDefaultReactiveDomain(),
      inputId = "inTabset",
      selected = "graph_page"
    )
  })
  
  # Plot graphical data ----
  
  output$chem_plot_output <- renderPlot({
    req(input$analysis_plot_select)
    req(filtered_data())
    
    print(filtered_data())
    
    filtered_data() %>% 
      filter(Stofparameter %in% input$analysis_plot_select ,
             between(year(Dato), input$chemistry_year_select[1], input$chemistry_year_select[2]) ,
             between(`Dybde (m)`, input$depth_plot_select[1],input$depth_plot_select[2])  ,
             Prøvetype %in% input$prøvetype_select,
             Medie %in% input$medie_plot_select) %>% 
      mutate(parameter_unit = paste0(Stofparameter," (",Enhed,")")) -> plot_data
    
    if (nrow(plot_data) == 0) {
      validate("Der er ingen observationer. Prøv at ændre stofparametre, prøvetype, dybde eller årrække.")
    }
    
    print(plot_data)
    
    plot_data %>% 
      ggplot(aes(Dato, Resultat)) + 
      geom_point(size = 3,aes(shape = Medie,fill = as.factor(Stedtekst)), col = "black", shape = 21) + 
      geom_line(aes(col = as.factor(Stedtekst)), show.legend = F) + 
      scale_x_datetime(date_labels = "%F") +
      facet_grid(parameter_unit~1, scales = "free_y", switch = "y")  +
      theme(strip.placement = "outside", 
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            title = element_text(size = 18)) + 
      labs(x= "Dato", 
           y = "",
           fill = "Stedtekst") + 
      scale_shape_manual(values = c(21,22,24))-> chem_plot
    
    print(chem_plot)
  })
  
  # Plot oxygen data ----
  
  o2_plot_df <- reactive({
    req(input$o2_medie_plot_select)
    req(input$o2_prøvetype_select)
    ilt_df %>% 
      filter(Stofparameter == input$o2_type,
             Medie %in% input$o2_medie_plot_select,
             Prøvetype %in% input$o2_prøvetype_select,
             between(`Dybde (m)`, input$o2_depth_plot_select[1],input$o2_depth_plot_select[2]),
             year(Dato) == input$o2_year_select,
             uge == input$o2_week_select
      ) %>% 
      arrange(desc(`Dybde (m)`)) -> o2_selected
    print(o2_selected)
    return(o2_selected)
  })
  
  output$o2_plot_output <-  renderLeaflet({
    
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      setView(11.67019958401003,55.92425708205749, zoom = 6) %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellit") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
      addResetMapButton() 
    
    if(input$o2_type == "Oxygen indhold") {
      
      m %>% 
        addLegend(position = "bottomleft",
                  labels = c("< 2 mg/l","< 4 mg/l","< 6 mg/l","< 8 mg/l","< 10 mg/l","> 10 mg/l"),
                  colors = c("#A50026","#D73027","#F46D43","#D9EF8B","#66BD63","#006837"),
                  opacity = 1)
    } else if(input$o2_type == "Oxygenmætning"){ 
      m %>% 
        addLegend(position = "bottomleft",
                  labels = c("< 20%","< 40%","< 60%","< 80%","< 100%","> 100%"),
                  colors = c("#A50026","#D73027","#F46D43","#D9EF8B","#66BD63","#006837"),
                  opacity = 1) 
    }
    
  })
  
  
  observe({
    if (nrow(o2_plot_df()) == 0) {
      leafletProxy("o2_plot_output") %>% 
        clearMarkers()
      validate("Der er ingen observationer. Prøv at ændre stofparametre, prøvetype, dybde eller årrække.")
    }
    leafletProxy("o2_plot_output") %>% 
      clearMarkers() %>% 
      addCircleMarkers(data = o2_plot_df(), 
                       lng = ~long, 
                       lat = ~lat,
                       stroke =F, 
                       radius = 4,
                       label = ~Resultat,
                       fillColor = ~col,
                       group = ~Medie,
                       fillOpacity = 1) %>% 
      addLayersControl(baseGroups = c('CartoDB','Satellit'),
                       overlayGroups = c("Sø","Marin","Vandløb")) 
  })
  
  ## Change week numbers to be only present values ----
  observe({
    req(input$o2_medie_plot_select)
    req(input$o2_prøvetype_select)
    ilt_df %>% 
      filter(Medie == input$o2_medie_plot_select,
             Prøvetype %in% input$o2_prøvetype_select) -> selected_year_o2
    
    updateSliderInput(inputId = "o2_depth_plot_select", min = min(selected_year_o2[,`Dybde (m)`], na.rm=T), max = max(selected_year_o2[,`Dybde (m)`], na.rm=T))
    updateSliderInput(inputId = "o2_week_select", min = min(selected_year_o2[,uge], na.rm=T), max = max(selected_year_o2[,uge], na.rm=T))
    updateSliderInput(inputId = "o2_year_select", min = min(selected_year_o2[,year(Dato)], na.rm=T), max = max(selected_year_o2[,year(Dato)], na.rm=T))
  })
  
  observeEvent(input$previous_week,
               {
                 selected_week <- input$o2_week_select
                 selected_week <- selected_week-1
                 updateSliderInput(inputId = "o2_week_select", value = selected_week)
               })
  observeEvent(input$next_week,
               {
                 selected_week <- input$o2_week_select
                 selected_week <- selected_week+1
                 updateSliderInput(inputId = "o2_week_select", value = selected_week)
               })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
