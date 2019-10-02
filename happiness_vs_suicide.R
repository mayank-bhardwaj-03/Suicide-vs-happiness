library(shiny)
library(maps)
library(leaflet)
library(maptools)
library(plotly)
library(shinyjs)
library(scatterD3)
library(scales)
library(shinythemes)

world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

happy_2015 <- read.csv("happy_final.csv",stringsAsFactors = FALSE)
cnt <- happy_2015

cnt$country <- as.character(cnt$country)

target <- subset(world_map, country %in% cnt$country)
joined <- merge(target, cnt, by.x="country", by.y="country", duplicate = TRUE)
target<-joined

suicide_master<- read.csv("suicde_master.csv",stringsAsFactors = FALSE)
suicide_master_temp <- suicide_master[, c("country", "year", "suicides.100k.pop","suicides_no", "continent")]

suicide_master_temp <- aggregate(suicide_master_temp[, c("suicides.100k.pop", "suicides_no")],
                                 by = list(suicide_master_temp$country, suicide_master_temp$year,
                                           suicide_master_temp$continent)
                                 , sum, na.rm = T)
colnames(suicide_master_temp) <- c("country", "year", "continent", "suicides.100k.pop", "suicides_no")

world_s <- map("world", fill=TRUE, plot=FALSE)
world_map_s <- map2SpatialPolygons(world_s, sub(":.*$", "", world_s$names))
world_map_s <- SpatialPolygonsDataFrame(world_map_s,
                                        data.frame(country=names(world_map_s), 
                                                   stringsAsFactors=FALSE), 
                                        FALSE)

suicide_cnt <- suicide_master_temp

target_s <- subset(world_map_s, country %in% suicide_cnt$country)

joined_s <- merge(target_s, suicide_cnt, by.x="country", by.y="country",duplicate=TRUE)
class(joined_s)
target_s<-joined_s

measure <- list("sex", "age", "continent", "country")

happy_suicide <- read.csv("happy_suicide.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  theme = "bootstrap.css",
  
  tags$head(tags$style(
    type="text/css",
    "#image img {max-width: 100%; width: auto; height: auto}"
  )),
  
  titlePanel(windowTitle = "Suicide vs Happiness",
             title =
               div(
                 img(
                   src = "http://66.media.tumblr.com/tumblr_mdre2pGMCL1r4e3ico1_500.png",
                   height = 100,
                   width = 500,
                   style = "margin:10px 10px"
                 ),
                 "Suicide vs Happiness Dashboard",
                 class = 'title'
               )                  
  ),
  
  navbarPage(id = "Navbar",
             fluid = TRUE,
             theme = shinythemes::shinytheme("spacelab"),
             footer = helpText(
               "Reviews/feedback/suggestions on the dashboard",
               a(href="mailto: mbha0003@student.monash.edu", target="_blank", "here")
             ),
             tags$style(type="text/css", "body {padding-top: 0px;}"),
             
             # Show a plot of the generated distribution
             
             tabPanel("About",
                      
                      fluidRow(
                        column(12,tags$h2("Introduction"),
                               tags$p(class = "intro",
                                      "Suicidal death are the causes of leading death around the globe which varies from different 
                                         age groups and demographic conditions. By using this data, I try to understand what
                                         makes the victim do suicides their mental condition, demographic, happiness of the country etc.
                                         Motivation behind this is to find out which factors play important role for the happiness of 
                                         country and suicidal tendencies. In addition to these, I will give user a freedom for checking
                                         dependencies with the help of linear regression line for the different variables. 
                                         This helps user to find out Is there any relation between happiness and Economy of a country? Or 
                                      Does mental health and GDPI of a country affect suicide rate? Or How Happiness Score 
                                      is affected by Suicide rate of a country?"),
                               tags$br("This application provides the user-friendly interface to explore the happiness and suicide data for the world.
                               The data has been taken from the Kaggle and ourworldindata.org. 
                                       In addendum to this it also helps user to understand to understand the Suicide vs Happiness trends "),
                               tags$h2("Happiness"),
                               tags$p("There are 8 variables which has been used to track trend in their ratings. 
                                      Below mention are the variables present in the data."),
                               HTML("
                   <ul>
                   <li>HappinessRank - This the rank started from 1 and till to the total number of countries. Number 1 rank says country is happiest in the world</li>
                   <li>Happiness Score  - This is the score assigned to every country from the scale of 0 to 10. Higher the score happier is the country</li>
                   <li>Economy(GDP per Capita) - This the GDP of the country per capita. It ranges from 0 to 2,which is lowest to highest respectively. </li>
                   <li>Family- The extent to which Family contributes to the calculation of the Happiness Score. It also ranges from 0 to 2. Higher the number greater the family support.</li>
                   <li> Healthy (Life Expectancy) - The country-specific ratios for the longer and healthy life expectancy are applied to other years to generate healthy life expectancy.</li>
                   <li>Freedom (to Make Life Choices) - The national average of binary responses to the question \"Are you satisfied or dissatisfied with your freedom to choose what you do with your life?\" </li>
                  
                   </ul>"),
                               
                               tags$h2("Suicide and mental health "),
                               tags$p("There are 5 variables in these two dataset. Below mention are the variables used for this application."),
                               HTML("
                   <ul>
                   <li>Country - This has been the country name in the world</li>
                   <li>Continent - There are different continents as Asia, Americas, Europe, Africa and Oceania. Africa has lowest data from the countries.(User can ignore Africa data if they wish) </li>
                   <li>Suicide rate - This the number of suicides per 100k happen within the country. </li>
                   <li>Suicide no. - This the total number of suicides has occurred in the country for the given year.</li>
                   <li> Mental health - This is the total number of  mental health patient in the country for the given year.</li>
                  
                   </ul>")
                        )
                      )),
             tabPanel("Happy World", value = "Happy",
                      fluidRow(column(4,
                                      div(class = "well",
                                          uiOutput("variable"),
                                          uiOutput("year"),
                                          tags$hr(style="border-color: red;border-top: 3px solid #F511BC;"),
                                          
                                          tags$p("Use the dropdown menu Map Variable to explore the different variable 
                                          (for more information on variable, please visit the About tab)
                                          Choose the year in the dropdown menu to see world map of that year."),
                                          tags$p("OR"),
                                          tags$p("Click directly on the country for trends and more details."),
                                          htmlOutput("t2")
                                          
                                          
                                      )
                      ),
                      column(8,
                             leafletOutput("leaflet" ),
                             
                             tags$br(tags$p(paste0("Please note that the map may take a few seconds to download")))
                      ) 
                      ), fluidRow(tags$br())),
             
             tabPanel("Happiness Statistics",value = "Happ_Stats",
                      
                      fluidRow(column(4,
                                      div(class = "well",
                                          uiOutput("variable_1"),
                                          uiOutput("country")),
                                      HTML("<strong>Statistics shown :</strong>
                   <ul>
                   <li>Use the dropdown menu Y Variable to explore the different variable (for more information on variable, please visit the About tab).</li>
                   <li>Choose the Countries in the menu to see trend for different countries</li>
                    </ul>")
                                      
                      ),
                      column(8,
                             plotlyOutput("lineplot")
                             
                      )), fluidRow(tags$br())
             ),
             
             tabPanel("Suicidal World", value = "suicide",
                      column(4,div(class = "well",
                                   uiOutput("year_s")
                      ),
                      fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                       
                                       tags$p("The World map shows estimation of death rates from suicide/self-harm, 
                                                     measured as the number of deaths per 100,000 people over the year. "),
                                       
                                       tags$p("
                                          Choose the year in the dropdown menu to see world map of that year."),
                                       tags$p("OR"),
                                       tags$p("Click directly on the country for trends and more details."))
                               
                      )),
                      column(8,leafletOutput("leaflet_s"),
                             htmlOutput("t3"),
                             tags$br(tags$p("Notes:Country in white colour has no data.")),
                             (tags$p(paste0("Please note that the map may take a few seconds to download"))))
             ),
             tabPanel("Suicide Statistics",value = "sui_stats",
                      column(4,div(class = "well",useShinyjs(),
                                   radioButtons("viewSelect",
                                                "Select view mode",
                                                c("Time series", "Cross section")),
                                   
                                   radioButtons("varSelect",
                                                "Select variable",
                                                c("rate", "num")),
                                   
                                   selectInput("measureSelect",
                                               "Select measure",
                                               measure, selected = "country"),
                                   uiOutput("yearSelect"),
                                   
                                   uiOutput("categorySelect"),
                                   
                                   
                                   HTML("<strong>Statistics shown :</strong>
                   <ul>
                   <li>Time series give the trend of a country over the year.</li>
                   <li>Cross section gives the bar graph for the selected measure</li>
                   <li>Rate gives the number per 100k population</li>
                   <li>Num gives the total number of suicides</li>
                   <li>In cross section tab, only the Top 10 countries are shown in case country is selected as measure</li>
                    </ul>")))
                      ,
                      column(8,div(class = "well",
                                   tags$br(""),
                                   plotlyOutput("distPlot"))))
             ,
             tabPanel("Suicide v/s Happiness",useShinyjs(),
                      column(4,div(class = "well", radioButtons("view",
                                                                "Select view mode",
                                                                c("Continent", "Region"), selected = "Continent"),
                                   
                                   uiOutput("ContinentSelect"),
                                   
                                   uiOutput("RegionSelect"),
                                   uiOutput("number"),
                                   selectInput("scatterD3_x", "x variable :",
                                               choices = c("Happiness Score" = "happiness_score",
                                                           "GDP_per_capita" = "GDP_per_capita",
                                                           "Family" = "Family",
                                                           "life_expectancy" = "life_expectancy",
                                                           "Freedom" = "Freedom",
                                                           "Trust in Government" = "trust_in_government",
                                                           "Generosity" = "Generosity",
                                                           "Suicide Rate" = "suicide_rate_100k",
                                                           "Mental Health" = "mental_disorder_rate_100k"),
                                               selected = "happiness_score"),
                                   
                                   selectInput("scatterD3_y", "y variable :",
                                               choices = c("Happiness Score" = "happiness_score",
                                                           "GDP_per_capita" = "GDP_per_capita",
                                                           "Family" = "Family",
                                                           "life_expectancy" = "life_expectancy",
                                                           "Freedom" = "Freedom",
                                                           "Trust in Government" = "trust_in_government",
                                                           "Generosity" = "Generosity",
                                                           "Suicide Rate" = "suicide_rate_100k",
                                                           "Mental Health" = "mental_disorder_rate_100k"),
                                               selected = "suicide_rate_100k"),
                                   
                                   
                                   uiOutput("zoom"),
                                   
                                   
                                   checkboxInput("scatterD3_threshold_line", "Regression line", value = FALSE),    
                                   sliderInput("scatterD3_labsize", "Labels size :",
                                               min = 5, max = 25, value = 11),
                                   checkboxInput("scatterD3_auto_labels", "Automatic labels placement", value = TRUE),
                                   sliderInput("scatterD3_opacity", "Points opacity :", min = 0, max = 1, value = 1, step = 0.05),
                                   checkboxInput("scatterD3_transitions", "Use transitions", value = TRUE),
                                   tags$p(actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
                                          actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
                                          tags$a(id = "scatterD3-svg-export", href = "#",
                                                 class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG")))
                      )),
                      column(8,
                             
                             titlePanel("Correlation between different attributes"),
                             div(class="row",
                                 div(class="col-md-12",
                                     div(class="alert alert-warning alert-dismissible",
                                         HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                                         HTML("<strong>Functionalities :</strong>
                   <ul>
                   <li>Zoom on the chart with the mousewheel</li>
                   <li>Regression line helps in understanding the trend between different variables</li>
                   <li>Clicking on dot gives us country information on the Wikipedia</li>
                   <li>You can download the graph and open it in browser</li>
                   <li>Lasso will help in selecting the selected areas country names</li>
                   <li>Hovering over the labels display label on the graph and hovering on graph display details associated with country</li>
                  
                   </ul>"))),
                                 scatterD3Output("scatterPlot"))
                      )
             )
             
             
  )          
  
  
  
  
)

server <- function(input, output, session) {  
  
  
  # suicide statistics leaflet map
  
  # selection panel for the variable to show the leaflet on
  output$variable <- renderUI({
    selectInput("variable", "Map-variable:", 
                choices = c("Happiness.Rank"="happiness_rank", "Happiness.Score"="happiness_score",
                            "Economy"="GDP_per_capita", 
                            "Family"="Family",
                            "Health"="life_expectancy", "Freedom"="Freedom", 
                            "Trust in Government"="trust_in_government", 
                            "Generosity" = "Generosity"), selected = "happiness_score")
  })
  
  # selection panel for Year
  output$year <- renderUI({
    selectInput("year", "Year:", 
                choices = c("2015", "2016", "2017"), selected = "2015")
  })
  
  # observe events for the above and generating the leaflet based on that
  observeEvent({input$variable
    input$year} , { 
      
      output$leaflet <- renderLeaflet({ 
        
        # setting the color pallete
        pal <-colorNumeric(c("darkred", "orangered", "orange", "yellow", "yellowgreen", "green"),
                           target@data[target@data$year== input$year,input$variable])
        
        # setting the labels  of the leaflet map
        labels <- sprintf(
          "%s<br/>%f<br/>",
          target[target@data$year== input$year,]$country, target@data[target@data$year== input$year,input$variable]
        ) %>% 
          lapply(htmltools::HTML)
        
        leaflet(target[target@data$year== input$year,]) %>%
          setView(0, 0,  zoom = 2) %>%
          
          setMaxBounds(230, 95, -230, -73) %>%
          addTiles()   %>% 
          #addProviderTiles(providers$Stamen.TonerHybrid)%>% 
          
          addPolygons(#
            weight = 1,
            #color = "#000",
            opacity = 0.2,
            # dashArray = "3",
            fillOpacity = 1,
            fillColor = pal(target@data[target@data$year== input$year,input$variable]),
            highlightOptions = highlightOptions(
              color='#000000', opacity = 1, weight = 1, fillOpacity = 1,
              bringToFront = F, sendToBack = T),
            label = labels,
            labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                        textsize = "15px",
                                        direction = "auto")) %>%
          addLegend(pal = pal, values = target@data[target@data$year== input$year,input$variable], opacity = 0.7, 
                    title = NULL, position = "bottomright")
        
      })
    })
  
  ## happiness statistics in more detail
  
  # selecting the variable for plotting the graph
  output$variable_1 <- renderUI({
    selectInput("variable_1", "Y-variable:", 
                choices = c("Happiness.Rank"="happiness_rank", "Happiness.Score"="happiness_score",
                            "Economy"="GDP_per_capita", 
                            "Family"="Family",
                            "Health"="life_expectancy", "Freedom"="Freedom", 
                            "Trust in Government"="trust_in_government", 
                            "Generosity"),  selected = 'happiness_rank'
    )
  })
  
  # country for plotting the graoh
  countries <- reactive({ 
    df_small <- cnt %>% 
      select(country) %>% 
      droplevels() %>%
      unique
    
    c(df_small$country)
    
  })
  # country selection panel
  output$country <- renderUI ({
    
    selectInput("country", "Countries:", 
                multiple = TRUE,
                choices = countries(), 
                selected = "Australia")
  })
  
  #generating the intial plot in the Happiness Statistics
  output$lineplot <- renderPlotly({
    
    yvar <- input$variable_1
    
    df <- happy_2015
    if(!is.null(input$variable_1)){
      filtered_data <- reactive({
        df <- happy_2015 %>% 
          filter(country %in% c(input$country)) %>% 
          arrange(country)
        
        return(df)
        
        
      })
      
      p <- ggplot(filtered_data(), aes_string(x = "year", y = input$variable_1, 
                                              colour = "country")) +
        geom_line( size = 1) +
        geom_point( size = 4)+
        ggtitle(paste0(input$variable_1, " vs. Year")) +
        theme_bw() +
        theme(legend.position = "bottom")+
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())+
        xlab("Year")+
        ylab(input$variable_1)+
        scale_x_continuous(breaks = seq(2015, 2017, by = 1))
      
      
      
      ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
      
    }
    
    
  })
  
  output$t2 <- renderText({
    
    c1 = happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == 1, "country"]
    c2 = happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == 2, "country"]
    c3 = happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == 3, "country"]
    
    cl = happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == max(happy_2015$happiness_rank), "country"]
    cs =happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == max(happy_2015$happiness_rank)-1, "country"]
    ct =happy_2015[happy_2015$year == input$year & happy_2015$happiness_rank == max(happy_2015$happiness_rank)-2, "country"]
    paste("<b>According to the world Happiness index (HPI) in year,", input$year, "are" , c1 , "is the happiest country on Earth, followed by",
          c2,",",c3 ,"and these are the unhappiest country", cl,"," ,cs,"," ,ct, "</b>"
    )
    
  }
  )
  
  # plotting the Suicidal statistics based on the leaflet shape click
  observeEvent(input$leaflet_shape_click,
               
               {
                 
                 updateSelectInput(session,"country",  
                                   selected = "")
                 
                   updateSelectInput(session,"variable_1",  
                                   selected = input$variable)
                 
                 output$lineplot <- renderPlotly({
                   
                   
                   
                   if(is.null(input$variable_1))
                     yvar <- input$variable
                   else
                     yvar <- input$variable_1
                   
                   click <- input$leaflet_shape_click
                   
                   
                   
                   df <- happy_2015
                   
                   filtered_data <- reactive({
                     
                     country_c <- map.where(database="world", click$lng, click$lat)
                     
                     coun <- strsplit(country_c, ":")
                     country_c <- coun[[1]][1]
                     
                     df <- happy_2015 %>% 
                       filter(country %in% c(input$country, country_c)) %>% 
                       arrange(country)
                     
                     return(df)
                     
                     
                   })
                   
                   p <- ggplot(filtered_data()) +
                     geom_line(aes_string(x = "year", y = yvar, 
                                          colour = "country"), size = 1) +
                     geom_point(aes_string(x = "year", y = yvar, 
                                           colour = "country"), size = 4)+
                     ggtitle(paste0(yvar, " vs. Year")) +
                     theme_bw() +
                     theme(legend.position = "bottom")+
                     theme(
                       panel.grid.minor.y = element_blank(),
                       panel.grid.minor.x = element_blank())+
                     xlab("Year")+
                     ylab(input$variable_1)+
                     scale_x_continuous(breaks = seq(2015, 2017, by = 1))
                   
                   ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
                 })
                 
                 
                 #updating the tab on leaflet click
                 updateTabsetPanel(session, "Navbar","Happ_Stats")
               })
  
  ## generating the leaflet for suicides
  
  # selecting the year
  
  output$t3 <- renderText({
    
    df_temp <- suicide_master[suicide_master$year == input$year_s,]
    df_temp <- aggregate(df_temp[, c("suicides.100k.pop")], by = list(df_temp$country ), sum, na.rm = T)
    colnames(df_temp) <- c("country", "suicide")
    df_temp <- df_temp[order(df_temp[, "suicide"], decreasing = TRUE),]
    rownames(df_temp) <- 1:nrow(df_temp)
    
    c1 = df_temp[1, "country"]
    c2 = df_temp[2, "country"]
    c3 = df_temp[3, "country"]
    
    cl = df_temp[nrow(df_temp), "country"]
    cs =df_temp[nrow(df_temp)-1, "country"]
    ct =df_temp[nrow(df_temp)-2, "country"]
    paste("<b>According to the world Happiness index (HPI) in year,", input$year_s, "are" , c1 , "is the higest suicidal country in world, followed by",
          c2,",",c3 ,"and these are the lowest suicidal country", cl,"," ,cs,"," ,ct, "</b>"
    )
    
  }
  )
  
  output$year_s <- renderUI({
    
    choices <- sort(unique(target_s@data$year))
    
    selectInput("year_s", "Year:", 
                choices = choices, selected = 2015, multiple = FALSE)
    
  })
  
  #leaflet function
  
  observeEvent(input$year_s, {
    
    output$leaflet_s <- renderLeaflet({
      
      yr <- input$year_s
      
      
      pal <-colorNumeric(c("darkred", "orangered", "orange", "yellow", "yellowgreen", "green"),
                         target_s[target_s@data$year == yr,]$suicides.100k.pop)
      
      
      labels <- sprintf(
        "%s<br/>%f<br/>",
        target_s[target_s@data$year == yr,]$country, target_s[target_s@data$year == yr,]$suicides.100k.pop
      ) %>% 
        lapply(htmltools::HTML)
      
      leaflet(target_s[target_s@data$year == yr,]) %>%
        setView(0, 0,  zoom = 2) %>%
        addTiles() %>% 
        #addProviderTiles(providers$Stamen.TonerHybrid)%>% 
        addPolygons(weight = 1,
                    #color = "#000",
                    opacity = 0.2,
                    # dashArray = "3",
                    fillOpacity = 1,
                    fillColor = pal(target_s[target_s@data$year == yr,]$suicides.100k.pop),
                    highlightOptions = highlightOptions(
                      color='#000000', opacity = 1, weight = 1, fillOpacity = 1,
                      bringToFront = F, sendToBack = T),
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")) %>%
        addLegend(pal = pal, values =target_s[target_s@data$year == yr,]$suicides.100k.pop, opacity = 0.7, 
                  title = NULL, position = "bottomright")
      
      
      
    })
  })
  
  
  # checking what view is selected in the Suicide statistics tab and then changing the selection panle based on that
  observe({
    if(input$viewSelect == "Time series") {
      show("categorySelect", anim = TRUE, time = 1) 
      hide("yearSelect", anim = TRUE)
    } else {
      hide("categorySelect", anim = TRUE) 
      show("yearSelect", anim = TRUE, time = 1)
    }
  })
  #Year selection panel 
  output$yearSelect <- renderUI({
    choices <- 
      
      sort(unique(suicide_master[, "year"])
           
      )
    
    selectInput("yearSelect",
                "Select year",
                choices = choices, "2016")
  })
  
  #Year selection panel 
  output$categorySelect <- renderUI({
    
    choices <- 
      unique(suicide_master[, input$measureSelect])
    
    
    
    selectInput("categorySelect",
                "Select category",
                choices = choices,
                multiple = TRUE,
                selected = choices[1])
  })
  
  #observing the leaflet country click and based on that the statistics plot is generated
  observeEvent(input$leaflet_s_shape_click, {
    
    
    click_s <- input$leaflet_s_shape_click
    
    country_s <- map.where(database="world", click_s$lng, click_s$lat)
    
    coun <- strsplit(country_s, ":")
    country_s <- coun[[1]][1]
    
    updateSelectInput(session, "viewSelect",
                      selected = "Time series")
    
    updateSelectInput(session, "measureSelect",
                      selected = "country")
    
    updateSelectInput(session,"categorySelect",  
                      selected = "")
    
    
    # eventReactive(input$email,
    #              {HTML('<a href="mailto:taylor.winter00@gmail.com?Subject=Suice%20app%20feedback" target="_top"></a>')})
    
    
    output$distPlot <- renderPlotly({
      
      
      if( (!is.null(input$viewSelect)) & (!is.null(input$measureSelect))& (!is.null(input$varSelect))){
        
        
        y <- ifelse(input$varSelect == "rate",
                    "suicides.100k.pop",
                    "suicides_no"
        )
        
        x <- ifelse(input$measureSelect == "country",
                    "country",
                    ifelse(input$measureSelect == "sex",
                           "sex",
                           ifelse(input$measureSelect == "age",
                                  "age",
                                  "continent")
                    )
        )
        
        # for selected Cross Section tab
        if (input$viewSelect == "Cross section") {
          if(!is.null(input$yearSelect) & (!is.null(input$viewSelect)) &
             (!is.null(input$measureSelect)) & (!is.null(input$varSelect))){
            
            df <-suicide_master[suicide_master$year == as.numeric(input$yearSelect) , c(input$measureSelect, y)]
            
            df <- aggregate(df[,2], by = list(df[, 1]), sum, na.rm = T)
            colnames(df) <- c(input$measureSelect, y)
            
            df <- df[order(df[, y], decreasing = TRUE),]
            
            if(nrow(df) > 10){
              df <- df[1:10,]
            }
            
            
            ggplotly(
              ggplot(df, 
                     aes_string(x=input$measureSelect, y=y)) +
                geom_bar(stat = "identity") +
                labs(
                  y = y,
                  x = x
                ) +
                theme_classic() +
                theme(
                  axis.text.x = element_text(angle = 35, hjust = 1)
                )+
                scale_y_continuous(labels = comma)+
                ggtitle(paste0(input$measureSelect, " vs.", y))
            ) 
          } }
        else {
          # for selected Time Series tab
          if (input$viewSelect == "Time series") {
            
            if((!is.null(input$viewSelect)) & 
               (!is.null(input$measureSelect)) & (!is.null(input$varSelect))  ){
              
              
              
              if(input$measureSelect == "country")
                df<- suicide_master[suicide_master[, input$measureSelect] %in% c(country_s, input$categorySelect),
                                    c(input$measureSelect, y, "year")]
              
              else
                df<- suicide_master[suicide_master[, input$measureSelect] %in% input$categorySelect,
                                    c(input$measureSelect, y, "year")]
              
              df <- aggregate(df[,2], by = list(df[, 1], df$year), sum, na.rm = T)
              colnames(df) <- c(input$measureSelect,"year", y)
              
              ggplotly(
                ggplot(df, 
                       aes_string(x="year", y=y, group=input$measureSelect, color=input$measureSelect)) +
                  geom_point() +
                  geom_line() +
                  labs(
                    y = y,
                    x = "Year"
                  ) +
                  theme_classic() +
                  theme(
                    axis.text.x = element_text(angle = 35, hjust = 1)
                  )+
                  scale_x_continuous(breaks = seq(1985, 2016, by = 2))+
                  scale_y_continuous(labels = comma)+
                  ggtitle(paste0(input$measureSelect, " vs.", y))
                , tooltip = c("year", y)
              )
            }
            
          }
        }
      }
    })
    
    #updating the tab on leaflet click
    updateTabsetPanel(session, "Navbar","sui_stats")
  })
  
  # if suicide statistics tab is clicked, then the initial plot
  output$distPlot <- renderPlotly({
    
    
    if( (!is.null(input$viewSelect)) & (!is.null(input$measureSelect)) & (!is.null(input$varSelect))){
      
      y <- ifelse(input$varSelect == "rate",
                  "suicides.100k.pop",
                  "suicides_no"
      )
      
      x <- ifelse(input$measureSelect == "country",
                  "country",
                  ifelse(input$measureSelect == "sex",
                         "sex",
                         ifelse(input$measureSelect == "age",
                                "age",
                                "continent")
                  )
      )
      
      
      if (input$viewSelect == "Cross section") {
        if(!is.null(input$yearSelect) & (!is.null(input$viewSelect)) &
           (!is.null(input$measureSelect)) & (!is.null(input$varSelect))){
          
          
          df <-suicide_master[suicide_master$year == as.numeric(input$yearSelect) , c(input$measureSelect, y)]
          
          df <- aggregate(df[,2], by = list(df[, 1]), sum, na.rm = T)
          colnames(df) <- c(input$measureSelect, y)
          
          df <- df[order(df[, y], decreasing = TRUE),]
          
          if(nrow(df) > 10){
            df <- df[1:10,]
          }
          
          
          ggplotly(
            ggplot(df, 
                   aes_string(x=input$measureSelect, y=y)) +
              geom_bar(stat = "identity") +
              labs(
                y = y,
                x = x
              ) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 35, hjust = 1)
              )+
              scale_y_continuous(labels = comma)+
              ggtitle(paste0(input$measureSelect, " vs.", y))
          ) 
        }
      } 
      else
      {
        if (input$viewSelect == "Time series") {
          
          if((!is.null(input$categorySelect)) & (!is.null(input$viewSelect)) & 
             (!is.null(input$measureSelect)) & (!is.null(input$varSelect)) ){
            
            df<- suicide_master[suicide_master[, input$measureSelect] %in% input$categorySelect,
                                c(input$measureSelect, y, "year")]
            
            df <- aggregate(df[,2], by = list(df[, 1], df$year), sum, na.rm = T)
            colnames(df) <- c(input$measureSelect,"year", y)
            
            ggplotly(
              ggplot(df, 
                     aes_string(x="year", y=y, group=input$measureSelect, color=input$measureSelect
                                
                     )) +
                geom_point() +
                geom_line() +
                labs(
                  y = y,
                  x = "Year"
                ) +
                theme_classic() +
                theme(
                  axis.text.x = element_text(angle = 35, hjust = 1)
                )+
                scale_x_continuous(breaks = seq(1985, 2016, by = 2))+
                scale_y_continuous(labels = comma)+
                ggtitle(paste0(input$measureSelect, " vs.", y))
              , tooltip = c("year", y)
            )
            
          }
          
        }
      }
    }
  })
  
  
  ## The happiness and Suicide statistics
  
  # creating observe events for the selection panels and showing and hiding them based on the option selected
  observe({
    if(input$view == "Continent") {
      show("ContinentSelect", anim = TRUE, time = 1) 
      hide("RegionSelect", anim = TRUE)
    } else {
      hide("ContinentSelect", anim = TRUE) 
      show("RegionSelect", anim = TRUE, time = 1)
    }
    
    
    output$ContinentSelect <- renderUI({
      choices <- 
        
        unique(happy_suicide[, "continent"]
               
        )
      
      selectInput("ContinentSelect",
                  "Select Continent",
                  multiple = TRUE,
                  choices = choices, "Europe")
    })
    
    
    output$RegionSelect <- renderUI({
      
      choices <- 
        unique(happy_suicide[, "region"])
      
      
      
      selectInput("RegionSelect",
                  "Select Region",
                  choices = choices,
                  multiple = TRUE,
                  selected = "Western Europe")
    })
    
    d <- subset(happy_suicide, continent %in% c("Europe"))
    
    
    output$zoom <- renderUI({
      choices <- 
        unique(d[, "country"])
      
      selectInput("scatterD3_zoomon", "Zoom on :", choices = c("None", choices))
      
    })
    output$number <- renderUI({sliderInput("scatterD3_nb", "Number of observations",
                                           min = 3, max = nrow(d), step = 1, value = nrow(d))})
    
  })
  
  # observe event for the scatterD3 plot
  observeEvent({input$view 
    input$ContinentSelect 
    input$RegionSelect
    input$scatterD3_threshold_line
    input$scatterD3_nb
    input$scatterD3_x
    input$scatterD3_y
  }, {
    
    # checking which view is selecte: Region or continent and assigning df accordingly
    if(input$view == "Continent" || is.na(input$view)){
      
      if(!is.null(input$ContinentSelect))
        d <- happy_suicide[happy_suicide$continent %in% input$ContinentSelect, ]
      else     
        d <- NULL
      
      
    }
    
    else{
      if(input$view == "Region" || is.na(input$view) ){
        if(!is.null(input$RegionSelect))
          d <- happy_suicide[happy_suicide$region %in% input$RegionSelect, ]
        #else     
        # d <- happy_suicide[happy_suicide$region %in% c("Western Europe"), ]
        else
          d <- NULL
      }
    }
    
    # checking if the dataset is null or not, if it is null, plotted the values else the empty plot is generated
    if(!is.null(d)){ 
      choices <- 
        unique(d[, "country"])
      
      updateSelectInput(session, "scatterD3_zoomon",
                        choices = c("None", choices))
      
      
      
      
      
      data <- reactive({
        updateSliderInput(session,"scatterD3_nb", 
                          min = 3, max = nrow(d), step = 1, value = input$scatterD3_nb)
        
        if (is.null(input$scatterD3_nb))
        {n <- 3}
        else
        {n <- input$scatterD3_nb}
        
        return(d[1:n,])
      })
      
      var <- as.name(input$scatterD3_x)
      
      # fitting the data value and ficing coefficients and intercepts for the regression line
      fla <- paste(input$scatterD3_y, input$scatterD3_x, sep = "~")
      
      m <- lm(fla, data())
      
      if(length(m) == 12){
        if(is.null(summary(m)$coefficients[2,1])){s <- 0}
        else { s <- summary(m)$coefficients[2,1]}
        
        if(is.null(summary(m)$coefficients[1,1])){i<-0}
        else{i <- summary(m)$coefficients[1,1]}
      }
      else{
        s<- 1
        i <- 0
      }
      
      threshold_line <- data.frame(slope = s, 
                                   intercept = i, 
                                   stroke = "#F67E7D",
                                   stroke_width = 2,
                                   stroke_dasharray = "")
      
      default_lines <- data.frame(slope = c(0, Inf), 
                                  intercept = c(0, 0),
                                  stroke = "#000",
                                  stroke_width = 1,
                                  stroke_dasharray = c(5, 5))
      
      lines <- reactive({
        if (input$scatterD3_threshold_line || is.null(input$scatterD3_threshold_line)) {
          return(rbind(default_lines, threshold_line))
        }
        default_lines
      })
      
      # generating the plots based on above values
      output$scatterPlot <- renderScatterD3({
        
        
        auto_label <- if (!input$scatterD3_auto_labels  || is.na(input$scatterD3_auto_labels)) "auto"
        else if(input$scatterD3_auto_labels) NULL
        
        zoom_on <-  if (input$scatterD3_zoomon == "None" || is.null(input$scatterD3_zoomon)) {
          NULL 
        } else {
          c(data()[data()["country"] == input$scatterD3_zoomon, input$scatterD3_x],
            data()[data()["country"] == input$scatterD3_zoomon, input$scatterD3_y])
        }
        
        scatterD3(x = data()[,input$scatterD3_x],
                  y = data()[,input$scatterD3_y],
                  lab = data()[, "country"],
                  xlab = input$scatterD3_x,
                  ylab = input$scatterD3_y,
                  col_var = data()[, "country"],
                  col_lab = "Country Legend",
                  size_lab = input$scatterD3_size,
                  url_var = paste0("https://en.wikipedia.org/wiki/", data()[, "country"]),
                  zoom_on = zoom_on,
                  zoom_on_level = 3,
                  labels_positions = auto_label,
                  point_opacity = input$scatterD3_opacity,
                  labels_size = input$scatterD3_labsize,
                  transitions = input$scatterD3_transitions,
                  left_margin = 90,
                  lines = lines(),
                  lasso = TRUE,
                  caption = list(title = "Correlation between country attributes",
                                 text = "Yep, you can even use <strong>markup</strong> in caption text. <em>Incredible</em>, isn't it ?"),
                  lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}")
      })
    }
    else{  output$scatterPlot <- renderScatterD3({ scatterD3(x = 0,
                                                             y = 0)
    })
    }
  })
  
}

shinyApp(ui, server)