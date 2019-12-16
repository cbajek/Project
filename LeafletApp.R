# Libraries and packages
library(tidyverse)
library(devtools)
library(ggplot2)
library(openintro)
library(maps)
library(sf)
library(stringr)
library(leaflet)
library(ggthemes)
library(plotly)
library(shinythemes)



########## POPULATION DATA ##########

# Read population data into R
PopData <- read_csv("PopData1.csv") 

# Pad FIPS County code to 3 digits
PopData <- PopData %>% 
  mutate(FIPS_County = str_pad(string = COUNTY, width = 3, pad = "0", side = "left"))

# Combine FIPS State and FIPS County
PopData <- PopData %>% 
  mutate(FIPS_Combined = paste(PopData$STATE, PopData$FIPS_County, sep = ""))

# Extract county names
PopData <- PopData %>%
  mutate(County = str_remove(PopData$CTYNAME, " County"))

# Rename variables
PopData <- PopData %>%
  rename(FIPS_State = STATE)

# Select necessary variables
PopData <- PopData %>%
  select(County, FIPS_County, FIPS_State, FIPS_Combined, POPESTIMATE2010,
         POPESTIMATE2011, POPESTIMATE2012, POPESTIMATE2013, POPESTIMATE2014,
         POPESTIMATE2015, POPESTIMATE2016, POPESTIMATE2017)

# Pivot longer
PopData <- PopData %>% 
  pivot_longer(cols = c("POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012",
                        "POPESTIMATE2013", "POPESTIMATE2014", "POPESTIMATE2015",
                        "POPESTIMATE2016", "POPESTIMATE2017"), 
               names_to = "Year", 
               values_to = "Population")

# Filter out observations at the state level
PopData <- PopData %>% 
  filter(FIPS_County != "000")

# Rename year values
PopData$Year[PopData$Year == "POPESTIMATE2010"] <- 2010
PopData$Year[PopData$Year == "POPESTIMATE2011"] <- 2011
PopData$Year[PopData$Year == "POPESTIMATE2012"] <- 2012
PopData$Year[PopData$Year == "POPESTIMATE2013"] <- 2013
PopData$Year[PopData$Year == "POPESTIMATE2014"] <- 2014
PopData$Year[PopData$Year == "POPESTIMATE2015"] <- 2015
PopData$Year[PopData$Year == "POPESTIMATE2016"] <- 2016
PopData$Year[PopData$Year == "POPESTIMATE2017"] <- 2017

# Select necessary variables
PopData <- PopData %>%
  select(Year, FIPS_Combined, Population)

# Convert variable types to match their counterparts
PopData$Year <- as.numeric(PopData$Year)



########## OPIOID DATA ##########

# Read the opioid data into R
DF <- read_csv("MCM_NFLIS_Data.csv",
               col_types = cols(FIPS_Combined = col_character(),
                                FIPS_State = col_character()))

# Rename variables
DF <- DF %>%
  rename(
    Year = YYYY,
    County = COUNTY,
    Substance = SubstanceName,
    Opioid_Reports = DrugReports,
    Total_Drug_Reports_County = TotalDrugReportsCounty,
    Total_Drug_Reports_State = TotalDrugReportsState
  )

# Change county names to lowercase
DF$County <- tolower(DF$County)
DF$County <- paste(toupper(substr(DF$County, 1, 1)),
                   substr(DF$County, 2, nchar(DF$County)), sep="")

# Compute total opioid reports for each year and county
Annual_County <- DF %>%
  group_by(Year, FIPS_Combined) %>%
  summarize(Total_Opioid_Reports_County = sum(Opioid_Reports))

# Compute total opioid reports for each year and state
Annual_State <- DF %>%
  group_by(Year, FIPS_State) %>%
  summarize(Total_Opioid_Reports_State = sum(Opioid_Reports))

# Join opioid totals back to original dataset
DF <- DF %>%
  inner_join(Annual_County, by = c("Year", "FIPS_Combined")) %>%
  inner_join(Annual_State, by = c("Year", "FIPS_State"))

# Join population data to opioid data
DF <- DF  %>% 
  inner_join(PopData, by = c("Year", "FIPS_Combined"))

# Compute total population for each year and state
Annual_State_Pop <- DF %>%
  group_by(Year, FIPS_State) %>%
  summarize(Population_State = sum(Population))

# Join population total back to original dataset
DF <- DF %>%
  inner_join(Annual_State_Pop, by = c("Year", "FIPS_State"))

# Compute the proportion of opioid reports
DF <- DF %>%
  mutate(Prop_Opioid_Reports_County = Total_Opioid_Reports_County / Total_Drug_Reports_County) %>%
  mutate(Prop_Opioid_Reports_State = Total_Opioid_Reports_State / Total_Drug_Reports_State)

# Compute per capita opioid reports
DF <- DF %>%
  mutate(Percap_Opioid_Reports_County = Total_Opioid_Reports_County / Population) %>%
  mutate(Percap_Opioid_Reports_State = Total_Opioid_Reports_State / Population_State)

# Compute per capita drug reports
DF <- DF %>%
  mutate(Percap_Drug_Reports_County = Total_Drug_Reports_County / Population) %>%
  mutate(Percap_Drug_Reports_State = Total_Drug_Reports_State / Population_State)

# Duplicate observations from missing counties
Dup_DF <- DF %>%
  subset(FIPS_Combined == 51770)
Dup_DF$County <- "Roanoke city"
Dup_DF$FIPS_County <- 161
Dup_DF$FIPS_Combined <- 51161
DF <- rbind(DF, Dup_DF)

# Select full dataset for county and year
All_Obs <- PopData %>%
  select(Year, FIPS_Combined)

# Add missing observations
DF <- DF %>%
  full_join(All_Obs, by = c("Year", "FIPS_Combined"))

# Reorder rows
DF <- DF %>%
  arrange(Year)

# Reorder columns
DF <- DF[c(1,2,3,4,5,6,7,8,11,12,9,10,15,16,17,18,19,20,13,14)]



########## SHAPE FILE DATA ##########

# Read in the shape files
county_shp <- st_read("UScounties/UScounties.shp", quiet = TRUE)
state_shp <- st_read("states_21basic/states.shp", quiet = TRUE)

# Extract relevant states and variables
county_shp <- county_shp %>%
  filter(STATE_FIPS %in% c(21, 39, 42, 51, 54)) %>%
  mutate(FIPS_Combined = as.character(FIPS)) %>%
  select(FIPS_Combined, geometry)

state_shp <- state_shp %>%
  filter(STATE_FIPS %in% c(21, 39, 42, 51, 54)) %>%
  mutate(FIPS_State = as.character(STATE_FIPS)) %>%
  select(FIPS_State, geometry)



########## SUMMARIZATION ##########

# Join the county shape file to the dataframe
DF_Shp <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

# Summarize observations by year and county
DF_Shp_Prop <- DF_Shp %>%
  group_by(Year, FIPS_Combined, County) %>%
  summarize(Prop_Opioid_Reports_County = mean(Prop_Opioid_Reports_County))

DF_PercapOpioid <- DF %>%
  group_by(Year, FIPS_Combined, County) %>%
  summarize(Percap_Opioid_Reports_County = mean(Percap_Opioid_Reports_County))

DF_Shp <- DF_Shp_Prop %>%
  inner_join(DF_PercapOpioid, by = c("Year", "FIPS_Combined", "County"))

# Create the color palattes
pal_prop_opioid <- colorNumeric("viridis", domain = DF_Shp$Prop_Opioid_Reports_County)
pal_percap_opioid <- colorNumeric("viridis", domain = DF_Shp$Percap_Opioid_Reports_County)

# Filter out missing observation
DF <- DF %>%
  drop_na()

# Aggregate and average over the seven year span
DF_Shp_County <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

DF_Shp_State <- state_shp %>%
  inner_join(DF, by = "FIPS_State")

DF_Shp_County_Prop <- DF_Shp_County %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Prop_Opioid_Reports = mean(Prop_Opioid_Reports_County))

DF_County_TotalDrug <- DF %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Total_Drug_Reports = mean(Total_Drug_Reports_County))

DF_County_TotalOpioid <- DF %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Total_Opioid_Reports = mean(Total_Opioid_Reports_County))

DF_County_PercapDrug <- DF %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Percap_Drug_Reports = mean(Percap_Drug_Reports_County))

DF_County_PercapOpioid <- DF %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Percap_Opioid_Reports = mean(Percap_Opioid_Reports_County))

DF_Shp_County_All <- DF_Shp_County_Prop %>%
  inner_join(DF_County_TotalDrug, by = c("FIPS_Combined", "County")) %>%
  inner_join(DF_County_TotalOpioid, by = c("FIPS_Combined", "County")) %>%
  inner_join(DF_County_PercapDrug, by = c("FIPS_Combined", "County")) %>%
  inner_join(DF_County_PercapOpioid, by = c("FIPS_Combined", "County"))

DF_Shp_State_Prop <- DF_Shp_State %>%
  group_by(FIPS_State, State) %>%
  summarize(Prop_Opioid_Reports = mean(Prop_Opioid_Reports_State))

DF_State_TotalDrug <- DF %>%
  group_by(FIPS_State, State) %>%
  summarize(Total_Drug_Reports = mean(Total_Drug_Reports_State))

DF_State_TotalOpioid <- DF %>%
  group_by(FIPS_State, State) %>%
  summarize(Total_Opioid_Reports = mean(Total_Opioid_Reports_State))

DF_State_PercapDrug <- DF %>%
  group_by(FIPS_State, State) %>%
  summarize(Percap_Drug_Reports = mean(Percap_Drug_Reports_State))

DF_State_PercapOpioid <- DF %>%
  group_by(FIPS_State, State) %>%
  summarize(Percap_Opioid_Reports = mean(Percap_Opioid_Reports_State))

DF_Shp_State_All <- DF_Shp_State_Prop %>%
  inner_join(DF_State_TotalDrug, by = c("FIPS_State", "State")) %>%
  inner_join(DF_State_TotalOpioid, by = c("FIPS_State", "State")) %>%
  inner_join(DF_State_PercapDrug, by = c("FIPS_State", "State")) %>%
  inner_join(DF_State_PercapOpioid, by = c("FIPS_State", "State"))



# Create the app itself
ui <- fluidPage(
  theme = shinytheme("readable"),
  h1(strong("Tracking the Spread of Opioids in Appalachia")),
  h2(strong("2010 - 2017")),
  hr(),
  sliderInput(inputId = "years", label = "Year Range",
              min = 2010, max = 2017, value = 2010, sep = ""),
  h3(strong("Spatio Temporal Summary of Opioid Use")),
  leafletOutput("AppMap"),
  h3(strong("Annual Breakdown by Substance Type")),
  plotOutput(outputId = "timeplot"),
  hr(),
  h3(strong("State and County: Seven Year Averages")),
  selectInput(inputId = "stat", label = "Statistic",
              choices = list("Proportion of Opioid Reports" = "Prop_Opioid_Reports",
                             "Total Drug Reports" = "Total_Drug_Reports",
                             "Total Opioid Reports" = "Total_Opioid_Reports",
                             "Per Capita Drug Reports" = "Percap_Drug_Reports",
                             "Per Capita Opioid Reports" = "Percap_Opioid_Reports")),
  leafletOutput("AppMapAvg"),
  splitLayout(
    plotlyOutput(outputId = "scatterplot"),
    plotlyOutput(outputId = "boxplot")
  ),
  hr(),
  h3(strong("Statewide Trends Over Time")),
  splitLayout(
    plotlyOutput(outputId = "line1plot"),
    plotlyOutput(outputId = "line2plot")
  )
)

server <- function(input, output, session) {
  output$AppMap <- renderLeaflet({
    DF_Shp %>%
      filter(Year == input$years) %>%
      leaflet() %>%
        addTiles() %>%
        addTiles(group = "Default Map") %>%
        addPolygons(group = "Proportion of Opioid Reports",
                    stroke = FALSE,
                    label = ~paste(County, ":", round(Prop_Opioid_Reports_County, digits = 2)),
                    fillColor = ~pal_prop_opioid(Prop_Opioid_Reports_County),
                    fillOpacity = 0.75,
                    smoothFactor = 0.5) %>%
        addPolygons(group = "Per Capita Opioid Reports",
                    stroke = FALSE,
                    label = ~paste(County, ":", round(Percap_Opioid_Reports_County, digits = 3)),
                    fillColor = ~pal_percap_opioid(Percap_Opioid_Reports_County),
                    fillOpacity = 0.75,
                    smoothFactor = 0.5) %>%
        addPolygons(data = state_shp,
                    group = "State Outlines",
                    stroke = TRUE,
                    fill = FALSE,
                    weight = 2,
                    opacity = 1,
                    color = "black") %>%
        addLegend(group = "Proportion of Opioid Reports",
                  pal = pal_prop_opioid,
                  values = c(0, 1),
                  opacity = 1,
                  title = "Proportion <br> of Opioid <br> Reports",
                  position = "bottomright") %>%
        addLegend(group = "Per Capita Opioid Reports",
                  pal = pal_percap_opioid,
                  values = c(0, 0.015),
                  opacity = 1,
                  title = "Per <br> Capita <br> Opioid <br> Reports",
                  position = "bottomleft") %>%
        addLayersControl(
                  baseGroups = c("Default Map", "Proportion of Opioid Reports", "Per Capita Opioid Reports"),
                  overlayGroups = c("State Outlines"),
                  options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("State Outlines")
  })
  output$timeplot <- renderPlot({
    DF %>%
      filter(Year == input$years) %>%
      group_by(Substance, Year) %>%
      summarize(Sub_Total = sum(Opioid_Reports)) %>%
      ungroup() %>%
      arrange(desc(Sub_Total)) %>%
      top_n(10) %>%
      mutate(Substance = fct_inorder(Substance)) %>%
      ggplot(aes(x = fct_rev(Substance), y = Sub_Total, fill = Substance)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d() +
      labs(x = "Substance", y = "Number of Opioid Reports") +
      theme_minimal() +
      theme(axis.text.x = element_text(face = "bold")) +
      theme(axis.text.y = element_text(face = "bold"))
  })
  output$AppMapAvg <- renderLeaflet({
    if (input$stat == "Prop_Opioid_Reports") {
        palCounty <- colorNumeric("viridis", domain = DF_Shp_County_All$Prop_Opioid_Reports)
        palState <- colorNumeric("viridis", domain = DF_Shp_State_All$Prop_Opioid_Reports)
        varCounty <- DF_Shp_County_All$Prop_Opioid_Reports
        varState <- DF_Shp_State_All$Prop_Opioid_Reports
        legendTitle <- "Proportion <br> of Opioid <br> Reports"
    }
    else if (input$stat == "Total_Drug_Reports") {
        palCounty <- colorNumeric("viridis", domain = DF_Shp_County_All$Total_Drug_Reports)
        palState <- colorNumeric("viridis", domain = DF_Shp_State_All$Total_Drug_Reports)
        varCounty <- DF_Shp_County_All$Total_Drug_Reports
        varState <- DF_Shp_State_All$Total_Drug_Reports
        legendTitle <- "Total <br> Drug <br> Reports"
    }
    else if (input$stat == "Total_Opioid_Reports") {
        palCounty <- colorNumeric("viridis", domain = DF_Shp_County_All$Total_Opioid_Reports)
        palState <- colorNumeric("viridis", domain = DF_Shp_State_All$Total_Opioid_Reports)
        varCounty <- DF_Shp_County_All$Total_Opioid_Reports
        varState <- DF_Shp_State_All$Total_Opioid_Reports
        legendTitle <- "Total <br> Opioid <br> Reports"
    }
    else if (input$stat == "Percap_Drug_Reports") {
        palCounty <- colorNumeric("viridis", domain = DF_Shp_County_All$Percap_Drug_Reports)
        palState <- colorNumeric("viridis", domain = DF_Shp_State_All$Percap_Drug_Reports)
        varCounty <- DF_Shp_County_All$Percap_Drug_Reports
        varState <- DF_Shp_State_All$Percap_Drug_Reports
        legendTitle <- "Per Capita <br> Drug <br> Reports"
    }
    else if (input$stat == "Percap_Opioid_Reports") {
        palCounty <- colorNumeric("viridis", domain = DF_Shp_County_All$Percap_Opioid_Reports)
        palState <- colorNumeric("viridis", domain = DF_Shp_State_All$Percap_Opioid_Reports)
        varCounty <- DF_Shp_County_All$Percap_Opioid_Reports
        varState <- DF_Shp_State_All$Percap_Opioid_Reports
        legendTitle <- "Per Capita <br> Opioid <br> Reports"
    }
    leaflet(data = DF_Shp_County_All) %>%
      addTiles() %>%
      addTiles(group = "Default Map") %>%
      addPolygons(group = "County Data",
                  stroke = FALSE,
                  label = ~paste(County, ":", round(varCounty, digits = 3)),
                  fillColor = ~palCounty(varCounty),
                  fillOpacity = 0.75,
                  smoothFactor = 0.5) %>%
      addPolygons(data = DF_Shp_State_All,
                  group = "State Data",
                  stroke = FALSE,
                  label = ~paste(State, ":", round(varState, digits = 3)),
                  fillColor = ~palState(varState),
                  fillOpacity = 0.75,
                  smoothFactor = 0.5) %>%
      addPolygons(data = state_shp,
                  group = "State Outlines",
                  stroke = TRUE,
                  fill = FALSE,
                  weight = 2,
                  opacity = 1,
                  color = "black") %>%
      addLayersControl(
        baseGroups = c("Default Map", "County Data", "State Data"),
        overlayGroups = c("State Outlines"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = palState,
                values = ~varState,
                opacity = 1,
                title = paste("State Level: <br> ", legendTitle),
                position = "bottomleft",
                bins = 6) %>%
      addLegend(pal = palCounty,
                values = ~varCounty,
                opacity = 1,
                title = paste("County Level: <br> ", legendTitle),
                position = "bottomright",
                bins = 6) %>%
      hideGroup("County Data") %>%
      hideGroup("State Data") %>%
      hideGroup("State Outlines")
  })
  output$scatterplot <- renderPlotly({
    ggplotly(DF %>%
               group_by(County, State) %>%
               summarize(Prop_Opioid_Reports = mean(Prop_Opioid_Reports_County),
                         Percap_Opioid_Reports = mean(Percap_Opioid_Reports_County)) %>%
               ggplot(aes(x = Percap_Opioid_Reports, y = Prop_Opioid_Reports, color = State, text = County)) +
               geom_point(size = 0.75) +
               scale_x_continuous(breaks = c(seq(0, 0.01, 0.001)), limits = c(0, 0.01)) +
               scale_y_continuous(breaks = c(seq(0, 0.75, 0.15)), labels = scales::percent, 
                                  limits = c(0, 0.75)) +
               labs(x = "Per Capita Opioid Reports", y = "Proportion of Opioid Related Reports") +
               ggtitle("Proportion Opioid by Per Capita Reports") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
               theme(axis.text.y = element_text(face = "bold")),
             tooltip = c("County", "State")
    )
  })
  output$boxplot <- renderPlotly({
    ggplotly(DF %>%
               group_by(County, State) %>%
               summarize(Percap_Opioid_Reports = mean(Percap_Opioid_Reports_County)) %>%
               ggplot(aes(x = State, y = Percap_Opioid_Reports, fill = State)) +
               geom_boxplot() +
               scale_y_continuous(breaks = c(seq(0, 0.85, 0.001)), 
                                  limits = c(0, 0.01)) +
               labs(x = "State", y = "Per Capita Opioid Reports") +
               ggtitle("Per Capita Opioid Reports by State") +
               theme_minimal() + 
               theme(axis.text.x = element_text(face = "bold")) +
               theme(axis.text.y = element_text(face = "bold"))
    )
  })
  output$line1plot <- renderPlotly({
    ggplotly(DF %>%
               group_by(State, Year) %>%
               summarize(Percap_Opioid_Reports = mean(Percap_Opioid_Reports_State)) %>%
               ggplot(aes(x = Year, y = Percap_Opioid_Reports, color = State)) +
               geom_line(size = 1) +
               scale_x_continuous(breaks = c(seq(2010, 2017, 1)), limits = c(2010, 2017)) +
               scale_y_continuous(breaks = c(seq(0, 0.0004, 0.0001)), limits = c(0, 0.0004)) +
               labs(x = "Year", y = "Per Capita Opioid Reports") +
               ggtitle("Per Capita Opioid Reports") +
               theme_minimal(),
             tooltip = c("Percap_Opioid_Reports", "Year")
    )
  })
  output$line2plot <- renderPlotly({
    ggplotly(DF %>%
               group_by(State, Year) %>%
               summarize(Prop_Opioid_Reports = mean(Prop_Opioid_Reports_State)) %>%
               ggplot(aes(x = Year, y = Prop_Opioid_Reports, color = State)) +
               geom_line(size = 1) +
               scale_x_continuous(breaks = c(seq(2010, 2017, 1)), limits = c(2010, 2017)) +
               scale_y_continuous(breaks = c(seq(0.20, 0.5, 0.05)), labels = scales::percent, limits = c(0.20, 0.5)) +
               labs(x = "Year", y = "Percent") +
               ggtitle("Proportion of Opioid Related Reports") +
               theme_minimal(),
             tooltip = c("Prop_Opioid_Reports", "Year")
    )
  })
}

shinyApp(ui = ui, server = server)
