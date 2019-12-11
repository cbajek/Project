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
library(shinythemes)

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

# Compute the proportion of opioid reports
DF <- DF %>%
  mutate(Prop_Opioid_Reports_County = Total_Opioid_Reports_County / Total_Drug_Reports_County) %>%
  mutate(Prop_Opioid_Reports_State = Total_Opioid_Reports_State / Total_Drug_Reports_State)

# Duplicate observations from missing counties
Dup_DF <- DF %>%
  subset(FIPS_Combined == 51770)
Dup_DF$County <- "Roanoke city"
Dup_DF$FIPS_County <- 161
Dup_DF$FIPS_Combined <- 51161
DF <- rbind(DF, Dup_DF)

# Reorder rows
DF <- DF %>%
  arrange(Year)

# Reorder columns
DF <- DF[c(1,2,3,4,5,6,7,8,11,12,9,10,13,14)]

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

# Join the county shape file to the dataframe
DF_Shp <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

# Summarize observations
DF_Shp_Prop <- DF_Shp %>%
  group_by(Year, FIPS_Combined, County) %>%
  summarize(Prop_Opioid_Reports_County = mean(Prop_Opioid_Reports_County))

DF_TotalDrug <- DF %>%
  group_by(Year, FIPS_Combined, County) %>%
  summarize(Total_Drug_Reports_County = mean(Total_Drug_Reports_County))

DF_Shp <- DF_Shp_Prop %>%
  inner_join(DF_TotalDrug, by = c("Year", "FIPS_Combined", "County"))

# Create new obeservations for missing counties


# Create the color palattes
pal_prop_opioid <- colorNumeric("viridis", domain = DF_Shp$Prop_Opioid_Reports_County)
pal_total_drug <- colorNumeric("viridis", domain = DF_Shp$Total_Drug_Reports_County)



# Aggregate and average over the seven year span
DF_Shp_County <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

DF_Shp_State <- state_shp %>%
  inner_join(DF, by = "FIPS_State")

DF_Shp_County <- DF_Shp_County %>%
  group_by(FIPS_Combined, County) %>%
  summarize(Prop_Opioid_Reports_County = mean(Prop_Opioid_Reports_County))

DF_Shp_State <- DF_Shp_State %>%
  group_by(FIPS_State, State) %>%
  summarize(Prop_Opioid_Reports_State = mean(Prop_Opioid_Reports_State))

pal_prop_avg <- colorNumeric("viridis", domain = DF_Shp_County$Prop_Opioid_Reports_County)



# Create the app itself
ui <- fluidPage(
  theme = shinytheme("readable"),
  h1(strong("Tracking the Spread of Opioids in Appalachia")),
  h2(strong("2010 - 2017")),
  hr(),
  sliderInput(inputId = "years", label = "Year Range",
              min = 2010, max = 2017, value = 2010, sep = ""),
  # selectInput(inputId = "stat", label = "Statistic", 
  #             choices = list("Proportion of Opioid Reports" = "Prop_Opioid_Reports_County",
  #                            "Total Drug Reports" = "Total_Drug_Reports_County")),
  h3(strong("Spatial Summary of Opioid Use")),
  leafletOutput("AppMap"),
  h3(strong("Annual Breakdown by Substance Type")),
  plotOutput(outputId = "timeplot"),
  hr(),
  h3(strong("State and County -- Seven Year Averages")),
  leafletOutput("AppMapAvg")
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
                    label = ~paste(County, round(Prop_Opioid_Reports_County, digits = 2)),
                    fillColor = ~pal_prop_opioid(Prop_Opioid_Reports_County),
                    fillOpacity = 0.75,
                    smoothFactor = 0.5) %>%
        addPolygons(group = "Total Drug Reports",
                    stroke = FALSE,
                    label = ~County,
                    fillColor = ~pal_total_drug(Total_Drug_Reports_County),
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
        addLegend(group = "Total Drug Reports",
                  pal = pal_total_drug,
                  values = c(0, 10000),
                  opacity = 1,
                  title = "Total <br> Drug <br> Reports",
                  position = "bottomleft") %>%
        addLayersControl(
                  baseGroups = c("Default Map", "Proportion of Opioid Reports", "Total Drug Reports"),
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
      theme_minimal()
  })
  output$AppMapAvg <- renderLeaflet({
    leaflet(data = DF_Shp_County) %>%
      addTiles() %>%
      addTiles(group = "Default Map") %>%
      addPolygons(group = "County Data",
                  stroke = FALSE,
                  label = ~County,
                  fillColor = ~pal_prop_avg(Prop_Opioid_Reports_County),
                  fillOpacity = 1,
                  smoothFactor = 0.5) %>%
      addPolygons(data = DF_Shp_State,
                  group = "State Data",
                  stroke = FALSE,
                  label = ~State,
                  fillColor = ~pal_prop_avg(Prop_Opioid_Reports_State),
                  fillOpacity = 1,
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
      addLegend(pal = pal_prop_avg,
                values = ~Prop_Opioid_Reports_County,
                opacity = 1,
                title = "Proportion <br> of Opioid <br> Reports",
                position = "bottomright") %>%
      hideGroup("County Data") %>%
      hideGroup("State Data") %>%
      hideGroup("State Outlines")
  })
}

shinyApp(ui = ui, server = server)

