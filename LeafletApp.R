library(tidyverse)
library(devtools)
library(ggplot2)
library(openintro)
library(maps)
library(sf)
library(stringr)
library(leaflet)
library(ggthemes)

# Read the data into R
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

ui <- fluidPage(
  sliderInput(inputId = "years", label = "Year Range",
              min = 2010, max = 2017, value = 2010, sep = ""),
  selectInput(inputId = "stat", label = "Statistic", 
              choices = list("Proportion of Opioid Reports" = "Prop_Opioid_Reports_County",
                             "Total Drug Reports" = "Total_Drug_Reports_County")),
  leafletOutput("AppMap"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output, session) {
  output$AppMap <- renderLeaflet({
    DF_Shp %>%
      filter(Year == input$years) %>%
      leaflet() %>%
        addTiles(group = "Default") %>% 
        addPolygons(group = "Proportion of Opioid Reports",
                    stroke = FALSE,
                    label = ~County,
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
                  values = ~Prop_Opioid_Reports_County,
                  opacity = 1,
                  title = "Proportion of Opioid Reports",
                  position = "bottomright") %>%
        addLegend(group = "Total Drug Reports",
                  pal = pal_total_drug,
                  values = ~Total_Drug_Reports_County,
                  opacity = 1,
                  title = "Total Drug Reports",
                  position = "bottomleft") %>%
        addLayersControl(
                  baseGroups = c("Default"),
                  overlayGroups = c("State Outlines", "Proportion of Opioid Reports", "Total Drug Reports"),
                  options = layersControlOptions(collapsed = FALSE))
  })
  output$timeplot <- renderPlot({
    DF %>%
      filter(Year == input$years) %>%
      group_by(Substance, Year) %>%
      summarize(Sub_Total = sum(Opioid_Reports)) %>%
      ungroup() %>%
      arrange(desc(Sub_Total)) %>%
      top_n(10) %>%
      ggplot(aes(x = fct_rev(fct_inorder(Substance)), y = Sub_Total, fill = Substance)) +
      geom_col() +
      coord_flip() +
      labs(x = "Substance", y = "Number of Opioid Reports") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)







