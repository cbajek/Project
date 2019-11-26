
library(shiny)
library(tidyverse)
library(leaflet)

# Read the data into R
DF <- read_csv("MCM_NFLIS_Data.csv", 
               col_types = cols(FIPS_Combined = col_character(), 
                                FIPS_State = col_character()))

# Rename Variables
DF <- DF %>%
  rename(
    Year = YYYY,
    County = COUNTY,
    Substance = SubstanceName,
    Opioid_Reports = DrugReports,
    Total_Drug_Reports_County = TotalDrugReportsCounty,
    Total_Drug_Reports_State = TotalDrugReportsState
  )

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

# Reorder Columns
DF <- DF[c(1,2,3,4,5,6,7,8,11,12,9,10,13,14)]

# Read in the shape file and join dataframes
county_shp <- st_read("UScounties/UScounties.shp", quiet = TRUE)

# Extract relevant states and variables
county_shp <- county_shp %>%
  filter(STATE_FIPS %in% c(21, 39, 42, 51, 54)) %>%
  mutate(FIPS_Combined = as.character(FIPS)) %>%
  #mutate(geomPoly = st_cast(geometry, "POLYGON", group_or_split = FALSE)) %>%
  select(FIPS_Combined, geometry)

# Join the shape file to the dataframe
DF_Shp <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

# Create the color palatte
pal_prop_opioid <- colorNumeric("viridis", domain = DF_Shp$Prop_Opioid_Reports_County)

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output,session) {
  output$mymap <- renderLeaflet({
    leaflet(data = DF_Shp) %>%
      addTiles() %>% 
      addPolygons(stroke = FALSE,
                  label = ~County,
                  fillColor = ~pal_prop_opioid(Prop_Opioid_Reports_County),
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  highlight = highlightOptions(weight = 5, color = "black", fillOpacity = 0.9,
                                               bringToFront = FALSE)) %>%
      addLegend(pal = pal_prop_opioid,
                values = ~Prop_Opioid_Reports_County,
                opacity = 0.5,
                title = "Proportion of Opioid Reports",
                position = "bottomright")
      
  })
}

shinyApp(ui = ui, server = server)





