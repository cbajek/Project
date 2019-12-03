# Libraries
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

# Extract relevant states and variables
county_shp <- county_shp %>%
  filter(STATE_FIPS %in% c(21, 39, 42, 51, 54)) %>%
  mutate(FIPS_Combined = as.character(FIPS)) %>%
  select(FIPS_Combined, geometry)

# Join the shape file to the dataframe
DF_Shp <- county_shp %>%
  inner_join(DF, by = "FIPS_Combined")

# Create the color palatte
pal_prop_opioid <- colorNumeric("viridis", domain = DF_Shp$Prop_Opioid_Reports_County)




ui <- fluidPage(
  sliderInput(inputId = "years", label = "Year Range",
              min = 2010, max = 2017, value = c(2010, 2017), sep = ""),
  selectInput("state", "State", choices = list("Kentucky" = "KY", 
                                               "Ohio" = "OH", 
                                               "Pennsylvania" = "PA", 
                                               "Virginia" = "VA", 
                                               "West Virginia" = "WV")),
  submitButton(text = "Create Plot"),
  plotOutput(outputId = "timeplot1"),
  plotOutput(outputId = "timeplot2")
)

server <- function(input, output){
  output$timeplot1 <- renderPlot({
    DF %>%
      filter(State == input$state) %>%
      ggplot() +
      geom_line(aes(x = Year, y = Prop_Opioid_Reports_State), size = 2, color = "black") +
      scale_x_continuous(limits = input$years) +
      scale_y_continuous(breaks = c(seq(0.20, 0.5, 0.05)), labels = scales::percent, limits = c(0.20, 0.5)) +
      labs(x = "Year", y = "Proportion of Opioid Reports") +
      theme_minimal()
  })
  output$timeplot2 <- renderPlot({
    DF %>% 
      filter(State == input$state) %>% 
      group_by(Substance, Year) %>%
      summarize(State_Sub_Total = sum(Opioid_Reports)) %>%
      ggplot(aes(x = Year, y = State_Sub_Total, fill = Substance)) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = c(input$years[1]:input$years[2]), 
                         limits = c((input$years[1] - 1), (input$years[2] + 1))) +
      labs(x = "Year", y = "Number of Opioid Reports") +
      ggtitle("Statewide Occurences of Opioid Reports by Substance") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)