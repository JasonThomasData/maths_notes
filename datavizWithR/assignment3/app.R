library(shiny)
library(ggplot2)

asthma <- read.csv("asthma.csv")
diabetes <- read.csv("diabetes.csv")
heart_stroke_vascular <- read.csv("heart_stroke_vascular.csv")
three_or_more_chronic <- read.csv("three_or_more_chronic.csv")
SA2_SEIFA_2021 <- read.csv("SA2_SEIFA_2021.csv")

#Remove duplicates
diabetes <- diabetes[, -2]
heart_stroke_vascular <- heart_stroke_vascular[, -2]
three_or_more_chronic <- three_or_more_chronic[, -2]
SA2_SEIFA_2021 <- SA2_SEIFA_2021[, -2]

names(SA2_SEIFA_2021)[1] <- "SA2.Code"

mergedData <- merge(asthma, diabetes, "SA2.Code")
mergedData <- merge(mergedData, heart_stroke_vascular, "SA2.Code")
mergedData <- merge(mergedData, three_or_more_chronic, "SA2.Code")
mergedData <- merge(mergedData, SA2_SEIFA_2021, "SA2.Code")

readableToMachine <- function(population, disease ) {
  populationNames <- list(
    "All ages" = "All.ages",
    "60 years and over" = "60.years.and.over",
    "70 years and over" = "70.years.and.over"
  )
  diseaseNames <- list(
    "Asthma" = "Asthma",
    "Diabetes" = "Diabetes",
    "Heart, stroke and vascular" = "Heart.stroke.vascular",
    "Three or more chronic conditions" = "Three.or.more.chronic"
  )
  (paste(diseaseNames[disease],populationNames[population], sep="....."))
}

socialIndexDescriptionHash <- function(index) {
  description <- list(
    "ISRAD" = "ISRAD: A low score indicates greater relative social advantage and disadvantage, such as low occupation levels and unskilled labour. A high score might indicate high incomes and skilled occupations.",
    "ISRD" = "ISRD: A low score indicates greater relative social advantage and disadvantage, such as low occupation levels and unskilled labour. This index does not include relative social advantage.",
    "IEO" = "IEO: A low score indicates a lower educational and occupational levels in the area. A high score might indicate relatively high levels of income and education.",
    "IER" = "IER: A low score indicates a lack of access to economic resources, such as people renting and households with low income. A higher score indicates home ownership and relatively high household incomes."
  )
  (paste(description[index]))
}

getDisplayData <- function(input) {
  populationDiseaseColumnNames <- readableToMachine(input$population, input$disease)
  data.frame(x=mergedData[,c(input$socialIndex)],
             y=mergedData[,c(populationDiseaseColumnNames)],
             sa2Label=mergedData[,"SA2.Label"])
}

focusOnSA2 = NA

ui <- fluidPage(
  h1("The relationship between socio-ecominic indicators and several diseases"),
  fluidRow(
    sidebarPanel(
      checkboxInput("lobf", "Include line of best fit", value = FALSE, width = NULL),
      checkboxInput("zeroOutliers", "Include zero outliers", value = TRUE, width = NULL),
      selectInput("population", label = "Population age", choices = c("All ages", "60 years and over", "70 years and over")),
      selectInput("disease", label = "Disease", choices = c("Asthma", "Diabetes", "Heart, stroke and vascular", "Three or more chronic conditions")),
      selectInput("socialIndex", label = "Social index", choices = c("ISRD", "ISRAD", "IER", "IEO")),
      textOutput("socialIndexDescription"),
      br(),
      selectizeInput("focusOnSA2_1", label = "Focus on SA2:", choices = NULL,
                     selected = NULL, multiple = FALSE, options = NULL),
      selectizeInput("focusOnSA2_2", label = NULL, choices = NULL,
                     selected = NULL, multiple = FALSE, options = NULL)
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      plotOutput("comparisonPlot"),
      p("Notes:"),
      p("1) Some SA2 estimates were removed due to uncertainty relating to low sample sizes.")
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'focusOnSA2_1', choices = c("None", mergedData$SA2.Label), server = TRUE)
  updateSelectizeInput(session, 'focusOnSA2_2', choices = c("None", mergedData$SA2.Label), server = TRUE)
  
  output$scatterPlot <- renderPlot({
    displayData <- getDisplayData(input)
    
    if(!input$zeroOutliers) {
      displayData[displayData==0] <- NA
    }

    if(input$lobf) {
      modelWidth <- 0.3
    } else {
      modelWidth <- 0
    }

    ggplot(displayData, aes(x, y)) + 
      geom_point(alpha = 0.2, size=2 ) +
      geom_point(data = displayData[displayData$sa2Label==input$focusOnSA2_1,], size=3, color="red") +
      geom_point(data = displayData[displayData$sa2Label==input$focusOnSA2_2,], size=3, color="blue") +
      theme_bw() +
      geom_smooth(method=lm, se=FALSE, linetype="solid", color="darkred", linewidth = modelWidth) +
      labs(title = paste("People (%) with", tolower(input$disease), "by", input$socialIndex, "in the population:", tolower(input$population)),
           subtitle = "Each point represents an SA2 location in Australia (defined by the ABS).",
           x=input$socialIndex,
           y=paste(input$disease, "(%) in population:", tolower(input$population)))
  })

  output$comparisonPlot <- renderPlot({

    displayData <- getDisplayData(input)
    displayData <- displayData[c(input$focusOnSA2_1, input$focusOnSA2_2),]
    
    ggplot(displayData, aes(x = displayData$sa2Label, y = displayData$y)) +
      geom_bar(position="dodge", stat="identity") +
      coord_flip()
  })

  output$socialIndexDescription <- renderText({
    socialIndexDescriptionHash(input$socialIndex)
  })
}

shinyApp(ui, server)