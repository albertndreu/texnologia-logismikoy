library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(Rtsne)
library(caret)
library(cluster)
library(factoextra)
library(class)
library(e1071) 
library(caTools)

ui <- fluidPage(
  titlePanel("Data Analysis Application with RShiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      fileInput("file2", "Choose Excel File", accept = ".xlsx"),
  
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("contents")),
        
        tabPanel("2D Visualization",
                 selectInput("method", "Choose Method", choices = c("PCA", "t-SNE")),
                 actionButton("plot", "Plot"),
                 plotlyOutput("plot2d")
        ),
        
        tabPanel("Classification",
                 selectInput("classifier", "Choose Classifier", choices = c("k-NN", "Logistic Regression")),
                 numericInput("k", "k for k-NN", 3, min = 1, max = 20),
                 actionButton("classify", "Classify"),
                 verbatimTextOutput("classification_result")
        ),
        
        tabPanel("Clustering",
                 selectInput("clustering", "Choose Clustering Method", choices = c("k-means", "Hierarchical")),
                 numericInput("k_clusters", "Number of clusters (k)", 3, min = 1, max = 10),
                 actionButton("cluster", "Cluster"),
                 plotOutput("clustering_result")
        )
      
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    
    if (!is.null(input$file1)) {
      read_csv(input$file1$datapath)
    } else if (!is.null(input$file2)) {
      read_excel(input$file2$datapath)
    }

  })
  
  output$contents <- renderTable({
    data()
  })
  
  
  
  output$plot2d <- renderPlotly({
    req(input$plot)
    df <- data()
    labels <- df[, ncol(df)]
    features <- df[, -ncol(df)]
    
    
    if (input$method == "PCA") {
      pca_result <- prcomp(features, center = TRUE, scale. = TRUE)
      plot_data <- data.frame(pca_result$x[, 1:2], Label = labels)
      colnames(plot_data) <- c("PC1", "PC2", "Label")
      p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Label)) + geom_point()
    } else if (input$method == "t-SNE") {
      tsne_result <- Rtsne(features, dims = 2, perplexity = 10)
      plot_data <- data.frame(tsne_result$Y, Label = labels)
      colnames(plot_data) <- c("Dim1", "Dim2", "Label")
      p <- ggplot(plot_data, aes(x = Dim1, y = Dim2, color = Label)) + geom_point()
    }
    
    ggplotly(p)
  })
  
  output$classification_result <- renderPrint({
    req(input$classify)
    df <- data()
    
    # Extract class labels and ensure they are factors
    labels <- as.factor(df[[ncol(df)]])
    
    # Debugging: Print unique classes
    print(unique(labels))
    
    # Check if there are at least two unique classes
    unique_classes <- unique(labels)
    if (length(unique_classes) < 2) {
      return("Error: The dataset must contain at least two distinct classes for classification.")
    }
    
    features <- df[, -ncol(df)]  # Features excluding the last column (class labels)
    
    # Perform data splitting using indices
    set.seed(123)  # for reproducibility
    trainIndex <- createDataPartition(labels, p = 0.8, list = FALSE)
    trainData <- features[trainIndex,]
    testData <- features[-trainIndex,]
    trainLabels <- labels[trainIndex]
    testLabels <- labels[-trainIndex]
    
    if (input$classifier == "k-NN") {
      model <- train(x = trainData, y = trainLabels, method = "knn", tuneGrid = data.frame(k = input$k))
    } else if (input$classifier == "Logistic Regression") {
      model <- train(x = trainData, y = trainLabels, method = "glm")
    }
    
    # Check if test data exists
    if (is.null(testData) || nrow(testData) == 0) {
      return("Error: No test data available for classification.")
    }
    
    # Make predictions
    predictions <- predict(model, testData)
    confusionMatrix(predictions, testLabels)
  })
  
  
  
  
  
  output$clustering_result <- renderPlot({
    req(input$cluster)
    df <- data()
    features <- df[, -ncol(df)]
    
    if (input$clustering == "k-means") {
      km_result <- kmeans(features, centers = input$k_clusters)
      fviz_cluster(km_result, data = features)
    } else if (input$clustering == "Hierarchical") {
      hclust_result <- hclust(dist(features))
      plot(hclust_result)
    }
  })
}

shinyApp(ui, server)
