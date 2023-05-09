library(shiny)
library(ggplot2)
library(ggbiplot)
library(DT)
library(FactoMineR)
library(factoextra)

function(input, output, session) {
  
  data(iris)
  
  #PCA繪圖處理
  observeEvent(input$input_size, {
    
    log.ir <- log(iris[1:input$input_size, 1:4])
    ir.species <- iris[1:input$input_size, 5]
    
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    pca_df <- as.data.frame(ir.pca$rotation)
    
    output$pca_summary <- renderPrint({
      summary(ir.pca)
    })
    
    output$rotation <- renderTable({
      rotation_df <- as.data.frame(ir.pca$rotation)
    }, rownames = TRUE)
    
    output$center <- renderTable({
      center_df <- as.data.frame(ir.pca$center)
    }, rownames = TRUE)
    
    output$xtable <- renderDT({
      datatable(ir.pca$x, rownames = TRUE)
    })
    
    # 監聽x,y
    observeEvent(c(input$x, input$y), {
      
      x <- match(input$x, names(pca_df))
      y <- match(input$y, names(pca_df))
      
      #繪圖
      g <- ggbiplot(ir.pca, choices = c(x, y), obs.scale = 1, var.scale = 1, 
                    groups = ir.species, ellipse = TRUE)
      output$pca_plot <- renderPlot(print(g))
      })
    
    })
  
  #CA繪圖處理
  observeEvent(input$ca_input_size, {
    
    ca <- CA(iris[1:input$ca_input_size,1:4])
    rowscores <- as.data.frame(ca$row$coord)
    colscores <- as.data.frame(ca$col$coord)
    
    output$ca_summary <- renderPrint({
      summary(ca)
    })
  
    output$ca_plot <- renderPlot({
      fviz_ca_biplot(ca,
                     col.var = "blue",
                     col.row = "blue")
    })
  })
  
  #Kmeans繪圖處理
  observeEvent(input$k_size, {
    
    set.seed(123)
    km <- kmeans(iris[, 1:4], input$k_size)
    
    cluster <- iris
    cluster$cluster <- factor(km$cluster)
    
    ca <- CA(cluster[, 1:4], graph = FALSE)
    
    output$kmeans_plot <- renderPlot({
      fviz_ca_biplot(ca,
                     col.var = "blue",
                     col.row = cluster$cluster)
    })
  })
  
}
