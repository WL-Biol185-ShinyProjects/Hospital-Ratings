function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

output$staff_chart <- renderPlot({
  staff_rating %>%
    ggplot(aes(x = reorder(`Facility Name`, ...), y = ...)) +
    geom_bar(stat = "identity", fill = "#2c7fb8") +
    coord_flip() +
    theme_minimal()
})
