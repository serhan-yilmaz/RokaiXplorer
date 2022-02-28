output$histogram_sitecentering <- renderPlot({
  req(processed_dataset())
  ds <- processed_dataset()
  ST = ds$ST
  ST$Phos = ds$Xv
  #hist(ST$Phos)
  m <- mean(ST$Phos, na.rm = T)
  ggplot(ST, aes(x=Phos)) +
    geom_histogram(color="black", fill="lightblue") +
    #theme_classic() +
    theme_bw() +
    theme(text = element_text(size = 16)) + 
    # theme(panel.grid = element_line(color = "#8ccde3",
    #                                  size = 0.75,
    #                                 linetype = 2)) + 
    labs(x = "Log2-FC", y = "Frequency") +
    ggtitle(paste("Mean: ", round(m, digit=2), sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5)) 
})