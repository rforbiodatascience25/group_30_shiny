# Load needed libraries
library("shiny")
library("bslib")
library("ggplot2")

# Define the Server (Backend)
server <- function(input, output) {
  output$dna <- renderText({
    gene_dna(length = input$n_bases,
             base_probs = c(input$prob_A, 
                            input$prob_T,
                            input$prob_C,
                            input$prob_G))
    })
  output$rna <- renderText({
    transcribe_dna(input$dna)
  })
  output$aa <- renderText({
    translate_rna(input$rna)
  })
  output$freq_plot <- renderPlot({
    x <- base_freqs(input$dna)
    x |>
    ggplot2::ggplot(ggplot2::aes(x = x$dna_vec,
                                 y = x$Freq,
                                 fill = x$dna_vec)) +
      ggplot2::geom_col() +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::xlab("DNA base") + 
      ggplot2::ylab("Frequency")
  })
}