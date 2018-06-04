#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("BSSP Aligner"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("fasta1",
                     "Reference Sequence:",
         		  multiple = F, 
         		  buttonLabel = "Upload"),
         fileInput("fasta2",
         		  "Sample Sequence", 
         		  multiple = F,
         		  buttonLabel = "Upload"),
         selectInput("choice", label = "DNA or Protein Alignment", choices = c("DNA", "Protein"), multiple = F),
         actionButton(inputId = "run", label = "Run")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("diffPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

	df <- reactive({
		input$run
		req(input$fasta1)
		req(input$fasta2)
		seq1 <- seqinr::read.fasta(file = input$fasta1$datapath, as.string = T)[[1]]
		seq2 <- seqinr::read.fasta(file = input$fasta2$datapath, as.string = T)[[1]]
		name1 = attr(seq1, "Annot")
		name2 = attr(seq2, "Annot")
		df <- data.frame(s1 = strsplit(seq1[1], split="")[[1]],
						 s2 = strsplit(seq2[1], split="")[[1]])
		
		
		if(as.character(input$choice)=="Protein"){
			tr1 <- seqinr::translate(strsplit(seq1[1], split="")[[1]])
			tr2 <- seqinr::translate(strsplit(seq2[1], split="")[[1]])
			df <- data.frame(s1 = tr1, 
							 s2 = tr2)
		}
		df$different = df[,1]!=df[,2]
		df$position = 1:nrow(df)	
		colnames(df)[1:2] <- c(name1, name2)

		return(df)
	})	

   output$diffPlot <- renderPlot({
   	input$run
   	df <- df()
   	library(ggplot2)
      i = which(df$different)
      ii = (i-10):(i+10)
      ggplot(data = df[ii,]) +
      	geom_text(aes(x = position, y = colnames(df)[2], label = df[ii,2], color = different, fontface="bold")) +
      	geom_text(aes(x = position, y = "Position", label = df[ii, 4], color = different)) +
      	geom_text(aes(x = position, y = colnames(df)[1], label = df[ii, 1], color = different, fontface='bold')) +
      	geom_rect(data = df, aes(xmin = position[i] - 0.5, xmax = position[i]+0.5, ymin = 0.5, ymax = 3.5), fill=NA, color = "black") +
      	theme(legend.position = 'none', panel.grid = element_blank()) +
      	labs(y = "")
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

