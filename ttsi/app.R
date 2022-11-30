XV_GRAPH_CHOICE <- c('Scatterplot','Boxplot','Density plot','Histogram')
################################################################
scatterplotUI <- function(XID,XT) {
	tagList(
		selectInput(NS(XID,"X_SCP_1"), "Abscissa", names(XT)),
		selectInput(NS(XID,"X_SCP_2"), "Ordinate", names(XT)),
    plotOutput(NS(XID,"Y_SCP"))
  )
}
scatterplotServer <- function(XID,XT) {
	moduleServer(XID, function(input, output, session) {
		XV <- reactive(XT[[input$X_SCP_1]])
		YV <- reactive(XT[[input$X_SCP_2]])
		output$Y_SCP <- renderPlot({
			plot(XV(), YV(), main="Scatterplot:", 
			xlab=input$X_SCP_1, ylab=input$X_SCP_2) 
		})
	})
}
################################################################
boxplotUI <- function(XID,XT) {
	tagList(
		selectInput(NS(XID,"X_BOX_1"), "Abscissa", names(XT)),
		selectInput(NS(XID,"X_BOX_2"), "Ordinate", names(XT)),
		plotOutput(NS(XID,"Y_BOX"))
	)
}
boxplotServer <- function(XID,XT) {
	moduleServer(XID, function(input, output, session) {
		XV <- reactive(XT[[input$X_BOX_1]])
		YV <- reactive(XT[[input$X_BOX_2]])
		output$Y_BOX <- renderPlot({
			boxplot(XV(),YV(),main="Boxplot:",
			xlab=input$X_SCP_1, ylab=input$X_SCP_2)
		})
	})
}
################################################################
densityUI <- function(XID,XT) {
	tagList(
		selectInput(NS(XID,"X_DEN_1"), "Abscissa", names(XT)),
		plotOutput(NS(XID,"Y_DEN"))
	)
}
densityServer <- function(XID,XT) {
	moduleServer(XID, function(input, output, session) {
		XV <- reactive(XT[[input$X_DEN_1]])
		output$Y_DEN <- renderPlot({
			plot(density(XV()),main="Density plot:",
			xlab=input$X_DEN_1)
		})
	})
}
################################################################
histogramUI <- function(XID,XT) {
	tagList(
		selectInput(NS(XID,"X_HIST"), "Variable", names(XT)),
		numericInput(NS(XID,"X_BIN"), "bins", 10, min = 1),
		plotOutput(NS(XID,"Y_HIST"))
	)
}
histogramServer <- function(id, df) {
	moduleServer(id, function(input, output, session) {
	XV <- reactive(df[[input$X_HIST]])
	output$Y_HIST <- renderPlot({
		hist(XV(), breaks = input$X_BIN, main = "Histogram:",xlab=input$X_HIST)
	})
  })
}
################################################################

################################################################
ui <- fluidPage(################################################
	selectInput("X_GRAPH_TYPE",
		label = "Choose type of graph",
		choices = XV_GRAPH_CHOICE,
		selected = "Scatterplot"
	),
	conditionalPanel(condition = ("input.X_GRAPH_TYPE == 'Scatterplot'"),
		scatterplotUI("mtcars",mtcars)
	),
	conditionalPanel(condition = ("input.X_GRAPH_TYPE == 'Boxplot'"),
		boxplotUI("mtcars",mtcars)
	),
	conditionalPanel(condition = ("input.X_GRAPH_TYPE == 'Density plot'"),
		densityUI("mtcars",mtcars)
	),
	conditionalPanel(condition = ("input.X_GRAPH_TYPE == 'Histogram'"),
		histogramUI("mtcars", mtcars)
	)
)###############################################################
################################################################

################################################################
server <- function(input, output, session) {####################
	scatterplotServer("mtcars",mtcars)
	boxplotServer("mtcars",mtcars)
	densityServer("mtcars",mtcars)
	histogramServer("mtcars",mtcars)
}###############################################################
################################################################
shinyApp(ui = ui, server = server)