library(sp)
library(rgdal)
library(foreach)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(shiny)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    # withMathJax(),
    # tags$div(HTML("<script type='text/x-mathjax-config'>
    #             MathJax.Hub.Config({
    #             tex2jax: {inlineMath: [['$','$']}
    #             });
    #             </script>
    #             ")),
    # Application title
    titlePanel("Herding Behaviour"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          withMathJax(helpText("$$s_{i}(t+1)=sign \\bigg(K \\sum_{k \\in N(i)} s_{k}(t)+ \\sigma \\epsilon_{i}(t) + G  \\bigg)$$")),
            sliderInput("dim",
                        "Grid Size:",
                        min = 20,
                        max = 500,
                        value = 50),
            sliderInput("K","K:",0.1,0.9,0.4),
            sliderInput("sig","Sigma:",0.1,0.9,0.4),
            sliderInput("G","G:",min=-1,max=1,value=0,step=0.05),
            sliderInput("iter","Iteration:",1,12,4),
            actionButton("run","Run")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", width = 1600, height = 6000)
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({

        
        
        if(input$run == 0)
            return()
        
        isolate({
        
            
            pl <- list()
            
            set.seed(0)
            dim=input$dim
            x <- raster(ncol=dim, nrow=dim)
            values(x) <- sign(rnorm(ncell(x),mean=0,sd=0.3))
            y <- raster(x)
            values(y) <- sign(rnorm(ncell(x),mean=0,sd=0.3))
            ei <- rnorm(ncell(x))
            
            K=input$K
            sig=input$sig
            G=input$G
            
            
            for(i in 1:input$iter){
            

            v <- getValuesFocal(x, 1, nrow(x), c(3,3))
            v <- v[, c(2,4,6,8)]
            a <- rowSums(v, na.rm=TRUE)
            values(x) <- sign(K*a+sig*ei+G)
            
            
            x_point <- rasterToPoints(x)
            x_df <- data.frame(x_point)
            x_df$cuts=cut(x_df$layer,breaks=c(-2,0.9,1,2))
            
            pl[[i]] <- ggplot(data=x_df)+
                geom_tile(aes(x=x,y=y,fill=cuts)) +
                scale_fill_manual(values = c("red", "green")) +
                coord_equal() +
                theme_bw()+theme(panel.grid.major = element_blank(),
                                 legend.title = element_blank(), 
                                 axis.title = element_blank(),
                                 legend.position = "none")

            
            }
            
            n.plot <- length(pl)
            
            grid.arrange(grobs=pl, ncol=1 )
            
            
            
        })

        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
