# Example of trying out different regression lines and viewing and measuring
# their residuals

# to install mowateR, try this line
# install.packages("/path/to/file/mowateR_0.2.tar", repos = NULL, type = "source")

library("shiny")
library("mowateR")
data("eml")

# only use a small amount of the data, so that things render quickly and we can
# actually see the residual lines
eml_small <- eml[1:200,]

# We would like the line to go through a fixed point that is where the data
# are (when the offset is zero). Otherwise picking the angle and offset
# is pretty tricky to do by hand. So compute the average x & y of the
# observations, and use them as that fixed point. These don't change (since the
# dataset isn't changing), so we only compute them once, globally.
MU_Y <- mean(eml_small$DO)
MU_X <- mean(eml_small$pH)

ui <- fillPage(
    headerPanel('Fitting EML Data with a Line'),
    
    mainPanel(plotOutput('plot1'), width=12),

    sidebarPanel(
      sliderInput(inputId="theta", label="Angle:", min=-89, max=89, value=0),
      sliderInput(inputId="offset", label="Vertical Offset:", min=-4, max=4, value=0, step=0.1),
      h3(textOutput("line_formula", inline=T)),
      h3("Standard deviation of residuals:", textOutput("std", inline=T)),
      width=12
    )
)

server <- function(input, output) {
    # Given the angle (in degrees) and offset, find out the slope and intercept.
    b <- reactive({ tan(input$theta * pi / 180) })
    a <- reactive({ MU_Y - b() * MU_X + input$offset })

    # record these for rendering the formula of the line
    output$line_formula <- renderText({
        paste("Formula: DO ~ ", round(b(), 2), " * pH + ", round(a(), 2))
    })

    output$plot1 <- renderPlot({
        # plot the data and line
        plot(DO ~ pH, data=eml_small, 
             xlab="pH", ylab="Dissolved Oxygen (mg/L)",
             main="Eagle Mountain Lake",
             cex.lab=1.5, cex=2, pch=19, col=rgb(0, 0, 0, 0.5))

        # we created "reactive" values a and b above (outside this reactive
        # chunk); we have to treat them like functions (aka "closures") here,
        # with a() and b().
        abline(a=a(), b=b(), col="red", lwd=3)

        # calculate and plot the residuals
        y0vals <- a() + b() * eml_small$pH
        y1vals <- eml_small$DO # actual values
        xvals <- eml_small$pH # x values (same for prediction and actual)

        # draw the residuals as vertical lines
        segments(xvals, y0vals, xvals, y1vals, col="red", lwd=1)

        # calculate the sum of squared residuals, and use them to find the
        # standard deviation (the square-root of average of the squared
        # residuals) 
        ssr <- sum((y0vals - y1vals) ^ 2)
        std <- sqrt(mean(ssr))
        output$std <- renderText({
            # use paste() to force the number to be a string
            paste(round(std, 3))
        })
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
