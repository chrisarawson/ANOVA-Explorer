library(shiny)

shinyUI(fluidPage(
  
  titlePanel("ANOVA Explorer"),
  
  fluidRow(
    column(3,
           wellPanel(
             fluidRow(
               column(6,
                      h3("Group 1")),
               column(6,
                      sliderInput("sampSize1","Sample Size",min=4,max=100,value=10))
             ),
             fluidRow(
               column(6,
                      sliderInput("mean1","Mean",min=10,max=100,value=10)),
               column(6,
                      sliderInput("sd1", "Standard Deviation",min=1,max=50,value=5))
             ),
             hr(),
             fluidRow(
               column(6,
                      h3("Group 2")),
               column(6,
                      sliderInput("sampSize2","Sample Size",min=4,max=100,value=10))
             ),
             fluidRow(
               column(6,
                      sliderInput("mean2","Mean",min=10,max=100,value=40)),
               column(6,
                      sliderInput("sd2", "Standard Deviation",min=1,max=50,value=5))
             ),
             hr(),
             fluidRow(
               column(6,
                      h3("Group 3")),
               column(6,
                      sliderInput("sampSize3","Sample Size",min=4,max=100,value=10))
             ),
             fluidRow(
               column(6,
                      sliderInput("mean3","Mean",min=10,max=100,value=80)),
               column(6,
                      sliderInput("sd3", "Standard Deviation",min=1,max=50,value=5))
             ),
             actionButton("reRandom","Click for another random set of data"),
             textOutput("mySeed"))),
             
    column(5,
           tabsetPanel(
             tabPanel("Distributions",plotOutput("DistSSwg"),plotOutput("DistSSbg")),
             tabPanel("Dot Charts",plotOutput("DotSSwg"),plotOutput("DotSSbg")),
             tabPanel("Explanations",
                      withMathJax(),
                      h3("Changing population means"),
                      h4("Residual (Within Groups)"),
                      p("The distance from each datapoint and the group mean has not changed. Neither 
has the number of degrees of freedom (total or residual). Therefore, there is no effect on the 
residual SS or MS and therefore the denominator of the F-ratio."),
                      h4("Group (Between Groups)"),
                      p("The distance from the group means to the global mean (The mean of all the data
points has changed. This will change the Group SS and therefore the group MS and the numerator of the 
F-ratio. If one mean is moved farther away from the global mean then the SS Group will increase causing
the SS Group to increase. This will increase the MS group and the numerator of the F-ratio. The F-ratio
will therefore be higher and the p-value will be lower indicating that the groups are more different."),
                      br(),
                      h3("Changing population standard deviations"),
                      h4("Residual (Within Groups)"),
                      p("There will be a change in the distance from each datapoint to the population mean which will result in
                      a change in the SS residual. An increase in population standard deviation will cause
                      an increase in the SS residual which will increase the MS residual. That will increase
                      the denominator of the F-ratio. The F-ratio will therefore be lower and the p-value will
                      be higher indicating that the probablity that the null hypothesis is the true result is
                      higher. More accurately, the probability of incorrectly rejecting the null hypothesis is decreased"),
                      h4("Group (Between Groups)"),
                      p("The distances between the population means and the global mean will not change much since 
the data are still approxmiately equally distributed around the population mean (by definition). The 
                      population means stay the same"),
                      br(),
                      h3("Changing sample size"),
                      h4("Residual (Within Groups)"),
                      p("Changing the number of overall sampling points affects the changes the SS residual as it changes
                        the number of individual measurements that contribute to it. It also changes the degrees of
                        freedom. An increase of 5 sampling points adds 5 measurements to the SS residual and will increases
the number of degrees of freedom by 5. The overall result is that the MS residual remains about the same and therefore
                        little impact on the F-ratio"),
                      h4("Group (Between Groups)"),
                      p("A change in the number of sampling points does not change the between groups degrees of 
                        freedom. However it increases the between group SS in a multiplicative way. Further,
                        changes in the sample sizes in groups further away from the overall mean will
                        change the between groups SS faster than changes of the same magnitude in groups
                        whose mean is closer to the overall mean. To see why this is the case see the 
                        formula for how this is calculed in the general ANOVA table below. 
                        An increase in the sample size will increase the between groups SS. The overall impact
                        of an increase in the sample size will be an increase in the between groups SS and with
                        no change to the between groups SS this will increase the between groups MS. Hence
                        there will be an increase in the F-ratio."),
                      br(),
                      h3("ANOVA Calculations"),
                      p("The data are fitted to a linear model of the form
                        $$y_{ij} = \\mu + \\alpha_i + \\epsilon_{ij}$$"),
                      p("\\(\\mu\\) is the overall mean and is a constant for all observations"),
                      p("\\(\\alpha_i\\) is the effect of treatment i. For our purposes (fixed factors) it is the difference
                        between the group mean and the overall mean (\\(\\mu_i - \\mu\\))"),
                      p("\\(\\epsilon_{ij}\\) is the random error associated with each measurement. It equates to the 
                        difference between the measurement and the group mean (\\(y_{ij} - \\mu_i\\))"),
                      p("Remember these important components of the ANOVA table (see below)"),
                      p("1. $$MS_{i} = \\frac{SS_{i}}{df_{i}}$$ So if the SS is increased the MS is increased. If 
                               the df is increased the MS decreases"),
                      p("2. $$F_{df1,df2} = \\frac{MS_{regression}}{MS_{residual}}$$ So if the MS of the regression increases F increases and
                               if the MS of the residual increases F decreases"),
                      p("3. The higher the **F-ratio**, the lower the **p** -value"),
                      p("The null hypothesis tested is that all means of all treatment groups are equal
                        $$\\mu_1 = \\mu_2 = ..... = \\mu_i$$"),
                      br(),
                      h3("General ANOVA table"),
                      HTML("
                        <table border='1' style='width:100%'>
                           <tr>
                           <th>Source</th>
                           <th>SS</th>
                           <th>df</th>
                           <th>MS</th>
                           <th>F</th>
                           </tr>
                           <tr>
                           <td>Between Groups</td>
                           <td>$$\\sum_{i=1}^{p} n_i (\\bar{y_i} - \\hat{y})^2$$</td>
                           <td>$$p - 1$$</td>
                           <td>$$\\frac{SS_{between groups}}{df_{between groups}}$$</td>
                           <td>$$\\frac{MS_{between groups}}{MS_{residual}}$$</td>
                           </tr>
                           <tr>
                           <td>Residual</td>
                           <td>$$\\sum_{i=1}^{p} \\sum_{j=1}^{n} (y_{ij} - \\bar{y_i})^2$$</td>
                           <td>$$\\sum_{i=1}^{p} n_i - p$$</td>
                           <td>$$\\frac{SS_{residual}}{df_{residual}}$$</td>
                           <td></td>
                           </tr>
                           <tr>
                           <td>Total</td>
                           <td>$$\\sum_{i=1}^{p} \\sum_{j=1}^{n} (y_{ij} - \\bar{y})^2$$</td>
                           <td>$$\\sum_{i=1}^{p} n_i - 1$$</td>
                           <td></td>
                           <td></td>
                           </tr>
                           
                           </table>"
                      ),
                      br(),
                      h3("Data Source"),
p("The datapoints for each population are drawn from a normally distributed population with the mean
and standard deviation as specified by the user. The dataset can be re-drawn by clicking the button
on the lower left hand side of the side panel.")),
                tabPanel("Try this!",
                         tags$i("The following all assume the original starting parameters.
                           To return to the starting parameters hit refresh in your browser."),
                         h3("Effect of changing means"),
                         tags$ol(
                           tags$li("Move all means to 40"),
                           tags$li("According to the ANOVA results, are the groups different?"),
                           tags$li("Increase one of the populations to 50."),
                           tags$li("Has the MS residual changed? Why or why not?"),
                           tags$li("Has the MS group changed? Why or why not"),
                           tags$li("Are the groups different now?")),
                         h3("Effect of changing standard devitation"),
                         tags$ol(
                           tags$li("Set 2 populations to have a mean of 40 and the other with a mean of 50."),
                           tags$li("Are the groups different?"),
                           tags$li("Increase the standard deviation of the group with a mean of 50 to 7."),
                           tags$li("What has happened to the MS residual? Why?"),
                           tags$li("Has the df residual changed? Why"),
                           tags$li("Are the groups different now?")
                         ),
                         h3("Effect of changing sample size"),
                         tags$ol(
                           tags$li("Set the population means to 40, 40 and 43"),
                           tags$li("Are the groups different?"),
                           tags$li("Increase the sample size from each population to 20"),
                           tags$li("What has happened to the F-value? Why?"),
                           tags$li("Are the groups different?")
                           ),
                         h3("What parameters increase the p-value"),
                         tags$ol(
                           tags$li("Set the population means to 35, 40 and 40."),
                           tags$li("The ANOVA table should indicate these are different groups."),
                           tags$li("What can you do to the Sample size to detect a difference?"),
                           tags$li("What can you do to the accuracy of your measurements (standard deviation
                                   to detect a difference?")
                         ),
                         h3("Effect of taking random samples"),
                         tags$ol(
                           tags$li("Set all population means to 40"),
                           tags$li("Would you expect ANOVA to detect a difference?"),
                           tags$li("Does ANOVA detect a difference?"),
                           tags$li("Click the button that resamples the populations"),
                           tags$li("Is the p-value the same?"),
                           tags$li("How many times to you have to resample before you get an incorrect
                                   detection of a significant difference?"),
                           tags$li("How do you think you could increase this number?"),
                           tags$li("Try again adjusting the parameters you suggested above")
                         ),
                         h3("Guess the result!"),
                         tags$ol(
                           tags$li("Set all population means to 40"),
                           tags$li("Click off the ANOVA output"),
                           tags$li("Move one of the population means away from the others until you think
                                   the groups are significantly different."),
                           tags$li("Click the ANOVA results back on. How did you go?"),
                           tags$li("Have another go with different sample sizes and standard deviations.
                                   Do larger sample sizes and smaller standard deviations make it easier
                                   to guess at the result?")
                         )
                         )
           )),
    column(4,
           wellPanel(
             fluidRow(
               column(4,
                      checkboxInput("showAnova","Show Anova Results", TRUE)),
               column(4,
                      checkboxInput("showQQnorm","Show Normality Check", TRUE)),
               column(4,
                      checkboxInput("showFitRes","Show Homoscedascity Check", TRUE))
             ),
             h3("Anova Results"),
             verbatimTextOutput("aovSummary"),
             plotOutput("normPlot",height=200),
             textOutput("Shapiro"),
             plotOutput("resFitPlot",height=200),
             htmlOutput("Bartletts")
           )
  ))))