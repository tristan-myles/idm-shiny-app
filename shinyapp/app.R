# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

# *===========================================================================*
# *==================================== NB ===================================*
# *===========================================================================*
######## PLEASE SETWD TO LOCATION OF THE FOLDER THIS SCRIPT IS IN #############
pwd = "/home/tristan/Documents/uct/repos/idm-shiny-app/shinyapp/"
# *===========================================================================*
# *==================================== NB ===================================*
# *===========================================================================*
library(deSolve)
library(shiny)


#Set/change working directory
# setwd("/home/tristan/Documents/uct/repos/masters-assignments/idm/")
## app.R ##
library(shinythemes)



# *===========================================================================*
costing_sliders_1 <- function(){
    column(2, align="center",
           br(),
           textInput(inputId="distCost",label="Cost of condom distribution", value=1),
           textInput(inputId="condomCost",label="Cost of condoms", value=1),
           textInput(inputId="trtCost",label="Cost of treatment", value=1),
           textInput(inputId="eduCost",label="Cost of education", value=1),
           textInput(inputId="wage",label="Hourly wage", value=1)
    )
}

costing_sliders_2 <- function(){
    column(2, align="center",offset=0, style='padding:3px;',
           br(),
          sliderInput(inputId="proportionBuy",label="Proportion who buy condoms",
                       value=20, min=0, max=100, step=1),
           sliderInput(inputId="proportionEducate",label="Proportion of men who are educated",
                       value=20, min=0, max=100, step=1),
          br(),
          br(),
          checkboxInput(inputId="boolPatient", label="Show Patient Costs", value=TRUE),
          checkboxInput(inputId="boolSociety", label="Show Societal Costs", value=TRUE),
          checkboxInput(inputId="boolProvider", label="Show Provider Costs", value=TRUE)
    )
}
# *===========================================================================*

# *================================ Slider Rows ==============================*
slider_tings <- function(){
    fluidRow(
        column(3, align="center",
               numericInput(inputId = "initP", label = "Initial population size", 
                              value =36500, min = 100, max=1000000, step=100),
               sliderInput(
                   inputId="lifExp", label = "Average number of deaths p.a.",
                   value=100, min=1, max=1000, step=10
               ), # mu_d
               sliderInput(
                   inputId="birthRt", label = "Average number of births p.a.",
                   value=100, min=1, max=1000, step=10
               )# mu_b
        ),
        column(3, align="center",
               
               sliderInput(
                   inputId="betaMW", label = "Contact rate of men with women",
                   value=0.5, min=0, max=1, step=0.05
               ),
               sliderInput(
                   inputId="betaMM", label = "Contact rate of men with men",
                   value=0.5, min=0, max=1, step=0.05
               ),
               sliderInput(
                   inputId="betaWM", label = "Contact rate of women with men",
                   value=0.5, min=0, max=1, step=0.05
               ), sliderInput(
                   inputId="betaWW", label = "Contact rate of women with women",
                   value=0.5, min=0, max=1, step=0.05
               )# beta matrix
        ),
        column(3, align="center",
               
               sliderInput(
                   inputId="natRec", label = "Average natural recovery time (days)",
                   value=30, min=1, max=700, step=1
               ), # gamma
               sliderInput(
                   inputId="treatRec", label = "Average treatment recovery time (days)",
                   value=14, min=1, max=700, step=1
               ), # r1
               sliderInput(
                   inputId="imDur", label = "Duration of immunity (days)",
                   value=30, min=1, max=700, step=1
               ), # rho
               sliderInput(
                   inputId="timeTreat", label = "Time to seek to treatment (days)",
                   value=7, min=1, max=700, step=1
               ) # tau
        ),
        column(3, align="center", 
               sliderInput(
                   inputId="pAbsTreat", label = "Percentage of abstinence during treatment",
                   value=90, min=0, max=100, step=1
               ), # q1
               sliderInput(
                   inputId="pNatRec", label = "Percentage of natural recovery",
                   value=20, min=0, max=100, step=1
               ), #q2
               sliderInput(
                   inputId="pWomenMen", label = "Percentage of women in population",
                   value=50, min=0, max=100, step=1
               ), #pi
               sliderInput(
                   inputId="pCondoms", label = "Percentage of condom usage", 
                   value=50, min=0, max=100, step=10) # alpha
        )#
    ) # End Fluid Row
}
# *===========================================================================*

model_page <- function(){
    fluidRow(
        fluidRow(
# *================================ Main Panel ===============================*
            fluidRow(column(12, align="center",style='margin-bottom:30px;
                            padding-top: 10px;
                            padding-bottom: 10px;
                            padding-left: 50px;
                            padding-right: 50px;',
                       tabsetPanel(id="panels",
                           tabPanel("Population",fluidRow(column(4),
                                                          column(8,align="center",
                                    plotOutput(outputId = "Plot", width = "80%"),
                                    style='margin-bottom:30px;border-left:1px solid; padding: 5px;'
                                    )
                                    )),
                           tabPanel("Cost", 
                                    fluidRow(
                                        costing_sliders_1(),
                                        costing_sliders_2(),
                                        column(8, align="center",style='margin-bottom:30px;border-left:1px solid; padding: 5px;',
                                               tabsetPanel(id="panels",
                                                           tabPanel("Table", tableOutput(outputId= "costTable")),
                                                           tabPanel("Tornado", plotOutput(outputId = "costPlot", width = "85%"))))
                           )
                           )
                       )
            )# End upper Col
                           )# End upper fluid row
# *===========================================================================*
        ),# end lower fluid row
        hr(),
# *=============================== Lower Sliders =============================*
        slider_tings()
# *===========================================================================*
    ) # end upper fluid row
}




# Define UI for application that displays my SIRSV output
ui <-  fluidPage(
    theme = shinytheme("flatly"),
    navbarPage("Gonorrhea",
               tabPanel("Background",
                        includeHTML(paste(pwd,"include.html", sep=""))),
               tabPanel("Model",model_page())
    )
)# end UI

# *===========================================================================*
### Setup of the processes to be run in the server happen outside both the ui and
### server functions

# Set the start and end time for the model simulation
## vector of timesteps
times <- seq(0, 250, by= 1)

# Set up a function to solve the equations
SITR <- function(t, x, parms)  {
    with(as.list(c(parms, x)), {
        
        N_w = S_w + I_w + T_w + R_w
        N_m = S_m + I_m + T_m + R_m
        N = N_w + N_m
        
        lambda_w = (1-alpha)*beta_wm*(I_m/N_m) + beta_ww*(I_w/N_w)
        
        dS_w = -lambda_w*S_w + rho*R_w - mu_d*S_w + pie*mu_b*N
        dI_w = lambda_w*S_w - q2*gamma*I_w - (1-q1-q2)*r*I_w - q1*tau*I_w - mu_d*I_w 
        dT_w = q1*tau*I_w - r1*T_w - mu_d*T_w
        dR_w = q2*gamma*I_w + (1-q1-q2)*r*I_w + r1*T_w - rho*R_w - mu_d*R_w
        
        lambda_m = (1-alpha)*(beta_mm*(I_m/N_m) + beta_mw*(I_w/N_w))
        
        dS_m = -lambda_m*S_m + rho*R_m - mu_d*S_m + (1-pie)*mu_b*N
        dI_m = lambda_m*S_m - q2*gamma*I_m - (1-q1-q2)*r*I_m - q1*tau*I_m - mu_d*I_m
        dT_m = q1*tau*I_m - r1*T_m - mu_d*T_m    
        dR_m = q2*gamma*I_m + (1-q1-q2)*r*I_m + r1*T_m - rho*R_m - mu_d*R_m
        
        output <- c(dS_w, dI_w, dT_w, dR_w, dS_m, dI_m, dT_m, dR_m)
        list(output)
    })
} 

# *=============================== Costings Code =============================*
provider_cost = function(delta1, delta2, eduCost, distCost, condomCost, trtCost, N_m, alpha=0) {
#### No trtCost
    education = eduCost*delta2*alpha*N_m 
    condoms = (condomCost+distCost)*(1-delta1)*alpha*N_m 
    return(c(education, condoms))
}
patient_cost = function(delta1, N_m, alpha=0) {
    delta1*alpha*N_m
}
society_cost = function(wage, delta2, q1, q2, N, trtDays, trtCost, natRecDays, I_w, I_m, N_w, N_m, beta_mm, beta_mw, beta_wm, beta_ww, alpha=0) {
    lambda_w = (1-alpha)*beta_wm*(I_m/N_m) + beta_ww*(I_w/N_w)
    lambda_m = (1-alpha)*(beta_mm*(I_m/N_m) + beta_mw*(I_w/N_w))
    treatment = trtCost*(1-q2)*(lambda_w*N_w + lambda_m*N_m)
    oppcost_of_condoms = 0.5*wage*(delta2 + 1)*alpha*N_m
    oppcost_of_treatment = 8*(1-q2)*trtDays*wage*(lambda_w*N_w + lambda_m*N_m)
    oppcost_of_nat_recovery = 8*q1*natRecDays*wage*(lambda_w*N_w + lambda_m*N_m)
    return(c(treatment, oppcost_of_treatment, oppcost_of_condoms, oppcost_of_nat_recovery))
}
denominator = function(I_w, N_w, I_m, N_m, beta_mm, beta_mw, beta_wm, beta_ww, alpha=0) {
    lambda_w = (1-alpha)*beta_wm*(I_m/N_m) + beta_ww*(I_w/N_w)
    lambda_m = (1-alpha)*(beta_mm*(I_m/N_m) + beta_mw*(I_w/N_w))
    (1-lambda_w)*N_w + (1-lambda_m)*N_m
}
combine_costs = function(delta1, delta2, eduCost, distCost, condomCost, trtCost, N_m, N_w, N,
                         trtDays, natRecDays, q1, q2, wage,
                         I_w, I_m, beta_mm, beta_mw, beta_wm, beta_ww, alpha) {
    provider_with = provider_cost(delta1, delta2, eduCost, distCost, condomCost, trtCost, N_m, alpha)
    provider_without = provider_cost(delta1, delta2, eduCost, distCost, condomCost, trtCost, N_m)
    patient_with = patient_cost(delta1, N_m, alpha)
    patient_without = patient_cost(delta1, N_m)
    society_with = society_cost(wage, delta2, q1, q2, N, trtDays, trtCost, natRecDays, I_w, I_m, N_w, N_m, beta_mm, beta_mw, beta_wm, beta_ww, alpha)
    society_without = society_cost(wage, delta2, q1, q2, N, trtDays, trtCost, natRecDays, I_w, I_m, N_w, N_m, beta_mm, beta_mw, beta_wm, beta_ww)
    
    with = c(provider_with, sum(provider_with), patient_with, sum(patient_with), society_with, sum(society_with))
    without = c(provider_without, sum(provider_without), patient_without, sum(patient_without), society_without, sum(society_without)) 
    diff = with - without
    den = denominator(I_w, N_w, I_m, N_m, beta_mm, beta_mw, beta_wm, beta_ww, alpha) - denominator(I_w, N_w, I_m, N_m, beta_mm, beta_mw, beta_wm, beta_ww)
    saved = diff/den
    tab = rbind(with, without, diff, saved)
    cols = c("Education", "Condom supply and distribution", "Total",
             "Condom purchases", "Total",
             "Treatment", "Time spent buying condoms", "Time spent in treatment", "Time spent recovering naturally", "Total")
    rows = c(" ","With condom use", "Without condom use", "Difference", "Money saved due to condom use")
    
    tab = round(tab, 2)
    colnames(tab) = c("Provider", " ", " ", "Patient", " ", "Society", " ", " ", " ", " ")
    tab = rbind(cols,tab)
    rownames(tab) = rows
    tab
}
# *===========================================================================*

# Define server logic required to run model and plot outputs
server <- function(input, output) {
    
    #Create parameters vector - reactive nature
    parametersR <- reactive(c(
        # Contact Rates
        beta_wm=input$betaWM,
        beta_ww = input$betaWW,
        beta_mm = input$betaMM,
        beta_mw = input$betaMW,
        
        # Rates
        gamma = 1/input$natRec,
        mu_d = 1/input$lifExp,
        mu_b = 1/input$birthRt,

        tau = 1/input$timeTreat,
        r1 = 1/input$treatRec,
        rho = 1/input$imDur,
        r = 1/(input$treatRec + input$timeTreat),
        
        # Proportions
        pie = input$pWomenMen/100,
        q1 = input$pAbsTreat/100,
        q2 = input$pNatRec/100,
        alpha=input$pCondoms/100)
    )
    #Create reactive plot function
    output$Plot <- renderPlot({
        #Define intital values  
        initP <- reactive(input$initP)
        initPw <- (input$pWomenMen/100) * initP()
        initPm <- initP() - initPw
        
        initIw <- 1
        initTw <- 0
        initRw <- 0
        initSw <- initPw - initIw - initRw
        
        initIm <- 1
        initTm <- 0
        initRm <- 0
        initSm <- initPm - initIm - initRm
        
        start<-c(S_w=initSw, I_w=initIw, T_w=initTw, R_w=initRw,
                 S_m=initSm, I_m=initIm, T_m=initTm, R_m=initRm) 
        
        run_d<-reactive(ode(times=times, y=start, func=SITR, parms=parametersR()))
        run <- run_d()
        
        # Plotting the function
        plot(run[,2], col="red", type="l",
             main=bquote(I[2]~"="~T[2]~"= 0,"~S[2]~"= 1000"), ylim=c(0,max(run[,2:9])),
             ylab="Number of people", xlab="Day")
        lines(run[,3], col="blue")
        lines(run[,4], col="orange")
        lines(run[,5], col="green")
        lines(run[,6], col="red", lty=14)
        lines(run[,7], col="blue", lty=14)
        lines(run[,8], col="orange", lty=14)
        lines(run[,9], col="green", lty=14)
        legend("topright",legend=c("S1", "I1", "T1", "R1",
                                   "S2", "I2", "T2", "R2"), col=c("red", "blue", "orange", "green"),
               lty=c(1,1,1,1,14,14,14,14))
    })


# *================================= Costings ================================*
    delta1 = reactive(input$proportionBuy)
    delta2 = reactive(input$proportionEducate)
    eduCost = reactive(input$eduCost)
    distCost = reactive(input$distCost)
    condomCost = reactive(input$condomCost)
    trtCost = reactive(input$trtCost)
    trtDays=reactive(input$timeTreat)
    natRecDays= reactive(input$natRec)
    q1 = reactive(input$pAbsTreat/100)
    q2 = reactive(input$pNatRec/100)
    wage=reactive(input$wage)
    beta_wm= reactive(input$betaWM)
    beta_ww = reactive(input$betaWW)
    beta_mm = reactive(input$betaMM)
    beta_mw = reactive(input$betaMW)
    alpha= reactive(input$pCondoms/100)

    showProvider=reactive(input$boolProvider)
    showPatient=reactive(input$boolPatient)
    showSociety=reactive(input$boolSociety)

    provider_cols = c(1,2)
    patient_cols = c(4)
    society_cols = c(6,7,8,9)

    # Create a table
    output$costTable <- renderTable(striped = TRUE, bordered = TRUE,  
                                    hover = TRUE, rownames = TRUE,width = "90%",
        {
            initP <- reactive(input$initP)
            initPw <- (input$pWomenMen/100) * initP()
            initPm <- initP() - initPw
            
            initIw <- 1
            initTw <- 0
            initRw <- 0
            initSw <- initPw - initIw - initRw
            
            initIm <- 1
            initTm <- 0
            initRm <- 0
            initSm <- initPm - initIm - initRm
            
            start<-c(S_w=initSw, I_w=initIw, T_w=initTw, R_w=initRw,
                     S_m=initSm, I_m=initIm, T_m=initTm, R_m=initRm) 
            
            run_d<-reactive(ode(times=times, y=start, func=SITR, parms=parametersR()))
            run <- run_d()
            end_row <- NROW(run)

            table_out<- combine_costs(
                delta1=delta1()/100,
                delta2=delta2()/100,
                eduCost=as.double(eduCost()),
                distCost=as.double(distCost()),
                condomCost=as.double(condomCost()),
                trtCost=as.double(trtCost()),
                N_w=run[end_row,2]+run[end_row,3]+run[end_row,4]+run[end_row,5],
                N_m=run[end_row,6]+run[end_row,7]+run[end_row,8]+run[end_row,9],
                N=N_w+N_m,
                trtDays=trtDays(),
                natRecDays = natRecDays(),
                q1 = q1(),
                q2 = q2(),
                wage=as.double(wage()),
                I_w=run[end_row,3],
                I_m=run[end_row,8],
                beta_wm = beta_wm(),
                beta_ww = beta_ww(),
                beta_mm = beta_mm(),
                beta_mw = beta_mw(),
                alpha = alpha()
            )

            if(!showProvider()){
                table_output_1<-NULL
            }else{
                table_output_1<-table_out[,c(provider_cols,3)]
            }
            if(!showPatient()){
                table_output_2<-NULL
            }else{
                table_output_2<-table_out[,c(patient_cols,5)]
            }
            if(!showSociety()){
                table_output_3<-NULL
            }else{
                table_output_3<-table_out[,c(society_cols,10)]
                
            }
            cbind(table_output_1,table_output_2,table_output_3)
        }
        ) # End of renderTable  

    # Create a plot
    output$costPlot <- renderPlot(
        {
            initP <- reactive(input$initP)
            initPw <- (input$pWomenMen/100) * initP()
            initPm <- initP() - initPw
            
            initIw <- 1
            initTw <- 0
            initRw <- 0
            initSw <- initPw - initIw - initRw
            
            initIm <- 1
            initTm <- 0
            initRm <- 0
            initSm <- initPm - initIm - initRm
            
            start<-c(S_w=initSw, I_w=initIw, T_w=initTw, R_w=initRw,
                     S_m=initSm, I_m=initIm, T_m=initTm, R_m=initRm) 
            
            run_d<-reactive(ode(times=times, y=start, func=SITR, parms=parametersR()))
            run <- run_d()
            end_row <- NROW(run)

           table_out<-combine_costs(
               delta1=delta1()/100,
               delta2=delta2()/100,
               eduCost=as.double(eduCost()),
               distCost=as.double(distCost()),
               condomCost=as.double(condomCost()),
               trtCost=as.double(trtCost()),
               N_w=run[end_row,2]+run[end_row,3]+run[end_row,4]+run[end_row,5],
               N_m=run[end_row,6]+run[end_row,7]+run[end_row,8]+run[end_row,9],
               N=N_w+N_m,
               trtDays=trtDays(),
               natRecDays = natRecDays(),
               q1 = q1(),
               q2 = q2(),
               wage=as.double(wage()),
               I_w=run[end_row,3],
               I_m=run[end_row,8],
               beta_wm = beta_wm(),
               beta_ww = beta_ww(),
               beta_mm = beta_mm(),
               beta_mw = beta_mw(),
               alpha = alpha()
           )

           if(!showProvider()){
               table_output_1<-NULL
               col_names_1 <- NULL
           }else{
               table_output_1<-table_out[,provider_cols]
               col_names_1 <- table_out[1,provider_cols]
           }
           if(!showPatient()){
               table_output_2 <- NULL
               col_names_2 <- NULL
           }else{
               table_output_2<-table_out[,patient_cols]
               col_names_2 <- table_out[1,patient_cols]
           }
           if(!showSociety()){
               table_output_3<-NULL
               col_names_3 <- NULL
           }else{
               table_output_3<-table_out[,society_cols]
               col_names_3 <- table_out[1,society_cols]
           }

           table_out <- cbind(table_output_1,table_output_2,table_output_3)
           col_names <- c(col_names_1,col_names_2,col_names_3)
           if(is.null(col_names)){
               NULL
           }
           else{
           table_out <- t(apply(table_out[-1,], MARGIN=1, FUN=as.double))
           num_cols <- NCOL(table_out)
           num_rows <- NROW(table_out)
           
           temp_pos <- apply(table_out, MARGIN=1, function(x) ifelse(x>=0,x,0))
           temp_neg <- apply(table_out, MARGIN=1, function(x) ifelse(x<0,x,0))
           new_table <- cbind(temp_pos[,num_rows], temp_neg[,num_rows])
           table_out <- t(as.matrix(new_table, ncol=num_cols))

           colnames(table_out) <- col_names

           x_axis <- range(table_out)

           par(mar=c(5.1,14.1,4.1,2.1))
           barplot(table_out, horiz = T, las=1, xaxt='n', ylab = '',
                   beside=T, col=c('springgreen','indianred2'), main="Costs in $")
           axis(1, at=pretty(x_axis),  lab=paste0(pretty(x_axis),"$"), las=TRUE)
           }
        }
    ) # End of renderPlot  
    } # End of Server
# *===========================================================================*
# Run the application 
shinyApp(ui = ui, server = server)