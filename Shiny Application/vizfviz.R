library(shiny)
library(tidyverse)
library(ggstatsplot)
library(ggplotify)
library(magrittr)
library(plotly)
library(tidyverse)
library(sf)
library(heatmaply)
library(readr)
library(poLCA)
library(reshape2)
library(gridExtra)
library(parsetR)

#===================================================================== Weimin =================================================================
survey <- read_csv("cleaned_survey_results_2019.csv")
countries <- filter(survey, `What country do you live in?` %in% 
                        c("USA", "United Kingdom","Canada","India")) %>%
    filter(`What is your educational background`  %in% 
               c("PhD","Bachelors","Masters","Technical School"))%>%
    filter(`What's your gender identity?`  %in% c("Man","Woman"))%>%
    filter(`The organization you work for is in which of the following areas?` %in% 
               c("Academic","Not-for-profit","Private sector","Public sector"))%>%
    filter(`What area is your next priority for improving your data visualization skills?`  %in% 
               c("Improving my design skills","Improving my data skills","Learning a new technical tool or library","Improving my skills with an existing technical tool or library"))

# rename columns
colnames(countries)[3]<-"Skills to improve"
colnames(countries)[6]<-"Full Engagement"
colnames(countries)[12]<-"Separate Group"
colnames(countries)[43]<-"Educational Level"
colnames(countries)[41]<-"Willingness to improve visualisation skills"
colnames(countries)[44]<-"Organization Area"
colnames(countries)[46]<-"Gender"
colnames(countries)[49]<-"Country"

#===================================================================== Xinyue =================================================================
# import processed dataset
survey_copy <- read_csv("processed_survey.csv")
survey_copy <- survey_copy[,2:ncol(survey_copy)]

# rename tool_Pen&Paper column
survey_copy <- survey_copy %>% rename(tool_Pen_and_Paper = `tool_Pen&Paper`)

# column information
col_list <- colnames(survey_copy)

# data preprocessing
survey_copy[is.na(survey_copy)] = 0
# factorise columns for LCA
for (i in c(1:3, 6:9, 17:26, 39:46)) {
    survey_copy[col_list[i]] <- as.factor(unlist(survey_copy[col_list[i]]))
}
# break hours into 5 bins
for (i in c(10:15)) {
    survey_copy[col_list[i]] <- cut(unlist(survey_copy[col_list[i]]), breaks=c(-1,5,10,15,20,25), labels=c(1,2,3,4,5))
}

#===================================================================== Hongting =================================================================

ui <- navbarPage("Group 14 - VIZfVIZ",
                 tabPanel("Introduction",
                          h4("About"),
                          p("Where does the data visualisation stands today? Collected and organized by Data Visualization Society, Annual Data visualization Community Survey includes 50+ data visualization related questions and covers data visualization details such as salary, tool usage, demographic data, audiences and organizational structure. This survey provides valuable insights for organizations, practitioners and people who love data visualisation. We are three university students passionate about data visualisation. This project aims to design and develop a R Shiny application to allow audiences to explore data visualisation community survey data, and to maximise the insights obtained from the survey data. We demonstrate survey result from 3 dimensions, exploratory data analysis, cluster analysis and association analysis.\n"),
                          fluidRow(
                              column(7,
                          h4("Comments"),
                          p("For Exploration Data Analysis, survey questions are categorised into 3 groups, Demography, Job and Challenge: "),
                          p("1.  Demography - demographic questions, e.g. gender, country"),
                          p("2.  Job - role, organization, engagement level in data visualisation"),
                          p("3.  Challenge - willingness to improve visualisation skill, which visualisation skill to improve"),
                          p("--------------------------------------------------------------------------------------------------------------------------------"),
                          p("For Cluster Analysis, 3 visualisation formats are available for Latent Class Analysis: "),
                          p("1.  Line chart"),
                          p("2.  Stacked bar chart"),
                          p("3.  Heatmap")
                          ),
                          column(5,
                                 img(src='hi.png', align="right", width="666px", height="400px"))
                          )),
                 navbarMenu("EDA",
                            tabPanel("Demography",
                                     sidebarLayout(sidebarPanel(selectInput(inputId = "variable1",
                                                                            label = "Group by",
                                                                            choices = c("Education" ="Educational Level",
                                                                                        "Gender" = "Gender",
                                                                                        "Country"="Country"),
                                                                            selected = "Country"),
                                                                selectInput(inputId = "variable2",
                                                                            label = "To be Examined",
                                                                            choices = c("Education" ="Educational Level",
                                                                                        "Gender" = "Gender",
                                                                                        "Country"="Country"),
                                                                            selected = "Educational Level")),   
                                                   mainPanel(plotlyOutput("DemographyPlot"))  )),
                            tabPanel("Job",
                                     sidebarLayout(sidebarPanel(selectInput(inputId = "variable3",
                                                                            label = "Group by",
                                                                            choices = c("Organization Area"="Organization Area",
                                                                                        "Separate Group" ="Separate Group",
                                                                                        "Country"="Country",
                                                                                        "Full Engagement"="Full Engagement"),
                                                                            selected = "Country"),
                                                                selectInput(inputId = "variable4",
                                                                            label = "To be Examined",
                                                                            choices = c("Organization Area"="Organization Area",
                                                                                        "Separate Group" ="Separate Group",
                                                                                        "Country"="Country",
                                                                                        "Full Engagement"="Full Engagement"),
                                                                            selected =  "Full Engagement")),   
                                                   mainPanel(plotlyOutput("JobPlot"))  )),
                            tabPanel("Challenge",
                                     sidebarLayout(sidebarPanel(selectInput(inputId = "variable5",
                                                                            label = "Group by",
                                                                            choices = c("Skills to improve"="Skills to improve",
                                                                                        "Willingness to improve visualisation skills" ="Willingness to improve visualisation skills",
                                                                                        "Country"="Country"),
                                                                            selected = "Country"),
                                                                selectInput(inputId = "variable6",
                                                                            label = "To be Examined",
                                                                            choices = c("Skills to improve"="Skills to improve",
                                                                                        "Willingness to improve visualisation skills" ="Willingness to improve visualisation skills",
                                                                                        "Country"="Country"),
                                                                            selected = "Skills to improve")),   
                                                   mainPanel(plotlyOutput("ChallengePlot"))))),
                 
                 navbarMenu("Cluster Analysis",
                            tabPanel("Latent Class Analysis",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(
                                                       sliderInput(inputId = "classes",
                                                                   label = "Number of classes:",
                                                                   min = 1,
                                                                   max = 9,
                                                                   value = 4),
                                                       radioButtons(inputId ="variables", 
                                                                    label = "Cluster Variables", 
                                                                    choices = list(
                                                                        "Visualisation Tool Used", "Hour Spent Working with Data")),
                                                       width = 2, height = 2
                                                   ),
                                                   
                                                   
                                                   # Show a plot of the generated distribution
                                                   mainPanel(
                                                       tabsetPanel(type = "tabs",
                                                                   tabPanel("Line Plot", plotlyOutput("lcaPlot_line", width = "120%", height="600px")), 
                                                                   tabPanel("Stack Bar Chart", plotlyOutput("lcaPlot_bar", height="600px")),
                                                                   tabPanel("Heatmap", plotlyOutput("lcaPlot_heatmap", width = "120%", height="600px"))
                                                       )
                                                   )))),
                            
                 navbarMenu("Association Analysis",
                            tabPanel("Parallel Set Chart",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(inputId = "firstlevel",
                                                         label = "Select 1st Variable",
                                                         choices = list("Undergraduate Major" = "Undergraduate_major",
                                                                        "Organization Area" = "Organization_area",
                                                                        "Role" = "Role",
                                                                        "Gender" = "Gender",
                                                                        "Country" = "Country",
                                                                        "Yearly Pay" = "Yearly_pay",
                                                                        "Excel" = "tool_Excel",
                                                                        "Tableau" = "tool_Tableau",
                                                                        "R" = "tool_R",
                                                                        "ggplot2" = "tool_ggplot2",
                                                                        "D3" = "tool_D3",
                                                                        "Python" = "tool_Python",
                                                                        "Pen & Paper" = "tool_Pen_and_Paper",
                                                                        "Illustrator" = "tool_Illustrator",
                                                                        "Power BI" = "tool_PowerBI",
                                                                        "Plotly" = "tool_Plotly"),
                                                         selected = "Role"),
                                             selectInput(inputId = "secondlevel",
                                                         label = "Select 2nd Variable",
                                                         choices = list("Undergraduate Major" = "Undergraduate_major",
                                                                        "Organization Area" = "Organization_area",
                                                                        "Role" = "Role",
                                                                        "Gender" = "Gender",
                                                                        "Country" = "Country",
                                                                        "Yearly Pay" = "Yearly_pay",
                                                                        "Excel" = "tool_Excel",
                                                                        "Tableau" = "tool_Tableau",
                                                                        "R" = "tool_R",
                                                                        "ggplot2" = "tool_ggplot2",
                                                                        "D3" = "tool_D3",
                                                                        "Python" = "tool_Python",
                                                                        "Pen & Paper" = "tool_Pen_and_Paper",
                                                                        "Illustrator" = "tool_Illustrator",
                                                                        "Power BI" = "tool_PowerBI",
                                                                        "Plotly" = "tool_Plotly"),
                                                         selected = "tool_Excel")
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                             parsetOutput("parallelPlot",  width = "100%", height = "666px")
                                         )
                                     )))
                           )

   
   
server <- function(input, output, session) {

    #===================================================================== Weimin =================================================================
    output$DemographyPlot <- renderPlotly({
        
        x <- input$variable1
        y <- input$variable2
        
        CountryNew <- group_by(countries,countries[, c(x,y)]) %>% tally() 
        CountryNew <- complete(CountryNew, CountryNew[,c(y)], fill = list(n = 0)) %>% 
            mutate(percentage = n / sum(n))
        
        p<- ggplot(CountryNew, aes(unlist(CountryNew[, c(x)]), y=percentage, fill = unlist(CountryNew[, c(y)]))) + 
            geom_bar(stat = 'identity', position = 'dodge') +
            geom_errorbar(stat = 'identity', position = 'dodge',aes(ymin = percentage - 2* sd(percentage), ymax = percentage +
                                                                        2*sd(percentage))) + 
            scale_y_continuous(labels=scales::percent)+
            coord_flip() +
            xlab(x) +
            guides(fill=guide_legend(title=y)) +
            theme_bw()
        
    })
    
    output$JobPlot <- renderPlotly({
        
        x <- input$variable3
        y <- input$variable4
        
        CountryNew <- group_by(countries,countries[, c(x,y)]) %>% tally() 
        CountryNew <- complete(CountryNew, CountryNew[,c(y)], fill = list(n = 0)) %>% 
            mutate(percentage = n / sum(n))
        
        p<- ggplot(CountryNew, aes(unlist(CountryNew[, c(x)]), y=percentage, fill = unlist(CountryNew[, c(y)]))) + 
            geom_bar(stat = 'identity', position = 'dodge') +
            geom_errorbar(stat = 'identity', position = 'dodge',aes(ymin = percentage - 2* sd(percentage), ymax = percentage +
                                                                        2*sd(percentage))) + 
            scale_y_continuous(labels=scales::percent)+
            coord_flip() +
            xlab(x) +
            guides(fill=guide_legend(title=y)) +
            theme_bw()
        
    })
    
    output$ChallengePlot <- renderPlotly({
        
        x <- input$variable5
        y <- input$variable6
        
        CountryNew <- group_by(countries,countries[, c(x,y)]) %>% tally() 
        CountryNew <- complete(CountryNew, CountryNew[,c(y)], fill = list(n = 0)) %>% 
            mutate(percentage = n / sum(n) )
        
        p<- ggplot(CountryNew, aes(unlist(CountryNew[, c(x)]), y=percentage, fill = unlist(CountryNew[, c(y)]))) + 
            geom_bar(stat = 'identity', position = 'dodge') +
            geom_errorbar(stat = 'identity', position = 'dodge',aes(ymin = percentage - 2* sd(percentage), ymax = percentage +
                                                                        2*sd(percentage))) + 
            scale_y_continuous(labels=scales::percent)+
            coord_flip() +
            xlab(x) +
            guides(fill=guide_legend(title=y)) +
            theme_bw()
        
    })
    
    #===================================================================== Xinyue =================================================================
    output$lcaPlot_bar <- renderPlotly({
        
        f1 <- cbind(tool_Excel, tool_Tableau, tool_R, tool_ggplot2, tool_D3, tool_Python, tool_Pen_and_Paper, tool_Illustrator, tool_PowerBI, tool_Plotly) ~ 1
        LCA1 <- poLCA(f1, survey_copy, nclass = input$classes)
        
        # bar chart
        class_prof <- as.data.frame(LCA1$probs)
        
        # yes: 2
        class_prof <- t(class_prof)
        class_prof <- cbind(variable = rownames(class_prof), class_prof)
        rownames(class_prof) <- NULL
        class_prof <- as.data.frame(class_prof)
        class_prof$variable_id <-c(1:nrow(class_prof))
        
        #melt data frame into long format
        df <- class_prof[colnames(class_prof)[2:length(colnames(class_prof))]]
        for (i in c(1:length(colnames(df))-1)) {
            df[colnames(df)[i]] <- as.numeric(unlist(df[colnames(df)[i]]))
        }
        df <- melt(df, id.vars = 'variable_id', variable.name = 'Class')
        
        df$Tool_usage <- rep(rep(c("no", "yes"),10),input$classes)
        df$variable_id[df$variable_id %% 2 ==0] <- df$variable_id[df$variable_id %% 2 ==0]-1
        
        df$variable_id <- as.factor(df$variable_id)
        mapping <- c("1"="Excel", "3"="Tableau", "5"="R", 
                     "7"="ggplot2", "9"="D3", "11"="Python", 
                     "13"="Pen&Paper", "15"="Illustrator", 
                     "17"="PowerBI", "19"="plotly")
        
        df$Tool <- mapping[df$variable_id]
        p1 <- ggplot(df, aes(Tool, value)) +
            geom_bar(aes(fill = Tool_usage), position="fill", stat="identity") +
            coord_flip() +
            theme(legend.title = element_text(size=8, face="bold")) +
            scale_fill_brewer(palette = "RdYlBu") +
            xlab("\nTool") +
            ylab("\nProbability") +
            facet_wrap(~ Class)
        
        
        #========================================================= Hour Visualisation =======================================================
        
        f2 <- cbind(Hour_data_visualization, Hour_data_engineering, Hour_data_science, Hour_design, Hour_data_prep, Hour_build_portfolio) ~ 1
        LCA2 <- poLCA(f2, survey_copy, nclass = input$classes)
        
        class_prof <- as.data.frame(LCA2$probs)
        
        # yes: 2
        class_prof <- t(class_prof)
        class_prof <- cbind(variable = rownames(class_prof), class_prof)
        rownames(class_prof) <- NULL
        class_prof <- as.data.frame(class_prof)
        
        #melt data frame into long format
        df <- class_prof
        for (i in c(2:length(colnames(df)))) {
            df[colnames(df)[i]] <- as.numeric(unlist(df[colnames(df)[i]]))
        }
        df <- melt(df, id.vars = 'variable', variable.name = 'Class')
        
        # get the category for each variable
        df <- cbind(df, reshape::colsplit(df$variable, split = "\\.", names = c('name', 'Pr', 'Time')))
        
        mapping <- c("1"="0-4hrs", "2"="5-9hrs", "3"="10-14hrs", "4"="15-19hrs", "5"="20-24hrs")
        df$Time <- mapping[df$Time]
        df$Time <- as.factor(df$Time)
        
        p2<-ggplot(df, aes(name, value)) +
            geom_bar(aes(fill = Time), position="fill", stat="identity") +
            scale_fill_brewer(palette = "RdYlBu") +
            coord_flip() + 
            xlab("\nDifferent Hours") +
            ylab("\nProbability") +
            facet_wrap(~ Class) 
        
        switch(input$variables,
               "Visualisation Tool Used" = ggplotly(p1),
               "Hour Spent Working with Data" = ggplotly(p2))
        
    })
    output$lcaPlot_line <- renderPlotly({
        
        f1 <- cbind(tool_Excel, tool_Tableau, tool_R, tool_ggplot2, tool_D3, tool_Python, tool_Pen_and_Paper, tool_Illustrator, tool_PowerBI, tool_Plotly) ~ 1
        
        LCA1 <- poLCA(f1, survey_copy, nclass = input$classes)
        # line chart
        class_prof <- as.data.frame(LCA1$probs)
        
        # yes: 2
        class_prof <- t(class_prof)
        class_prof <- cbind(variable = rownames(class_prof), class_prof)
        rownames(class_prof) <- NULL
        class_prof <- as.data.frame(class_prof)
        class_prof <- cbind(class_prof[-c(1)], reshape::colsplit(class_prof$variable, split = "\\.", names = c('variable', 'Pr', 'category')))
        class_prof <- class_prof %>% filter(category == 2)
        class_prof <- subset(class_prof, select=-c(Pr, category))
        
        df <- class_prof
        for (i in c(1:length(colnames(df))-1)) {
            df[colnames(df)[i]] <- as.numeric(unlist(df[colnames(df)[i]]))
        }
        
        #melt data frame into long format
        df <- melt(df, id.vars = 'variable', variable.name = 'Class')
        
        #create line plot for each column in data frame
        p3 <- ggplot(df, aes(factor(variable), value, group = Class, color = Class)) + 
            geom_line() +
            geom_point(aes(color = Class)) +
            theme(axis.text.x = element_text(angle = 90)) +
            xlab("\nTool") +
            ylab("\nProbability")
        
        
        #========================================================= Hour Visualisation =======================================================
        
        f2 <- cbind(Hour_data_visualization, Hour_data_engineering, Hour_data_science, Hour_design, Hour_data_prep, Hour_build_portfolio) ~ 1
        
        LCA2 <- poLCA(f2, survey_copy, nclass = input$classes)
        
        class_prof <- as.data.frame(LCA2$probs)
        
        # yes: 2
        class_prof <- t(class_prof)
        class_prof <- cbind(variable = rownames(class_prof), class_prof)
        rownames(class_prof) <- NULL
        class_prof <- as.data.frame(class_prof)
        class_prof <- cbind(class_prof[-c(1)], reshape::colsplit(class_prof$variable, split = "\\.", names = c('variable', 'Pr', 'Time')))
        mapping <- c("1"="0-4hrs", "2"="5-9hrs", "3"="10-14hrs", "4"="15-19hrs", "5"="20-24hrs")
        class_prof$Time <- mapping[class_prof$Time]
        class_prof$variable <- paste(class_prof$variable, class_prof$Time, sep="_")
        class_prof <- subset(class_prof, select=-c(Pr, Time))
        
        df <- class_prof
        for (i in c(1:length(colnames(df))-1)) {
            df[colnames(df)[i]] <- as.numeric(unlist(df[colnames(df)[i]]))
        }
        
        #melt data frame into long format
        df <- melt(df, id.vars = 'variable', variable.name = 'Class')
        
        #create line plot for each column in data frame
        p4 <- ggplot(df, aes(factor(variable), value, group = Class, color = Class)) + 
            geom_line() +
            geom_point(aes(color = Class)) +
            theme(axis.text.x = element_text(angle = 90)) +
            xlab("\nDifferent Hours") +
            ylab("\nProbability")
        
        switch(input$variables,
               "Visualisation Tool Used" = ggplotly(p3,tooltip = c("x", "y", "group")),
               "Hour Spent Working with Data" = ggplotly(p4, tooltip = c("x", "y", "group")))
    })
    output$lcaPlot_heatmap <- renderPlotly({
        
        f1 <- cbind(tool_Excel, tool_Tableau, tool_R, tool_ggplot2, tool_D3, tool_Python, tool_Pen_and_Paper, tool_Illustrator, tool_PowerBI, tool_Plotly) ~ 1
        
        LCA1 <- poLCA(f1, survey_copy, nclass = input$classes)
        
        ob_prob <- as.data.frame(LCA1$posterior)
        names(ob_prob) <- c(1:ncol(ob_prob))
        p5 <- heatmaply(ob_prob, 
                        k_col = 1,
                        k_row = input$classes,
                        fontsize_row = 1,
                        colors = rev(heat.colors(100)),
                        xlab = "Class")
        
        #========================================================= Hour Visualisation =======================================================
        
        f2 <- cbind(Hour_data_visualization, Hour_data_engineering, Hour_data_science, Hour_design, Hour_data_prep, Hour_build_portfolio) ~ 1
        
        LCA2 <- poLCA(f2, survey_copy, nclass = input$classes)
        
        ob_prob <- as.data.frame(LCA2$posterior)
        names(ob_prob) <- c(1:ncol(ob_prob))
        p6 <- heatmaply(ob_prob, 
                        k_col = 1,
                        k_row = input$classes,
                        fontsize_row = 1,
                        colors = rev(heat.colors(100)),
                        xlab = "Class")
        
        switch(input$variables,
               "Visualisation Tool Used" = ggplotly(p5),
               "Hour Spent Working with Data" = ggplotly(p6))
    })
    
    output$parallelPlot <- renderParset({
        
        col <- c(input$firstlevel, input$secondlevel)
        parset(survey_copy[col], width = "100%", height = "100%")
        
    })
    
}

 
shinyApp(ui = ui, server = server)
