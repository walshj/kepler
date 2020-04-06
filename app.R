#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
# library(shinyalert)
library(ggplot2)
library(grid)
require(DT)
library(shinyWidgets)
# library(shinyBS)
# library(shinydashboardPlus)


options(scipen = 999)
word_wrap <- function(x, len) {
  paste(strwrap(x, width = len), collapse = '\n')
}
#Get the data set and manipulate it to create some needed rows. Could easily be stored adjusted for efficiency, but this is just a proof of concept
kepler <- read.csv('kepler.csv', stringsAsFactors = FALSE)
star_sequences <- c('M', 'K', 'G', 'F', 'A', 'B', 'O') 
kepler <- kepler[order(kepler$koi_time0bk),]
kepler$star_class <- as.character(cut(kepler$koi_steff, breaks = c(2400,3700,5200,6000,7500,10000,30000,100000), labels = star_sequences))
kepler$star_class[is.na(kepler$koi_steff)] <- '?'
kepler$star_colora <- as.character(cut(kepler$koi_steff, breaks = c(2400,3700,5200,6000,7500,10000,30000,100000), labels = c('red', 'orange', 'yellow', 'yellow', 'teal', 'lightblue', 'blue') ))
kepler$star_colorb <- as.character(cut(kepler$koi_steff, breaks = c(2400,3700,5200,6000,7500,10000,30000,100000), labels = c('orangered', 'orange', 'yellow', 'lightyellow', 'white', 'lightblue', 'blue') ))
# levels(kepler$star_colora) <- c(levels(kepler$star_colora), "black")
# levels(kepler$star_colorb) <- c(levels(kepler$star_colorb), "black")
# kepler$star_colora <- as.character(kepler$star_colora)
# kepler$star_colorb <- as.character(kepler$star_colorb)
kepler$star_colora[is.na(kepler$star_colora)] <- 'black'
kepler$star_colorb[is.na(kepler$star_colorb)] <- 'black'
kepler$first_seen <- as.Date(kepler$koi_time0bk, origin = '2009-01-01')
kepler$icon <- sapply(kepler$koi_disposition, function(x) ifelse(x == 'CONFIRMED', 'check',"times"))
kepler$stat_color <- sapply(kepler$koi_disposition, function(x) ifelse(x == 'CONFIRMED', 'green', 'red'))
star_sequences <- unique(kepler$star_class)
habitable <- read.csv('habitable.csv', stringsAsFactors = FALSE)
#columns for analysis
t_columns <- c('kepid','kepoi_name', 'kepler_name','star_class','first_seen','koi_disposition','koi_score','stat_color', 'star_colora', 'star_colorb', 'koi_tce_plnt_num', 'koi_count', 'icon', 'koi_teq', 'koi_period', 'koi_prad')
#columns that will be seen
view_columns <- c('kepoi_name', 'kepler_name','koi_score', 'koi_period', 'koi_prad')
friendly_names <- c('KOI Name', 'Kepler Name', 'Certainty Score', 'Orbital (Earth Days)', 'Size (Earth Radii)')

ui <- dashboardPage(
  
  dashboardHeader(title = strong("Kepler Objects of Interest (KOI)"), titleWidth = 325),
  dashboardSidebar(
    sidebarMenu(
      box(width = 12,
          title = tags$h2(style="color:Aqua",strong("Filters")),
          background = 'black',
        menuItem(
          selectInput(inputId = 'st_type', label = 'Star Sequence', choices = c('All',star_sequences))
        ),
        menuItem(
          selectInput(inputId = 'status', label = 'Status', choices = c('All', 'CONFIRMED', 'CANDIDATE', 'FALSE POSITIVE'))
        ),
        menuItem(
          selectInput(inputId = 'sys_size', label = 'System Size', choices = c('All', sort(unique(kepler$koi_count))))
        ),
        menuItem(
          selectInput(inputId = 'hab_filt', label = 'Habitable', choices = c('Any', 'Most Likely', 'Optimistic'))
        )
      )
    )
    ),
  tags$head(includeHTML('google-analytics.html')),
  # tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 25%; font-size: 20px; width:400px; height:100px"))),
  body=dashboardBody(
    #this is the dashboard theme used. Can be changed
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    useShinyjs(),
    # useShinyalert(),
    fluidRow(
      column(width=6,
             valueBox(width = 10,
               subtitle=textOutput("intro"),
               
               value=NULL,
               icon=tags$i(
                 class = "fa fa-question", 
                 style = "color: rgb(21,182,234), opacity: 30"
               )
      ),
      tags$style(type="text/css", "#intro {white-space: pre-wrap;}")
    )
      
    ),
    fluidRow(
      column(width = 6,
               box(width = 10,
                   title = 'Descriptions',
                   textOutput('descriptions'),
                   solidHeader = TRUE,
                   background = 'aqua',
                   collapsible = TRUE,
                   collapsed = TRUE
               ),
             tags$style(type="text/css", "#descriptions {white-space: pre-wrap;}")
             )
    ),
    #This row just contains the infoboxes that display relevant info about the KOI and it's star
    fluidRow(
      column(width = 6,
             column( width = 6,
                     fluidRow(
                       valueBoxOutput('st_class', width = 12)
                       # ,
                       # bsPopover(id = "st_class", title = 'Test', content = "This describes the class of a start", placement = "right", trigger = "hover",
                       #           options = NULL)
                     ),
                     fluidRow(
                       valueBoxOutput('kep_id', width = 12)
                     )
                     
             ),
             column(width = 6,
                    fluidRow(
                      valueBoxOutput('first_seen', width = 12)
                    ),
                    fluidRow(
                      valueBoxOutput('status', width = 12)
                    )
             )
             )
      
    ),
    fluidRow(
      column(width = 7,
             DT::dataTableOutput(outputId = 'k_table') 
      )
    ),
    # fluidRow(
    #   textOutput(outputId = 'debugger')
    # ),
    #Row for the solar system map (as best approximated) and 
    fluidRow(
      column(width = 10,
        box(width = 4,
            title = tags$p(style="color: rgb(0, 0, 0)", "Objects in the stellar system"),
            background = 'aqua',
            collapsible = TRUE,
            collapsed = FALSE,
            tags$p(style="color: rgb(245, 250, 250)", "Stellar body in center, planet in Green"),
          plotOutput(outputId = 'planetRings')
        ),
 
        box(width = 4,
            title = tags$p(style="color: rgb(0, 0, 0)", "Relative size of the Planet"),
            background = 'aqua',
            collapsible = TRUE,
            collapsed = FALSE,
            tags$p(style="color: rgb(245, 250, 250)", "Earth in Green"),
          plotOutput(outputId = 'earth_compare')
        )
        )
      ),
    fluidRow(
      column(width = 8,
        box(width = 10, 
            title = tags$p(style="color: rgb(0, 0, 0)", "Temperature of the planet"),
            background = 'aqua',
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput(outputId = 'temperature')  
        )
      )
    ),
    # tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
    #                               background-color: #9c4242 !important;
    #                           }
    #                           "))),
    tags$style(HTML(".dataTables_wrapper .dataTables_length,  .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                    color: #FFFFFF !important;
                    },
                    .pf-char:before{
                        font-family: Arial; /* your font family here! */
                        font-weight: bold;
                        content: attr(data-char-content);
                    }")),

    fluidRow(
      box(width = 6,
          verbatimTextOutput("sig")
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  passModal <- function() {
    modalDialog(
      # renderText({
        tags$p(paste("The historic Kepler space telescope (Retired: October 2018) observed distant star systems in the hopes of discovering and confirming earth-sized planets orbiting other stars.",
              "Kepler discovered over 2,600 exoplanets during its tenure. This simple widget will allow you to explore some of the planets, whether confirmed or not, and some of their characteristics.",
              sep="\n"), style="color: rgb(0, 0, 0);")
        
      # }
      # )
    ,
      title = tags$h1("Introduction"),
      size = 'l',
      footer = tagList(
        column(width = 9, 
               renderText({"Based on the NASA Exoplanet Science Institute's KOI API."})
               ),
        column(width = 3, 
               modalButton("Dismiss")
               )
        ),
      style = "white-space: pre-wrap; background-color: rgb(240,250,250);"  
    )
  }
  showModal(passModal())
  output$intro <- renderText({
    paste("To select a different planet, please select a row from the table below.",
          "",
          "Collapsable graphics found below the table.",
          sep="\n")
    
  })
  
  output$descriptions <- renderText({
    paste("Kepler Object of Interest (KOI): A star observer by the Kepler Space telescope that is or was suspected of hosting one or more transiting planets.",
          "Kepler ID: ID of a potentially targeted star. Based on the Kepler Input Catalog (KIC), the list of Kepler IDs was generated prior to launch. The full list of KepIds has 13 million sources.",
          "KOI Name: This is the actual number of the KOI. These are stars in the KIC which are suspected to have transiting planets.",
          "Kepler Name: Kepler number name in the form \"Kepler-N,\" plus a lower-case letter, identifying the planet. ",
          "Star Class: The estimated class of star star based on the Morgan-Keenan Scale of O, B, A, F, G, K, and M, a sequence from the hottest (O type) to the coolest (M type). This class helps to describe the color (box color indicated) and other spectral qualities.",
          "First Seen: This is the date when the Kepler Space Telescope first detected this object.",
          "Kepler ID: Target identification number, as listed in the Kepler Input Catalog (KIC).",
          "Current Status: Indicates whether or not the object has been determined to be a planet or not.",
          "Certainty Score: Confidence in the assesment of it's disposition. Candidates with a higher value indicate more confidience that it could be a plaent. False Positives with a higher score indicate more certainty that it is not a transiting planet.",
          "Orbital: The interval between consecutive planetary transits. Analogous to a solar year on that planet.",
          "Size: The estimated radii of the planet as measured in earth radii.",
          "",
          "To select a different planet, please select a row from the table below.", sep="\n\n")
    
  })
  
  f_star <- reactive({if(input$st_type == 'All'){
    star_sequences
  } else{
    input$st_type
  }
    })
  f_status <- reactive({if(input$status == 'All') {
    c('CONFIRMED', 'CANDIDATE', 'FALSE POSITIVE')
    } else{
      input$status
      }
    })
  output$debugger <- renderText(f_status())
  
  f_system <- reactive({if(input$sys_size == 'All'){
    sort(unique(kepler$koi_count))
  } else{
    input$sys_size
  }})
  
  hab <- reactive({if(input$hab_filt == 'Any'){
    unique(kepler$kepler_name)
  } else{
    habitable$Name[habitable$Habitable==input$hab_filt]
  }})
  
  kepf <- reactive(kepler[(kepler$koi_disposition %in% f_status() 
                          & kepler$koi_count %in% f_system() 
                          & kepler$star_class %in% f_star()
                          & kepler$kepler_name %in% hab() ),
                          names(kepler) %in% t_columns])
  
  #build the table that will be used for most of the visualizations
    selected_row <- reactive(kepf()[input$k_table_rows_selected,])
    kep_status <- reactive(kepler$koi_disposition[kepler$kepoi_name == input$id_select])
    icon_stat <- reactive(kepler$icon[kepler$kepoi_name == input$id_select])
    stat_color <- reactive(kepler$stat_color[kepler$kepoi_name == input$id_select])
    
    styledBox <- function(btitle = NULL, bsubtitle = NULL, bwidth = 12, bcolor = 'aqua', tsize = 12, subsize = 12, bicon = icon('question'), icon = NULL, bicon_title = NULL, bicon_text = NULL){
      valueBox(width = bwidth,
          # background = bcolor,
          color = bcolor,
          value = list(fluidRow(
            column(width = 5,
                   align = 'left',
                    tags$p(style=paste0("font-size: ",tsize,"px"),btitle)
                   )
          )),
          subtitle = list(
          fluidRow(
            column(width = 10,
                   offset = 1,
                    tags$p(style=paste0("font-size: ",subsize,"px"),bsubtitle)
                   )
          ),
          fluidRow(align = 'right',
                   dropdownButton(
                     tags$h5(style="color: rgb(0, 0, 0)", bicon_title),
                     status='info',
                     size = 'xs',
                     icon = icon('question'),
                     tags$p(style="color: rgb(0, 0, 0)", bicon_text)
                   )
          )),
          icon = icon
      )
    }
    #the first box to display the star type and color. special tags to control font sizing
    output$st_class2 <- renderValueBox(
     valueBox(tags$p(style="font-size: 12px","Star Class"),
             color = selected_row()$star_colora, 
             value = selected_row()$star_class, 
             icon = icon("sun"),
             footer = fluidRow(align = 'right',
                               # height = "2px",
                               dropdownButton(
                                 size = 'xs',
                                 tags$h5(icon_title),
                                 icon_text,
                                 icon = icon('question')
                               )
             )
             )
   )
    output$st_class <- renderUI(
      styledBox(btitle = 'Star Class', 
                tsize=18, 
                subsize = 20, 
                bsubtitle = selected_row()$star_class, 
                icon = tags$i(
                  class = "fas pf-char fa-fw",
                  style = "`data-char-content` = \"S\""
                
                ),
                #icon('sun'),
                bcolor = selected_row()$star_colora, 
                bicon_title = NULL, 
                bicon_text = "This is the described star class. It helps to describe the color, size, heat and other aspects of the orbited star. The background of this box should be a very rough color indication.")
    )
    output$first_seen <- renderUI(
      styledBox(btitle = 'First Seen', 
                tsize=18, 
                subsize = 20, 
                bsubtitle = selected_row()$first_seen, 
                icon = icon('calendar'),
                bcolor = selected_row()$star_colora, 
                bicon_title = NULL, 
                bicon_text = "This is the date when the Kepler Space Telescope first detected this object.")
    )
    output$status <- renderUI(
      styledBox(btitle = 'Disposition (Status)', 
                tsize=16, 
                subsize = 20, 
                bsubtitle = selected_row()$koi_disposition, 
                icon = icon(selected_row()$icon),
                bcolor = selected_row()$stat_color, 
                bicon_title = NULL, 
                bicon_text = "Has the object been confirmed as a planet or not.")
    )
    output$kep_id <- renderUI(
      styledBox(btitle = 'Kepler ID', 
                tsize=16, 
                subsize = 20, 
                bsubtitle =  selected_row()$kepid, 
                icon = icon('tag'),
                bcolor = selected_row()$star_colora, 
                bicon_title = NULL, 
                bicon_text = "ID of a potentially targeted star. Based on the Kepler Input Catalog (KIC), the list of Kepler IDs was generated prior to launch. The full list of KepIds has 13 million sources.")
    )

    output$first_seen2 <- renderValueBox(
      valueBox(tags$p(style="font-size: 12px", "First Seen"), 
              color =selected_row()$star_colora, 
              value = tags$p(style="font-size: 10px", icon("calendar"), selected_row()$first_seen)
              )
    )
    output$status2 <- renderValueBox(
      valueBox(tags$p(style="font-size: 12px","Current Status"), 
              color = selected_row()$stat_color, 
              value = tags$p(style="font-size: 12px",icon(selected_row()$icon),selected_row()$koi_disposition)
              )
    )
    output$kep_id2 <- renderValueBox(
      valueBox(tags$p(style="font-size: 12px","Kepler ID"), 
               color = selected_row()$stat_color, 
               # value = tags$p(style="font-size: 12px",icon(selected_row()$icon),selected_row()$koi_disposition)
               value = selected_row()$kepid
               )
    )
    
    
   output$temperature <- renderPlot({
     temp <- selected_row()$koi_teq
     
     ggplot(kepf(), aes(koi_teq)) +
       geom_histogram(bins = 20, fill = 'yellow', color = 'yellow', alpha = 0.3) + 
       geom_vline(xintercept = temp,  color = 'green') + 
       geom_vline(xintercept = 251.15,  color = 'red') + 
       geom_vline(xintercept = 395.15,  color = 'red') + 
       geom_text(data=data.frame(x=temp,y=0),aes(x,y,label = temp), vjust=1.5) +
       geom_text(data=data.frame(x=temp,y=0),aes(x,y,label = 'Selected KOI'), vjust=-38) +
       geom_text(data=data.frame(x=300,y=0),aes(x,y,label = 'Life'), vjust = -42) +
       xlab('Temperature in Kelvin') +
       ylab('Number of KOI') +
       ggtitle(word_wrap("Planet temperatures in grouping", len = 30), subtitle = word_wrap("Life is viable between the red bars. Selected planet indicated in Green", len = 50)) +
       theme(plot.title = element_text(color='grey', size = 18, hjust = 0.5),
             plot.subtitle = element_text(color='grey', size = 12, hjust = 0.5))
       # geom_bar(aes(x=temp, y= 100))
     })
   
   output$planetRings <- renderPlot({
     x <- selected_row()$koi_tce_plnt_num
     y <- 0
     plantc <- max(selected_row()$koi_tce_plnt_num, selected_row()$koi_count, na.rm = TRUE)
     circ_rads<- seq(1,plantc,1)
     if(length(circ_rads[-x])==0){
       other_planets <- 0
     } else(
       other_planets <- circ_rads[-x]
     )
     # circ_rads<-seq(1,3,1)
     qplot(x=x,y=y, colour = I("green"), size = 1)+
       lapply(circ_rads,FUN=function(x) annotation_custom(circleGrob(gp=gpar(alpha=.3, fill="transparent", color="black")),-x,x,-x,x)) +
       geom_point(aes(x = other_planets, y = (other_planets*0)), size = .5) + 
       annotation_custom(circleGrob(gp=gpar( fill = selected_row()$star_colorb, color='black')),-.25,.25,-.25,.25) +
       geom_text(aes(x=0,y=circ_rads+0.1,label=circ_rads)) + 
       coord_fixed(xlim = c(-plantc - .5, plantc + .5), ylim = c(-plantc - .5, plantc + .5) ) + #coord_fixed(ratio = 1) +
       # ggtitle(word_wrap("Objects in the Stellar System", len = 20), subtitle = "(KOI in green)") +
       theme(plot.title = element_text(color='black', size = 16, hjust = 0.5),
             plot.subtitle = element_text(color='green', size = 12, hjust = 0.5),
             axis.line=element_blank(),axis.text.x = element_blank(),
             axis.text.y=element_blank(),axis.ticks = element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),legend.position="none",
             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),plot.background=element_blank())
   })
   output$earth_compare <- renderPlot({
     p_rad <- ifelse(is.na(selected_row()$koi_prad),1,selected_row()$koi_prad)
     ggplot() +
       annotation_custom(circleGrob(gp=gpar(alpha=.3, fill="green", color="black")),-1,1,-1,1) +
       annotation_custom(circleGrob(gp=gpar(alpha=.3, fill="red", color="black")), -p_rad, p_rad, -p_rad, p_rad) +
       coord_fixed(xlim = c(-max(1,p_rad,na.rm=TRUE) , max(1,p_rad,na.rm=TRUE)), ylim = c(-max(1,p_rad,na.rm=TRUE), max(1,p_rad,na.rm=TRUE)) ) +
       # ggtitle(word_wrap("Approximate KOI size relative to earth", len = 20), subtitle = "(Earth in green)") +
       theme(plot.title = element_text(color='black', size = 16, hjust = 0.5),
             plot.subtitle = element_text(color='green', size = 12, hjust = 0.5),
             axis.line=element_blank(),
             axis.text.x = element_blank(),
             axis.text.y=element_blank(),
             axis.ticks = element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             legend.position="none",
             panel.background=element_blank(),
             panel.border=element_blank(),
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             plot.background=element_blank()) 
   })
  output$k_table <- DT::renderDataTable({
   view_table <- format(kepf()[,view_columns], columns = c('koi_period', 'koi_score'), digits = 2)
   # names(view_table) <- friendly_names
   # view_table[,'koi_period']
   # formatRound(view_table,columns=c('Orbital Period (Earth Days)'), digits=3)
   },
   options = list(autoWidth = TRUE,
                  pageLength = 5),
   rownames = FALSE,
   colnames = friendly_names,
   # server = FALSE,
   selection = list(mode='single', selected = 1) 
   )
  
  output$sig <- renderText({
   paste("Designed and built by: James Walsh",
         "Using R Shiny and available packages",
         "Habitable planets sourced from http://phl.upr.edu/projects/habitable-exoplanets-catalog/data University of Puerto Rico at Arecibo",
         "List of KOI found at https://exoplanetarchive.ipac.caltech.edu/docs/data.html as hosted by California Institute of Technology",sep="\n")
 })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

