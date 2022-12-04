source("sidebar.R")
source("home_tab.R")
source("player_tab.R")
source("data_tab.R")

header <- dashboardHeader(
    title = "Dota Stats"
)

sidebar <- dashboardSidebar(
    custom_sidebar
)


body <- dashboardBody(
    # shinyDashboardThemes(
    #     theme = "poor_mans_flatly"
    # ),
    
    tags$style(HTML("
                
                .box.box-solid.box-primary>.box-header {
                background:#ffffff
                
                }
                
                .box.box-solid.box-primary {
                background:#ffffff
                }
                ")),
    
    # customTheme, # custom_theme.R
    
    tabItems(
        homeBody,
        playerBody,
        dataBody
    )
)

dashboardPage(
    header, 
    sidebar, 
    body
)