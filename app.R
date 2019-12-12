library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(googlesheets4)
library(googlesheets)
library(snakecase)
library(scales)
library(googledrive)
library(rvest)
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(gargle)

googleform_embed_link = "https://docs.google.com/forms/d/e/1FAIpQLSc5Pa72f1x48L30smKdkBD4GUxZZcDdxGcU5PGHYGK3LuT8QQ/viewform?usp=pp_url"
googleform_data_url = "https://docs.google.com/spreadsheets/d/1lNhXPLF4LxVOgzwsZfizc1NkpbPFjR42JaZYkUKDCXs/edit#gid=1652423070"

# data = "1lNhXPLF4LxVOgzwsZfizc1NkpbPFjR42JaZYkUKDCXs" %>%
#     gs_key(lookup = FALSE) %>%
#     gs_read(range = "A1:AP1107")

## gets live data
espn_start <- read_html("https://www.espn.com/college-football/schedule/_/week/1/seasontype/3")
links <- espn_start %>%
    html_nodes(".home+ td a") %>%
    html_attr('href') %>%
    str_sub(-9,-1)  

urls =
    links %>%
    map(~paste0("http://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=",.)) %>%
    unlist() %>%
    as_tibble() %>%
    rename(links = value)

raw.result <- GET(url = urls$links[1])
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)

getter = function(url = url) {
    raw.result <- GET(url = url)
    this.raw.content <- rawToChar(raw.result$content)
    this.content <- fromJSON(this.raw.content)
    game = tibble(
        home = this.content$boxscore$teams$team$displayName[1],  # home
        away = this.content$boxscore$teams$team$displayName[2],  # away
        bowl = this.content$header$gameNote,
        time = lubridate::ymd_hm(this.content$header$competitions$date),
        homecolor = this.content$boxscore$teams$team$color[1],  # home
        awaycolor = this.content$boxscore$teams$team$color[2],  # away
        homepred = this.content$predictor$homeTeam$gameProjection,  # home
        homeproblose = this.content$predictor$homeTeam$teamChanceLoss,  # home
        awaypred = this.content$predictor$awayTeam$gameProjection,  # away
        awayproblose = this.content$predictor$awayTeam$teamChanceLoss
    ) # away
    return(game)
}

url = "http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?seasontype=3&week=1&year=2020"

raw.result1 <- GET(url = url)
this.raw.content1 <- rawToChar(raw.result1$content)
this.content1 <- fromJSON(this.raw.content1)
# listviewer::jsonedit(this.content1)

scorevec =
    this.content1$events$competitions %>%
    map("competitors") %>% purrr::flatten() %>%
    map("score") %>% unlist()

scorevec = append(scorevec,c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

fulldata =
    map_df(urls$links[-40], ~getter(.)) %>%
    force_tz(time, tzone = "America/Los_Angeles") %>%
    mutate(time = time-hours(5)) %>%
    mutate(date = lubridate::date(time)) %>%
    mutate(gameid = urls$links[-40]) %>%
    mutate(homescore = scorevec[rep(c(TRUE,FALSE),length.out = 78)]) %>%
    mutate(awayscore = scorevec[rep(c(FALSE,TRUE),length.out = 78)])

### end live data getter



ui <- fluidPage(
    theme = shinytheme("journal"),
    titlePanel("Welcome to 2019 NCAA Bowl Pick'em!"),
    sidebarLayout(
        sidebarPanel(width = 4,

                     htmlOutput("googleForm")
        ),
        mainPanel(
            textInput("email", "Enter your email:", value = "your.email@here.com"),
            numericInput("pin", "Enter your pin:", value = "0000",min = 0, max = 9999),
            tabsetPanel(selected = "Instructions",
                        tabPanel("Your Selections", dataTableOutput("bigtable")),
                        tabPanel("All Selections", 
                                 tags$h3("Once a game begins, other players predictions will populate below."),
                                 dataTableOutput("allowed")),
                        tabPanel("Rankings", dataTableOutput("rankings")),
                        tabPanel("Analysis", dataTableOutput("analysis")),
                        tabPanel("Instructions", includeMarkdown("instructions.md"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$googleForm <- renderUI({
        tags$iframe(
            id = "googleform",
            src = googleform_embed_link,
            width = 475,
            height = 1000,
            frameborder = 25,
            marginheight = 0
        )
    })
    
    forappdata = reactive({
        
        data = "1lNhXPLF4LxVOgzwsZfizc1NkpbPFjR42JaZYkUKDCXs" %>%
            gs_key(lookup = FALSE) %>%
            gs_read(range = "A1:AP1107") %>% 
            mutate(Timestamp = lubridate::mdy_hms(Timestamp))
        
        rdata =
            data %>%
            janitor::clean_names() %>%
            rename(pin = provide_a_4_digit_pin_so_you_can_access_your_data_in_the_app) %>% 
            pivot_longer(cols = -c(1:2,42), names_to = "bowl") %>%
            separate(bowl, sep = "_or_", c("home", "away")) %>% 
            mutate(home = str_remove(home, "a_"), away = str_remove(away, "b_")) %>%
            mutate(home = to_any_case(home, case = "upper_camel", sep_out = " ")) %>%
            mutate(away = to_any_case(away, case = "upper_camel", sep_out = " "))  %>%
            mutate(away = if_else(away == "Charlotte 49 Ers", "Charlotte 49ers", away)) %>%
            mutate(pick = case_when(value < 50 ~ home,
                                    value > 50 ~ away,
                                    TRUE ~ "abstain")) %>%
            full_join(fulldata %>%
                          select(home, away, bowl, time,homescore,awayscore),
                      by = c("home", "away")) %>%
            filter(!is.na(value)) %>%
            filter(timestamp < time) %>%
            group_by(email_address, home, away) %>%
            filter(timestamp == max(timestamp)) %>%
            ungroup() %>%
            mutate(bowl = to_any_case(bowl, case = "upper_camel", sep_out = " "))  %>%
            mutate(value = rescale(value, to = c(0, 1), from = c(0, 100))) %>%
            mutate(pointsifhome = (1 - (1 - value)) ^ 2) %>%
            mutate(pointsifaway = (1 - value) ^ 2) %>%
            mutate(confidence = if_else(value > .5, (value - .5) * 2, (((
                1 - value
            ) - .5) * 2))) %>%
            mutate(confidence = round(confidence,2)) %>% 
            mutate(selection = if_else(confidence>.5,home,if_else(confidence<.5,away,"Neutral"))) %>%
            mutate(winner = if_else(homescore>awayscore,home,if_else(awayscore>homescore,away,"Tied"))) %>%
            mutate(pointsifcorrect = round(pmin(pointsifhome,pointsifaway),2), pointsifwrong = round(pmax(pointsifhome,pointsifaway),2)) %>%
            mutate(Points = ifelse(homescore != awayscore & winner==pick, pointsifcorrect, if_else(homescore != awayscore & winner!=pick,pointsifwrong,0))) %>% 
            mutate(Time = stamp("1 March 2019 at 7:30")(time))            
        
        return(rdata)
    })
    
    output$bigtable = renderDataTable({
        input$refresh
        rthis = forappdata() %>%
            mutate(pin = if_else(time<Sys.time(),99999,pin)) %>% 
            filter(email_address == input$email & pin == input$pin) %>% 
            select(email_address,bowl,home,homescore,away,awayscore,pick,confidence,Time,pointsifcorrect,pointsifwrong,winner) 
            
        return(rthis)
    }, filter = 'top')

    output$allowed = renderDataTable({
        input$refresh
        rthis = forappdata() %>%
            mutate(show = if_else(time<Sys.time(),TRUE,FALSE)) %>% 
            # mutate(show = if_else(time<Sys.time()+lubridate::weeks(2),TRUE,FALSE)) %>% 
            filter(show == TRUE) %>% 
            select(email_address,bowl,home,homescore,away,awayscore,pick,confidence,Time,pointsifcorrect,pointsifwrong,winner) 
            
        return(rthis)
    }, filter = 'top')
    
    output$rankings = renderDataTable({
        rankingsoutput =
            # rdata %>%
            forappdata() %>%
            group_by(email_address) %>%
            summarise(Points = sum(Points))
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)