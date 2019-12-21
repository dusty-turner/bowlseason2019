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
# listviewer::jsonedit(this.content)


getter = function(url = url) {
    raw.result <- GET(url = url)
    this.raw.content <- rawToChar(raw.result$content)
    this.content <- fromJSON(this.raw.content)
    game = tibble(
        home = this.content$boxscore$teams$team$displayName[1],  # home
        away = this.content$boxscore$teams$team$displayName[2],  # away
        homeid = this.content$boxscore$teams$team$id[1],  # home
        awayid = this.content$boxscore$teams$team$id[2],  # away
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

# rdata %>% 
#     filter(email_address == "dusty.s.turner@gmail.com") %>% 
#     select(gameid)

url = "http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?seasontype=3&week=1&year=2020"
url2 = "http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?seasontype=3&dates=2020"

raw.result1 <- GET(url = url)
this.raw.content1 <- rawToChar(raw.result1$content)
this.content1 <- fromJSON(this.raw.content1)
# listviewer::jsonedit(this.content1)

raw.result2 <- GET(url = url2)
this.raw.content2 <- rawToChar(raw.result2$content)
this.content2 <- fromJSON(this.raw.content2)
# listviewer::jsonedit(this.content2)

scorevec1 =
    this.content1$events$competitions %>%
    map("competitors") %>% purrr::flatten() %>%
    map("score") %>% unlist()

teamid1 =
    this.content1$events$competitions %>% 
    map("competitors") %>% purrr::flatten() %>% 
    map("id") %>% unlist()

scorevec2 =
    this.content2$events$competitions %>%
    map("competitors") %>% purrr::flatten() %>%
    map("score") %>% unlist()

teamid2 =
    this.content2$events$competitions %>% 
    map("competitors") %>% purrr::flatten() %>% 
    map("id") %>% unlist()

scorevec = append(scorevec1,scorevec2) 
idvec = append(teamid1,teamid2) 

pointsdfhome =
    tibble(scorevec,idvec) %>% 
    mutate(hora = rep(c(FALSE,TRUE),38)) %>% 
        filter(hora == TRUE) %>% 
    select(-hora)
pointsdfaway =
    tibble(scorevec,idvec) %>% 
    mutate(hora = rep(c(FALSE,TRUE),38)) %>% 
        filter(hora == FALSE) %>% 
    select(-hora)

# fulldata %>% select(home,away,homeid,awayid) %>% 
#     left_join(pointsdfhome, by = c("homeid"="idvec")) %>% 
#     rename(homescore = scorevec) %>% 
#     left_join(pointsdfaway, by = c("awayid"="idvec")) %>% 
#     rename(awayscore = scorevec) %>% 
#     select(-c(homeid,awayid))

fulldata =
    map_df(urls$links[-40], ~getter(.)) %>%
    force_tz(time, tzone = "America/Los_Angeles") %>%
    mutate(time = time-hours(5)) %>%
    mutate(date = lubridate::date(time)) %>%
    mutate(gameid = urls$links[-40]) %>% 
    left_join(pointsdfhome, by = c("homeid"="idvec")) %>% 
    rename(homescore = scorevec) %>% 
    left_join(pointsdfaway, by = c("awayid"="idvec")) %>% 
    rename(awayscore = scorevec) %>% 
    select(-c(homeid,awayid)) %>% 
    mutate(homescore = as.numeric(homescore),awayscore = as.numeric(awayscore))

### end live data getter


library(shinyjs)
ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("journal"),
    titlePanel("Welcome to 2019 NCAA Bowl Pick'em!"),
    sidebarLayout(position = "right",
        div( id ="Sidebar",sidebarPanel(width = 4,

                     htmlOutput("googleForm")
        )),
        mainPanel(
            actionButton("toggleSidebar", "Show / Hide Picker Window"),
            tags$h3("To view your picks, type in your email and pin:"),
            div(style="display:inline-block",textInput("email", "Enter your email:", value = "your.email@here.com")),
            div(style="display:inline-block",numericInput("pin", "Enter your pin:", value = "0000",min = 0, max = 9999)),
            tabsetPanel(selected = "Instructions",
                        tabPanel("Your Selections", dataTableOutput("bigtable")),
                        tabPanel("All Selections", 
                                 tags$h3("Once a game begins, other players predictions will populate below."),
                                 dataTableOutput("allowed")),
                        tabPanel("Rankings", tags$h3("Reminder: Lowest score wins!"), 
                                 # tags$br(),
                                 tags$h5("Scores reflect current scores in live games."),
                                 dataTableOutput("rankings")),
                        tabPanel("Game Info", 
                                 tags$h3("All the following information comes directly from ESPN's bowl game predictions."),
                                 dataTableOutput("gameinfo")),
                        tabPanel("Analysis", 
                                 tags$h3("Analysis of picks and results to follow once games begin."),
                                 plotOutput("analysis")),
                        tabPanel("Instructions", includeMarkdown("README.md"))
                        # tabPanel("Instructions", includeMarkdown("instructions.md"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$toggleSidebar, {
        shinyjs::toggleElement(id = "Sidebar")
    })

    output$googleForm <- renderUI({
        tags$iframe(
            id = "googleform",
            src = googleform_embed_link,
            width = 450,
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
        
        baseline = 
        data %>% select(`Email Address`,`Provide a 4 digit pin between 1000 and 9999 (so you can access your data in the app - and update your picks at a later date).`) %>% 
            group_by(`Email Address`,`Provide a 4 digit pin between 1000 and 9999 (so you can access your data in the app - and update your picks at a later date).`) %>% 
            distinct() %>% 
            ungroup()
        
        baseline2 =
        data %>% 
            select(-c(`Email Address`,`Provide a 4 digit pin between 1000 and 9999 (so you can access your data in the app - and update your picks at a later date).`,Timestamp)) %>% 
            slice(1) %>% 
            mutate_if(is.double, ~50) %>% 
            mutate(Timestamp = ymd_hms("2018-12-09 20:38:58")) %>%
            slice(rep(1:n(), each=nrow(baseline)))
            
        baseline3 =
            bind_cols(baseline,baseline2) 

        rdata =
            baseline3 %>%
            bind_rows(data) %>%
            janitor::clean_names() %>%
            rename(pin = provide_a_4_digit_pin_between_1000_and_9999_so_you_can_access_your_data_in_the_app_and_update_your_picks_at_a_later_date) %>% 
            pivot_longer(cols = -c(1:2,42), names_to = "bowl") %>%
            separate(bowl, sep = "_or_", c("home", "away")) %>% 
            mutate(home = str_remove(home, "a_"), away = str_remove(away, "b_")) %>%
            mutate(home = to_any_case(home, case = "upper_camel", sep_out = " ")) %>%
            mutate(away = to_any_case(away, case = "upper_camel", sep_out = " ")) %>%  
            mutate(away = if_else(away == "Charlotte 49 Ers", "Charlotte 49ers", away)) %>%
            mutate(home = if_else(home == "Smu Mustangs", "SMU Mustangs", home)) %>%
            mutate(home = if_else(home == "Ucf Knights", "UCF Knights", home)) %>%
            mutate(home = if_else(home == "Usc Trojans", "USC Trojans", home)) %>%
            mutate(home = if_else(home == "Hawaii Rainbow Warriors", "Hawai'i Rainbow Warriors", home)) %>%
            mutate(home = if_else(home == "Louisiana Ragin Cajuns", "Louisiana Ragin' Cajuns", home)) %>%
            mutate(away = if_else(away == "Uab Blazers", "UAB Blazers", away)) %>%
            mutate(away = if_else(away == "Byu Cougars", "BYU Cougars", away)) %>%
            mutate(away = if_else(away == "Texas A M Aggies", "Texas A&M Aggies", away)) %>%
            mutate(away = if_else(away == "Lsu Tigers", "LSU Tigers", away)) %>%
            mutate(away = if_else(away == "Miami Oh Red Hawks", "Miami (OH) RedHawks", away)) %>% 
            # mutate(time = force_tz(time))
                # select(home,away) %>% as.data.frame() %>% distinct()
            
            # fulldata %>% select(home, away) %>% as.data.frame()
            
                
            mutate(pick = case_when(value < 50 ~ home,
                                    value > 50 ~ away,
                                    TRUE ~ "abstain"))  %>% 
            full_join(fulldata %>%
                          select(home, away, bowl, time,homescore,awayscore,gameid),
                      by = c("home", "away")) %>%
            filter(!is.na(value)) %>% 
            filter(force_tz(timestamp) <force_tz(time)) %>%   
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
            mutate(Time = stamp("1 March 2019 at 7:30")(time)) %>% 
            mutate(time = force_tz(time)) %>% 
            mutate(over = if_else(force_tz(time)<force_tz(Sys.time()),TRUE,FALSE)) 
        
        # rdata %>%
        #     filter(email_address=="dusty.s.turner@gmail.com") %>%
        #     slice(38:39) %>%
        #         select(home,away,homescore, awayscore,Points,pick,pointsifcorrect,pointsifwrong,selection,winner,over)

        
        return(rdata)
    })
    
    output$bigtable = renderDataTable({
        input$refresh
        rthis = forappdata() %>%
            # mutate(pin = if_else(time<Sys.time(),99999,pin)) %>% 
            filter(email_address == input$email & pin == input$pin) %>% 
            arrange(time) %>% 
            select(email_address,bowl,home,homescore,away,awayscore,pick,confidence,Time,pointsifcorrect,pointsifwrong,winner,over) %>% 
            rename(`Email Address` = email_address, Bowl = bowl, Home = home, `Home Score` = homescore,
                   Away = away, `Away Score` = awayscore, Pick = pick, `Calculated Confidence` = confidence,
                   `Points If Correct` = pointsifcorrect, `Points If Wrong` = pointsifwrong, Winner = winner)  
            # formatStyle(~over,target = "row",backgroundColor = styleEqual(c("true", "false"), c('gray', 'yellow'))) %>% 
            
        return(rthis)
    }, filter = 'top', options = list(pageLength = 50, autoWidth = TRUE))

    output$allowed = renderDataTable({
        input$refresh
        rthis = forappdata() %>%
            # rdata  %>%
                # select(bowl, time) %>% arrange(time) %>% distinct() %>%
                # mutate(Sys.time()) %>%
            mutate(show = if_else(force_tz(time)<force_tz(Sys.time()),TRUE,FALSE)) %>% 
            # mutate(show = if_else(time<Sys.time()+lubridate::weeks(2),TRUE,FALSE)) %>%
            filter(show == TRUE) %>% 
            select(email_address,bowl,home,homescore,away,awayscore,pick,confidence,Time,pointsifcorrect,pointsifwrong,winner) %>% 
            rename(`Email Address` = email_address, Bowl = bowl, Home = home, `Home Score` = homescore,
                   Away = away, `Away Score` = awayscore, Pick = pick, `Calculated Confidence` = confidence,
                   `Points If Correct` = pointsifcorrect, `Points If Wrong` = pointsifwrong, Winner = winner)
        return(rthis)
    }, filter = 'top', options = list(pageLength = 50, autoWidth = TRUE))
    
    output$rankings = renderDataTable({
        rankingsoutput =
                # rdata %>%
            forappdata() %>% 
            mutate(show = if_else(force_tz(time)<force_tz(Sys.time()),TRUE,FALSE)) %>% 
            filter(show == TRUE) %>% 
            select(email_address, show, Points)  %>% 
            group_by(email_address) %>%
            summarise(Points = sum(Points)) %>%
            rename(`Email Address` = email_address) %>%
            arrange(Points)
        # return(forappdata())
        return(rankingsoutput)
            # mutate("test")
    }, filter = 'top', options = list(pageLength = 50, autoWidth = TRUE))

    output$gameinfo = renderDataTable({
        gameinfooutput =
            fulldata %>%
            select(bowl,home,away,time,homepred,awaypred,homescore,awayscore) %>% 
            rename(Bowl = bowl, Home = home, Away = away, Time = time,
                   `Home Prediction` = homepred, `Away Prediction` = awaypred, 
                   `Home Score` = homescore, `Away Score` = awayscore) %>% 
        mutate(Time = stamp("1 March 2019 at 7:30")(Time)) 
        return(gameinfooutput)
    }, filter = 'top', options = list(pageLength = 50, autoWidth = TRUE))

    output$analysis = renderPlot({
        forappdata() %>% 
        
        # rdata %>%
            filter(over == TRUE) %>%
            group_by(email_address) %>%
            summarise(Points = sum(Points)) %>%
            full_join(
                forappdata() %>%
                    filter(over == FALSE) %>%
                    group_by(email_address) %>%
                    summarise(best = sum(pointsifcorrect),worst = sum(pointsifwrong)),
                by = "email_address"
            ) %>% 
            mutate(best = Points+best, worst = Points+worst) %>% 
            mutate(email_address = str_remove(email_address, pattern = "@.*")) %>%
            mutate(email_address = str_replace_all(email_address,"[.]"," ")) %>%
            mutate(email_address = snakecase::to_upper_camel_case(email_address)) %>%
            ggplot(aes(x=fct_reorder(email_address,Points),y=Points)) +
            geom_point() +
            geom_errorbar(aes(ymin = best,ymax = worst)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Current Standings",
                subtitle = "Max and min scores represented by errorbars based on current selections",
                y = "Points Scored",
                x = "")
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)