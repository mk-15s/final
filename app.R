# Load Packages ----
library(shiny)
library(tidyverse)
library(data.table)

# Load data ----

summary_user <- as.data.table(readRDS("data/summaryuser.rds"))
age_user <- as.data.table(readRDS("data/ageuser.rds"))
season_num_k <- as.data.frame(readRDS("data/seasonnumk.rds"))
season_num_k$people <- "내국인"
names(season_num_k)[2] <- "seasonnum"
season_num_f <- as.data.frame(readRDS("data/seasonnumf.rds"))
season_num_f$people <- "외국인"
names(season_num_f)[2] <- "seasonnum"
season_num <- rbind(season_num_k, season_num_f)
season_num <- as.data.table(season_num)


colorlist<- c("#F7766D", "#7CAD00", "#00BEC4", "#C77DFF")
colorlist1 <- c("#B3CDE3", "#FAB3AC")



# Define UI ----
ui <- fluidPage(
  titlePanel("서울특별시 자전거 대여사업 분석"),
  
  hr(),
  
  sidebarLayout(
  
  sidebarPanel(
    h3("대용량 자료관리 및 시각화 프로젝트"),
    h4("서미경 2019310286")
    ),
  
  mainPanel(
    img(src = "seoulbike.png", height = 100, width = 480)
    )
  ),
  
  
  hr(),
  
  h3("가입년월에 따른 가입자 수"),
  
  sidebarLayout(
    sidebarPanel(
      p("따릉이를 이용하기 위해 앱에 가입한 최근 가입자 수를 기록한 데이터 사용"),
      p("2018년부터 2021년 1월까지의 데이터"),
      
      
      helpText("가입연도에 따라 가입자 수를 나타낸 그래프"),
      
      selectInput("user", 
                  label = "가입연도 선택",
                  choices = c("2018", 
                              "2019",
                              "2020", 
                              "2021"),
                  selected = "2018")
      ),
    
    mainPanel(plotOutput("num")
      
    )
  ),

  hr(),
  
  h3("연령대와 성별에 따른 가입자 수"),
  
  sidebarLayout(
    sidebarPanel(
      
      p("따릉이를 이용하기 위해 앱에 가입한 최근 가입자 수를 기록한 데이터 사용"),
      p("연령대(10대 ~ 70대)와 성별로 나누어진 데이터"),
      
      helpText("연령대와 성별에 따라 가입자 수를 나타낸 그래프 / M (남성), F (여성)"),
      
      radioButtons("ageuser", 
                  label = "성별 선택",
                  choices = c("M", 
                              "F"),
                  selected = "M")
    ),
    
    mainPanel(plotOutput("agesex")
              
    )
  ),
  
  hr(),
  
  h3("2019년도 계절별 대여건수"),
  
  sidebarLayout(
    sidebarPanel(
    
      p("2019년 1월(겨울), 4월(봄), 8월(여름), 10월(가을)"),
      p("계절별로 따릉이를 대여한 건수"),
      
      helpText("내국인 / 외국인의 계절별 대여건수"),
      
      radioButtons("season", 
                   label = "내국인 / 외국인 선택",
                   choices = c("내국인", 
                               "외국인"),
                   selected = "내국인")
    ),
    
    mainPanel(plotOutput("season")
              
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  userInput <- reactive({
    summary_user[가입연도 == input$user]
  })
  
  colorInput <- reactive({
    colorlist[as.double(input$user) - 2017]
  })
  
  ageuserInput <- reactive({
    age_user[성별 == input$ageuser]
  })
  
  color1Input <- reactive({
    if (input$ageuser == "M"){
      a <- 1
    }
    else {
      a <- 2
    }
    colorlist1[a]
  })
  
  seasonInput <- reactive({
    season_num[people == input$season]
  })
  
  output$num <- renderPlot({
    
    ggplot(data = userInput(),
           mapping = aes(x = 가입월, y = 월별가입자수, group = 가입연도, color = 가입연도)) +
      geom_line(size = 2.5, colour = colorInput()) +
      theme_minimal() +
      labs(x = "가입 월", y = "가입자 수")+
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.position = "none",
            axis.text.y = element_text(angle = 90))+
      scale_x_discrete(labels = paste(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), "월")) +
      geom_point(data = userInput(), colour = colorInput(), size = 2.5) +
      geom_text(aes(label = 월별가입자수),
                position = position_dodge(width = 1),
                size = 3.5, vjust = -1, colour = "black")
      
    
  })
  
  output$agesex <- renderPlot({
    
    ggplot(data = ageuserInput(),
           mapping = aes(x = 연령대, y = 월별가입자수, fill = 성별)) +
      geom_bar(stat = "identity", position = "dodge", colour = color1Input()) +
      theme_minimal() +
      labs(x = "연령대", y = "가입자 수") +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            axis.text.y = element_text(angle = 90)) +
      geom_text(aes(label = 월별가입자수),
                position = position_dodge(width = 1),
                size = 4, vjust = -0.5) +
      scale_fill_manual(values = color1Input())
    
  })
  
  output$season <- renderPlot({
    
    ggplot(data = seasonInput(),
           mapping = aes(x = season, y = seasonnum, fill = as.factor(season)))+
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(x = "계절", y = "대여건수") +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.title = element_blank(),
            axis.text.y = element_text(angle = 90)) +
      scale_x_discrete(limits = c("봄", "여름", "가을", "겨울")) +
      scale_fill_manual(values = c("#FFCCCC", "#87D4CF", "#FFA881", "#AD9D9D"), breaks = c("봄", "여름", "가을", "겨울")) +
      geom_text(aes(label = seasonnum), position = position_dodge(width = 1), size = 4, vjust = -0.5)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
