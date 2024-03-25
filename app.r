## app.R ##
library(shinydashboard)
library(sortable)
library(jsonlite)

loot_stuff = fromJSON("loot.json")

createBasicLoot = function(type_list) {
  item = sample(type_list,size = 1)
  return(item)
}

createLegendLoot = function(type_list) {
  item = sample(type_list,size = 1)
  attr = sample(c("Str","Dex","Will"),size = 1)
  return(paste(item,attr))
}

rollCharacter = function() {
  hp = sample(1:6,size = 1)
  str = sum(sample(1:6,size = 3))
  dex = sum(sample(1:6,size = 3))
  will = sum(sample(1:6,size = 3))
  money = sample(1:100,size =1)
  return(c(hp,str,dex,will,money))
  
}
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "DM Screen"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DM screen", tabName = "dm_screen", icon = icon("dashboard")),
      menuItem("Player screen", tabName = "player_screen", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
 tabItems(
   tabItem(tabName = "dm_screen",
    fluidRow(
      box(width = 3,
          title = "Turn counter",
          
      tagList(
        tags$div(
          id = "turn_counter",
          tags$div(
            style = "border: solid 0.2em red; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          ),
          tags$div(
            style = "border: solid 0.2em black; float:left; margin: 5px",
            p("")
          )
        ),
        sortable_js("turn_counter") # the CSS id
      )),
      box(width = 3,
          title = "DM checklist",
          tags$ul(
            tags$li("Increment Turn counter"),
            tags$li("Set Supply rate - check darkness"), 
            tags$li("Manage Supply"), 
            tags$li("Random encounter roll"),
            tags$li("Check time of day every 10 Turns")
          )),
      box(width = 3,
          title = "Travel actions",
          tags$ul(
            tags$li("Investigate something nearby"),
            tags$li("Explore or search for something"), 
            tags$li("Travel to a known location within the same hex"), 
            tags$li("Travel to another hex")
          )),
      uiOutput("dynamicBox")
      
    ),
    fluidRow(
      h4("Time of Day"),
      tagList(
        tags$div(id = "timeCounter",
                 box(width = 2,
                     background = "red",
                     p("Morning")),
                 box(width = 2,
                     p("Present time")),
                 box(width = 2,
                     "Midday"),
                 box(width =2,
                     background = "red",
                     "Evening"),
                 box(width = 2,
                     background = "black",
                     "Night"),
                 box(width = 2,
                     background = "black",
                     "Small hours")),
        sortable_js("timeCounter")
      )
      ),
      fluidRow(
        box(width = 3,
            title = "Basic loot table",
            tagList(
              tags$table(style = "width:100%",
                tags$tr(
                  tags$th("Roll"),
                  tags$th("Loot"),
                  tags$th("Sell price")
                ),
                tags$tr(
                  tags$td("1"),
                  tags$td("Wild herbs"),
                  tags$td("1 copper")
                ),
                tags$tr(
                  tags$td("2"),
                  tags$td("Animal parts"),
                  tags$td("1 copper")
                ),
                tags$tr(
                  tags$td("3"),
                  tags$td("Ore"),
                  tags$td("1 silver")
                ),
                tags$tr(
                  tags$td("4"),
                  tags$td("10 silver"),
                  tags$td(" ")
                ),
                tags$tr(
                  tags$td("5"),
                  tags$td("Basic item"),
                  tags$td("5 silver")
                ),
                tags$tr(
                  tags$td("6"),
                  tags$td("5 Supply"),
                  tags$td("50 copper")
                )
              )
            )),
      box(width = 3,
          title = "Legendary loot table",
          tagList(
            tags$table(style = "width:100%",
                       tags$tr(
                         tags$th("Roll"),
                         tags$th("Loot"),
                         tags$th("Sell price")
                       ),
                       tags$tr(
                         tags$td("1"),
                         tags$td("Elixer"),
                         tags$td("1 gold")
                       ),
                       tags$tr(
                         tags$td("2"),
                         tags$td("Potion"),
                         tags$td("4 gold")
                       ),
                       tags$tr(
                         tags$td("3"),
                         tags$td("Legendary item"),
                         tags$td("1 gold")
                       ),
                       tags$tr(
                         tags$td("4"),
                         tags$td("3d100 silver"),
                         tags$td(" ")
                       ),
                       tags$tr(
                         tags$td("5"),
                         tags$td("1d10 Ore"),
                         tags$td("1 silver/ore")
                       ),
                       tags$tr(
                         tags$td("6"),
                         tags$td("Legendary item"),
                         tags$td("1 gold")
                       )
            )
          )),
      box(
        title = "Dice Roller",
        width = 3,
        numericInput("numDice", "Number of Dice", value = 1, min = 1),
        numericInput("numFaces", "Number of Faces on Dice", value = 6, min = 1),
        actionButton("rollDice", "Roll Dice"),
        verbatimTextOutput("diceResult") # Output area for dice roll result
      ),
      box(
        title = "Loot generator",
        width = 2,
        actionButton("basicLootBut", "Basic item"),
        actionButton("legendLootBut","Legend item"),
        uiOutput("textOutput"), # Placeholder for text to be displayed
        p("Basic one-hand d6"),
        p("Basic two-hand d8"),
        p("Legendary one-hand 2d4"),
        p("Legendary two-hand 2d6")
      )
    ),
    fluidRow(
      box(title = "Crafting",
          width = 6,
          tags$ul(
            tags$li("Elixer - 1 gold or 10 Wild herbs and 10 silver, or 5 Wild herbs, 2 Animal parts, and 10 silver"),
            tags$li("Potion - 4 gold or 50 Wild herbs, 20 Animal parts, and 1 Gold, or 4 Gold"), 
            tags$li("Weapon upgrade - 1 gold or 30 Ore and 50 silver")
            )
          ),
      box(title = "Town and merchant",
          width = 6)
    )
   ),
   tabItem(tabName = "player_screen",
      fluidRow(
         box(title = "Stats",
             width = 3,
             textInput("hp","HP"),
             textInput("armor","Armor"),
             textInput("str","Str"),
             textInput("dex","Dex"),
             textInput("will","Will")
             ),
         box(
           title = "Wallet",
           width = 3,
           textInput("money","Money"),
           textInput("supply","Supply"),
           textInput("exp","Exp"),
           textInput("lore","Lore points")
         ),
         box(
           title = "Roll a character",
           width = 6,
           actionButton("rollChar", "Roll"),
           verbatimTextOutput("charOutput"),
           tags$ul(
             tags$li("Choose a race"),
             tags$li("Provision your character")
           )
         )
      ),
      fluidRow(
        box(width = 6,
            title = "Equiped items",
            textInput("head","Head"),
            textInput("back","Back"),
            textInput("chest","Chest"),
            textInput("feet","Feet"),
            textInput("wrists","Wrists"),
            textInput("weapon1","Weapon 1"),
            textInput("weapon2","Weapon 2")),
        box(width = 6,
            title = "Inventory",
            textInput("slot1","Slot 1"),
            textInput("slot2","Slot 2"),
            textInput("slot3","Slot 3"),
            textInput("slot4","Slot 4"),
            textInput("slot5","Slot 5"),
            textInput("slot6","Slot 6"),
            textInput("slot7","Slot 7"),
            textInput("slot8","Slot 8"),
            textInput("slot9","Slot 9"),
            textInput("slot10","Slot 10"))
      )
   )
 )#tabItem end 
  )
)

server <- function(input, output,session) {
  
  colorState <- reactiveVal("white")
  textColor = reactiveVal("black")
  
  observeEvent(input$colorCheckbox,{
    if (input$colorCheckbox) {
      colorState("black")
      textColor("white")# If checked, set color state to black
    } else {
      colorState("white")
      textColor("black") # If not checked, set color state to white
    }
  })
  
  output$dynamicBox <- renderUI({
    
    
    box(
      p("Light"),
      checkboxInput("colorCheckbox", "Switch Color", value = FALSE),
      width = 2,
      style = sprintf("background-color: %s; color: %s;", colorState(), textColor())
    )
  })
  
  # Ensure checkbox retains its state across reactivity updates
  observe({
    updateCheckboxInput(session, "colorCheckbox", value = input$colorCheckbox)
  })
  
  output$diceResult <- renderText({
    input$rollDice # This makes diceResult reactive to the button press
    isolate({
      if(input$numDice > 0 && input$numFaces > 0) {
        rolls <- sample(1:input$numFaces, size = input$numDice, replace = TRUE)
        rollSum <- sum(rolls)
        paste("Rolls:", paste(rolls, collapse = ", "), "\nTotal:", rollSum)
      } else {
        "Please enter valid numbers for dice and faces."
      }
    })
  })
  
  lootRet = reactiveVal("")
  
  observeEvent(input$basicLootBut, {
    loot = createBasicLoot(c(loot_stuff$weapon_types,loot_stuff$armor_types))
    lootRet(loot)
  })
  
  observeEvent(input$legendLootBut, {
    loot = createLegendLoot(c(loot_stuff$weapon_types,
                              loot_stuff$armor_types,
                              loot_stuff$trinkets))
    lootRet(loot)
  })
  
  output$textOutput <- renderUI({
    lootRet()
  })
  
  characterText = reactiveVal(" ")
  
  observeEvent(input$rollChar, {
    characterText(paste(rollCharacter()))
  })
  
  output$charOutput <- renderText({
    characterText()
  })
  
  
}

shinyApp(ui, server)