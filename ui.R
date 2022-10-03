library(shiny)
library(shinydashboard)



# UI for Shiny Budget App
ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "Catat Cuan"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Budget", tabName = "budget", icon = icon("book")),
        menuItem("Pengeluaran Terbaru", tabName = "pengeluaran_terbaru", icon = icon("money")),      
        menuItem("Riwayat Pengeluaran", tabName = "riwayat_pengeluaran", icon = icon("table"))

        
        
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "riwayat_pengeluaran",
                fluidRow(
                  column(6,
                         tabBox(
                           selected = "Rata-Rata Tahunan",
                           width = "100%",
                           tabPanel("Rata-Rata Tahunan", plotOutput("plot_per_tahun"),
                                    selectInput("yr", label = "Tahun",choices = c()) 
                                    )
                           )
                  ),
                  column(6,
                         tabBox(
                           selected = "Total per Bulan",
                           width = "100%",
                           tabPanel("Total per Bulan", plotOutput("plot_per_bulan"),
                                    selectInput("mon", label = "Bulan", choices = month.name,
                                                selected = month.name[as.numeric(data.table::month(Sys.Date()))])),
                           tabPanel("Total per Kategori", plotOutput("plot_per_kategori"),
                                    selectInput("gp", label = "Kategori",choices = c()))
                         )
                  ),
                  fluidRow(
                    column(12,
                           style = "padding-left:30px",
                           tabBox(
                             selected = "Tabel Pengeluaran",
                             width = "100%", 
                             tabPanel("Tabel Pengeluaran", DT::dataTableOutput("expense_table"))
                           ),
                           br(),
                           br(),
                           )
                  )
                )),
        tabItem(tabName = "budget", 
                fluidRow(
                  column(6,
                         tabBox(
                           selected = "Budget Bulan Ini",
                           width = "100%",
                           tabPanel("Budget Bulan Ini", plotOutput("plot_budget")))
                         ),
                  column(6,
                         DT::dataTableOutput("tabel_budget"),
                         splitLayout(
                           cellWidths = c("50%", "50%"),
                           div(
                           br(),
                           br()
                           ),
                           div(
                             textInput("tambah_kategori_budget", "Kategori"),
                             numericInput("tambah_nominal_budget", "Budget", value = 0),
                             actionButton("tambah_budget", "Tambah Budget"))
                           )
                         )
                )),
        tabItem(tabName = "pengeluaran_terbaru",
                fluidRow(
                  column(6,
                         textInput("nama", label = "Nama ", placeholder = "Misal : Makan Siang"),
                         
                         selectInput("pilihan_kategori", label = "Kategori", choices = c("Pilih dari budget", "Buat kategori baru")),
                         
                         conditionalPanel(
                           condition = "input.pilihan_kategori === 'Buat kategori baru'",
                           textInput("kategori_baru", label = "Nama Kategori")
                         ),
                         
                         conditionalPanel(
                           condition = "input.pilihan_kategori === 'Pilih dari budget'",
                           selectInput("kategori_budget", label = "Nama Kategori", choices = c())
                         )
                  ),
                  column(6,
                         numericInput("nominal", label = "Nominal", value = 0),
                         dateInput("input_tanggal", "Tanggal", format = "mm/dd/yy"),
                         br(),
                         h5(""),
                         
                         actionButton("tambah_pengeluaran", "Tambah Pengeluaran")
                  )
                )
        )
      )
    )
  )

