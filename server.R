library(shiny)
library(data.table)
library(DT)
library(ggplot2)

#=====Data====#

server <- function(input, output, session) {
  load_db <- function() {
    #data untuk pengeluaran
    data_pengeluaran <- data.table(nama = character(),
                                   kategori = character(),
                                   nominal = numeric(),
                                   tanggal = character(),
                                   month = numeric(),
                                   year = numeric())
    #data untuk budget
    data_budget <- data.table(kategori = character(), 
                              budget = numeric()) 
    #warna yang tersedia
    warna_bar <- c('#48D1CC', '#C71585', '#191970', '#6B8E23', '#FFA500', '#D87093', '#98FB98', '#DC143C', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000',"#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
    
    #warna dan kategori sesuai data
    warna_kategori <- data.table(kategori = character(),
                                 col = character())
    return (list(data_pengeluaran, data_budget, warna_bar, warna_kategori))
  }
  
  ##################################### Buat Grafik ##########################################
  # data = data.table yang isinya kolom tanggal, kategori, bulan, tahun, and nominal
  # grup = pengeluaran berdasarkan kategori
  plot_per_kategori <- function(data, grup, yr) {
    if (grup == "Total") {
      pdata <- data[year == yr, .(total_pengeluaran = sum(nominal)), by = month]
    } else {
      pdata <- data[kategori == grup & year == yr, .(total_pengeluaran = sum(nominal)), by = month]
    }
    #kalau gaada data yang dimasukin, jangan buat plot apapun
    if(pdata[,.N] == 0) {
      return(FALSE)
    }
    #Bulan diambil 3 huruf aja di plot 
    pdata$bulan_awalan <- sapply(pdata$month, function(x) month.abb[x])
    #berdasarkan bulan
    pdata$bulan_awalan <- factor(pdata$bulan_awalan, levels = month.abb)
    
    # Plot total per kategori per tahun
    plt <- ggplot(pdata, aes(x = bulan_awalan, y = total_pengeluaran, grup = 1)) + 
      geom_line(size = 1.5, color = "black") +
      geom_point(size = 2, color = "red") + 
      expand_limits(y = 0)
    if (grup == "Total") {
      plt <- plt + ggtitle(paste0("Total Pengeluaran Bulanan"))
    } else {
      plt <- plt + ggtitle(paste0("Pengeluaran Bulanan pada ", grup))
    }
    
    plt <- plt + 
      labs(x = "Bulan", y = "Total Pengeluaran") + 
      theme(text=element_text(face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            title=element_text(size=16),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    plt
  }
  # Barplot per kategori
  #color_df : dataframe dengan warna untuk setiap kategori pada data
  barplot_per_kategori <- function(data, yr, color_df) {
    #Buat list dari input tahun untuk setiap kategori
    grups <- unique(data$kategori[data$year == yr])
    #kalo gaada, jadi plot kosong aja
    if (length(grups) == 0) {
      return(FALSE)
    }
    
    #Hitung rata-rata pengeluaran setiap tahun per kategori
    year_avg <- function(d, ct) {
      pd <- d[kategori == ct & year == yr , .(total_pengeluaran = sum(nominal)), by = month]
      return(mean(pd$total_pengeluaran))
    }
    avg_spending_list <- sapply(grups, function(x) year_avg(data, x))
    avg_spending_df <- data.frame(avg_spending = avg_spending_list, 
                                  kategori = names(avg_spending_list),
                                  row.names = NULL)
    
    # Add a color palette 
    avg_spending_df <- merge(avg_spending_df, color_df, by = "kategori", all.x = T) 
    
    # Order the categories by spending
    avg_spending_df$kategori <- factor(avg_spending_df$kategori, 
                                       levels = avg_spending_df$kategori[order(avg_spending_df$avg_spending, decreasing = T)])
    avg_spending_df$col <- avg_spending_df$col[order(avg_spending_df$avg_spending, decreasing = T)]
    
    plt <- ggplot(avg_spending_df, aes(x = kategori, y = avg_spending, fill = kategori)) +
      geom_col() + 
      scale_fill_manual(values = avg_spending_df$col) + 
      ggtitle(paste0("Rata-Rata per Kategori ", yr)) +
      xlab("Kategori") +
      ylab("Pengeluaran") +
      theme(text=element_text(face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            title=element_text(size=16),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      geom_text(aes(label=round(avg_spending)), position=position_dodge(width=0.9), vjust=-0.25)
    
    return(plt)
  }
  
  #barplot total per bulan
  plot_total_per_bulan <- function(data, mon, yr, color_df) {
    mon.index <- which(month.name == mon)
    #buat dataframe baru dengan pengeluaran per kategori
    pdata <- data[month == mon.index & year == yr, .(total_pengeluaran = sum(nominal)), by = kategori]
    
    #Tambah warna_bar
    pdata <- merge(pdata, color_df, by = "kategori", all.x = T) 
    
    #Kategori berdasarkan pengeluaran
    pdata$kategori <- factor(pdata$kategori,
                             levels = pdata$kategori[order(pdata$total_pengeluaran,decreasing = T)])
    
    pdata$col <- pdata$col[order(pdata$total_pengeluaran, decreasing = T)]
    
    #barplot tol per bulan
    ggplot(pdata, aes(x = kategori, y = total_pengeluaran, fill = kategori)) + 
      geom_col() +
      scale_fill_manual(values = pdata$col) + 
      ggtitle(paste0("Pengeluaran Bulan ", mon," ", yr)) +
      xlab("Kategori") +
      ylab("Pengeluaran") +
      theme(text=element_text(face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            title=element_text(size=16),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      geom_text(aes(label=round(total_pengeluaran)), position=position_dodge(width=0.9), vjust=-0.25)
    
  }
  
  #Budget
  sisa_bulanan <- function(data, budget_df, color_df) {
    mon.index <- as.numeric(data.table::month(Sys.Date()))
    year.index <- as.numeric(data.table::year(Sys.Date()))
    #Buat dataframe baru dengan pengeluaran per kategori
    pdata <- data[month == mon.index & year == year.index, .(total_pengeluaran = sum(nominal)), by = kategori]
    
    # Tetapkan kategori yang belum dihabiskan untuk bulan ini ke 0
    #Kategori yang belum dihabiskan untuk 
    plot_df <- merge(pdata, budget_df, by = "kategori", all.y = T)
    plot_df[is.na(plot_df$total_pengeluaran), "total_pengeluaran"] <- 0
    plot_df$remaining <- plot_df$budget - plot_df$total_pengeluaran 
    plot_df <- merge(plot_df, color_df, by = "kategori", all.x = T) 
    
    #Urutan kategori berdasarkan berapa banyak yg dibudgetkan 
    plot_df$kategori <- factor(plot_df$kategori,
                               levels = plot_df$kategori[order(plot_df$budget,decreasing = T)])
    plot_df$col <- plot_df$col[order(plot_df$budget, decreasing = T)]
    
    #Plot budget
    ggplot(plot_df, aes(x = kategori, y = total_pengeluaran, fill = kategori)) + 
      geom_col() +
      geom_point(mapping = aes(x = kategori, y = budget)) + 
      scale_fill_manual(values = plot_df$col) + 
      ggtitle(paste0("Budget Bulan ", month.name[mon.index]," ", year.index)) +
      xlab("Kategori") +
      ylab("Sisa Budget") +
      theme(text=element_text(face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            title=element_text(size=16),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      geom_text(aes(label=round(remaining)), position=position_dodge(width=0.9), vjust=-0.25)
  }
  
  # input data
  input_data <- load_db()
  data_pengeluaran <- input_data[[1]]
  data_budget <- input_data[[2]]
  warna_bar <- input_data[[3]]
  warna_kategori <- input_data[[4]]
  
  
  values <- reactiveValues(session_database = data_pengeluaran, 
                           session_budget = data_budget,
                           session_color_choices = warna_kategori,
                           show_table = FALSE, 
                           rerender_tabel_pengeluaran = 0,
                           rerender_tabel_budget = 0)
  #memperbarui tabel
  budget_proxy <- dataTableProxy("tabel_budget")
  
  #Set proxy cuma sekali, kalau tabel sebelumnya ga muncul karena kosong
  observe({
    if (!values$show_table && values$session_database[,.N] > 0) {
      proxy <<- dataTableProxy("expense_table")
      values$show_table <- TRUE
    }
  })
  
  #Hapus input kalau udah ditambah pengeluaran
  reset_inputs <- function() {
    updateTextInput(session, inputId = "nama", value = "")
    updateSelectInput(session, inputId = "pilihan_kategori", selected = "Pilih dari budget")
    updateSelectInput(session, inputId = "kategori", choices = isolate(values$session_database$kategori))
    updateTextInput(session, inputId = "kategori_baru", value = "")
    updateNumericInput(session, inputId = "nominal", value = 0)
    updateDateInput(session, inputId = "input_tanggal", value = Sys.Date())
  }
  
  #Tambah kategori pada budget, kalau udah ada gak bakal nambah kategori baru 
  # tabel budget re-render
  tambah_kategori_budget <- function(cg) {
    if (! cg %in% values$session_budget$kategori) {
      baris_baru <- data.table(kategori = cg, budget = 0) 
      values$session_budget <- rbindlist(list(values$session_budget, baris_baru))
      values$rerender_tabel_budget <- values$rerender_tabel_budget  + 1 
    }
  }
  
  #Tambah kategori dari tambah pengeluaran(belu ada di budget) untuk bulan ini
  tambah_kategori_budget_dari_pengeluaran <- function() {
    # Get current month/year
    mon.index <- as.numeric(data.table::month(Sys.Date()))
    year.index <- as.numeric(data.table::year(Sys.Date()))
    
    data <- values$session_database
    
    # tambah kategori bulan ini
    Kategori_bulan_ini <- unique(data$kategori[data$month == mon.index & data$year == year.index])
    sapply(Kategori_bulan_ini, tambah_kategori_budget)
  }
  
  #tambah kategori baru ke warna_bar
  tambah_warna_kategori <- function(cg) {
    baris_baru <- data.table(kategori = cg, col = warna_bar[1])
    warna_bar <<- warna_bar[-1]
    values$session_color_choices <- rbindlist(list(values$session_color_choices, baris_baru))
  }
  
  #hapus kategori yang tidak dibutuhin ladi dari data warna
  remove_kategori_color <- function(cg) {
    warna_bar <<- c(values$session_color_choices$col[values$session_color_choices$kategori == cg], warna_bar)
    values$session_color_choices <- values$session_color_choices[values$session_color_choices$kategori != cg,]
  }
  
  ################################## Data Addition/Changes ##########################################
  
  #Mengumpulkan data untuk input yang baru dimasukan
  input_baru <- reactive({
    tanggal <- format(input$input_tanggal, "%m/%d/%y")
    mon.index <- as.numeric(data.table::month(format(input$input_tanggal, "%Y-%m-%d")))
    year.index <- as.numeric(data.table::year(format(input$input_tanggal, "%Y-%m-%d")))
    
    #Pemilihan kategori
    if (input$pilihan_kategori == "Pilih dari budget") {
      kategori <- input$kategori_budget
    } else {
      kategori <- trimws(input$kategori_baru)
    }
    
    item_data <- data.frame(input$nama,
                            kategori,
                            input$nominal,
                            tanggal,
                            mon.index,
                            year.index,
                            stringsAsFactors = F)
    colnames(item_data) <- c("nama", "kategori", "nominal", "tanggal", "month", "year")
    item_data
  })
  
  # Add item to database on submit 
  #Tambah data pada 'tambah pengeluaran'
  observeEvent(input$tambah_pengeluaran, {
    if (values$show_table) {
      tmp_input_baru <- input_baru()
      tmp_input_baru$month <- month.name[tmp_input_baru$month] #tampilkan nama bulan
      proxy %>% addRow(tmp_input_baru, resetPaging = F)
    }
    # Update database
    baris_baru <- as.data.table(input_baru())
    values$session_database <- rbindlist(list(values$session_database, baris_baru), fill = T)
    
    #Cek apakah kategori baru perlu ditambahkan ke budget atau udah ada
    if (input$pilihan_kategori %in% c("Buat kategori baru")) {
      mon.index <- as.numeric(data.table::month(Sys.Date()))
      year.index <- as.numeric(data.table::year(Sys.Date()))
      #tambah kategori ke budget kalau yang baru ditambahin dari bulan ini
      if(baris_baru$month == mon.index && baris_baru$year == year.index) {
        tambah_kategori_budget(baris_baru$kategori)
      }
    } 
    reset_inputs()
    
  })
  
  #Data dari kategori yang baru di input
  new_budget_kategori <- reactive({
    item_data <- data.frame(trimws(input$tambah_kategori_budget),
                            input$tambah_nominal_budget,
                            stringsAsFactors = F)      
    colnames(item_data) <- c("kategori", "budget")
    item_data
  })
  
  # Tambah kategori baru pada budget
  observeEvent(input$tambah_budget, {
    #kalau kategorinya sudah ada, nominal di budget langsung nambah
    if (input$tambah_kategori_budget %in% values$session_budget$kategori) {
      values$session_budget[kategori == input$tambah_kategori_budget, "budget"] <- input$tambah_nominal_budget
      values$rerender_tabel_budget <- values$rerender_tabel_budget + 1
    } else {
      #kalau belum ada, tambah budget
      budget_proxy %>% addRow(new_budget_kategori(), resetPaging = F)
      
      # Update database
      baris_baru <- as.data.table(new_budget_kategori())
      values$session_budget <- rbindlist(list(values$session_budget, baris_baru), fill = T)  
    }
    
    # Reset inputs
    updateTextInput(session, inputId = "tambah_kategori_budget", value = "")
    updateNumericInput(session, inputId = "tambah_nominal_budget", value = 0)
  })
  
  #Perubahan budget
  observeEvent(input$tabel_budget_edit, {
    info <- input$tabel_budget_edit
    i <- info$row
    v <- info$value
    value <- as.numeric(v)
    #ubah datanya
    values$session_budget[i,"budget"] <- value
  })
  
  # Tambah/hapus warna dari data warna pas budget diupdate
  observeEvent(values$session_budget, {
    #Tambah kategori yang ada di budget tapi gaada di data warna
    tambah_kategori_ke_warna <- setdiff(values$session_budget$kategori, values$session_color_choices$kategori)
    
    #Hapus kategori yang gaada di budget sama pengeluaran
    hapus_kategori <- setdiff(values$session_color_choices$kategori, 
                              c(values$session_budget$kategori, values$session_database$kategori))
    
    if (length(tambah_kategori_ke_warna != 0)) {
      sapply(tambah_kategori_ke_warna, tambah_warna_kategori)
    }
    if (length(hapus_kategori) != 0) {
      sapply(hapus_kategori, remove_kategori_color)
    }
  })
  
  #Tambah/hapus kategori ke data warna
  observeEvent(values$session_database, {
    #tambah kategori yang ada di pengeluaran tapi gaada di data warna
    tambah_kategori_ke_warna <- setdiff(values$session_database$kategori, values$session_color_choices$kategori)
    
    #Hapus kategori yang gaada di budget sama pengeluaran
    hapus_kategori <- setdiff(values$session_color_choices$kategori, 
                              c(values$session_budget$kategori, values$session_database$kategori))
    
    if (length(tambah_kategori_ke_warna != 0)) {
      sapply(tambah_kategori_ke_warna, tambah_warna_kategori)
    }
    if (length(hapus_kategori) != 0) {
      sapply(hapus_kategori, remove_kategori_color)
    }
  })
  ####################################### Update Input #############################################
  
  #ubah pilihan berdasarkan kategori yang ada
  observe({
    updateSelectInput(session, "kategori", choices = values$session_database$kategori)
    updateSelectInput(session, "kategori_budget", choices = values$session_budget$kategori)
    updateSelectInput(session, "gp", choices = c("Total", values$session_database$kategori))
    updateSelectInput(session, "yr", choices = year(as.Date(values$session_database$tanggal, format = "%m/%d/%y")),
                      selected = max(as.numeric(year(as.Date(values$session_database$tanggal, format = "%m/%d/%y")))))
  })
  
  #######################################Riwayat Pengeluaran#################################################
  
  #Tabel pengeluaran
  output$expense_table <- DT::renderDataTable({
    # rerender tabel pengeluaran
    re_render <- values$rerender_tabel_pengeluaran
    if (values$show_table) {
      # indeks bulan jadi nama bulan
      df = isolate(values$session_database)
      df$month <- sapply(df$month, function(x) month.name[x], USE.NAMES = F)
      DT::datatable(df,
                    options = list(lengthMenu = c(10, 20, 50),
                                   pageLength = 10,searching=FALSE
                    ),
                    rownames = F,
                    editable = F)
    }
  }, server = FALSE)
  
  # Rata-rata per kategori bar plot
  output$plot_per_tahun <- renderPlot({
    # Don't render before year has a chance to update
    if (values$show_table && !is.na(as.numeric(input$yr))) {
      barplot_per_kategori(values$session_database, as.numeric(input$yr), values$session_color_choices)
    }
  })
  
  #plot total per kategori
  output$plot_per_kategori<- renderPlot({
    # Jangan dulu render kalau tahun belum memperbaharui
    if (values$show_table && !is.na(as.numeric(input$yr))) {
      plot_per_kategori(values$session_database, input$gp, as.numeric(input$yr))
    }
  })
  
  # Total spending in a given month bar plot
  output$plot_per_bulan <- renderPlot({
    # Don't render before year has a chance to update
    if (values$show_table && !is.na(as.numeric(input$yr))) {
      plot_total_per_bulan(values$session_database,input$mon, as.numeric(input$yr), values$session_color_choices)
    }
  })
  
  ####################################### Budget ####################################################
  
  # Budget barplot
  output$plot_budget <- renderPlot({
    sisa_bulanan(values$session_database, values$session_budget, values$session_color_choices)
  })
  #Tabel untuk budget
  output$tabel_budget <- DT::renderDataTable({
    re_render <- values$rerender_tabel_budget
    DT::datatable(isolate(values$session_budget),
                  options = list(lengthMenu = c(10, 20, 50),
                                 pageLength = 10, searching=FALSE
                  ),
                  rownames = F,
                  editable = F) 
  }, server = FALSE)
  
}
