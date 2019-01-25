library(shiny)
library(tmap)
library(tmaptools)
library(plyr)
library(readr)
library(broom)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)
library(GISTools)
library(ggrepel)
library(gridExtra)
library(grid)
library(png)
library(gtable)
library(shinyjs)
library(geofacet)
require(mapproj)
library(showtext)
library(shinyWidgets)
library(scales)
library(stringr)

options(shiny.maxRequestSize=30*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Генератор мап щодо електроенергії"),
    
    # Sidebar layout with input and output definitions ----
    useShinyjs(),
    column(7,
           
           fluidRow( 
               
               sidebarPanel(
                   
                   # Input: Select a file ----
                   fileInput("file1", "Оберіть csv-файл з даними. Розділювач - кома, кодування - UTF-8",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   tags$br(),
                   disabled(sliderTextInput("date_range", "Часовий проміжок. Якщо графік передбачає один місяць, буде обрано пізніший", 
                                            choices = c("січень 2018", "вересень 2018"), selected = c("січень 2018", "вересень 2018"))),
                   tags$br(),
                   disabled(selectInput("cons_category", label = "Оберіть категорію споживачів", choices = "Усі")),
                   disabled(selectInput("cons_type", label = "Оберіть тип споживачів", choices = "Усі")),
                   selectInput("variable_name", label = NULL, choices = c("Оберіть змінну" = "", "Накопичений борг, грн" = "debt_month_start", 
                                                                          "Споживання, кВт/год" = "consumption_tkwth", "Споживання, грн" = "consumption_tuah")),
                   radioButtons("map_type", "Тип графіка: ", choices = c("стан", "стан і зміни", "зміни кольором", "зміни графіком")),
                   helpText("Мапа генерується і змінюється тільки після натискання кнопки знизу:"),
                   fluidRow(disabled(actionButton("generate_button", "Згенерувати мапу", width = "100%")))
                   
               ),
               
               column(width = 8,
                      plotOutput("ukraine", width = "750px",height =  "500px" ))
           ),
           wellPanel(
               fluidRow( #tags$hr(),
                   helpText("Після того, як мапа згенерувалася, ви можете завантажити її на диск."),
                   column(1, 
                          radioButtons("extension", "Формат: ", choices = c("png", "pdf"))),
                   column(2,
                          radioButtons("goal", "Призначення: ", choices = c("facebook", "презентація"))),
                   column(3, 
                          textInput("customtitle", label = "Заголовок на мапі у файлі")),
                   column(3, 
                          textInput("customsubtitle", label = "Підзаголовок у файлі")),
                   
                   column(2,
                          disabled(downloadButton('downloadData', 'Завантажити')))),
               fluidRow(tags$hr(),
                   column(12, 
                          img(src = "./dixilogo_small.png"))),
               fluidRow(column(12, 
                               helpText("Виконання цього інструменту стало можливим завдяки підтримці американського народу, наданій через Агентство США з міжнародного розвитку (USAID). Думки, викладені в цьому інструменті, є виключною відповідальністю DiXi Group і за жодних обставин не можуть розглядатися як такі, що відображають позицію USAID чи Уряду США.")))
           )
           
    )
)

# Define server logic to read selected file ----
server <- function(session, input, output) {
    options(warn=-1)
    source("geofacet_functions.R")
    ua_monthes <- c("січень", "лютий", "березень", "квітень", "травень", "червень", "липень", 
                    "серпень", "вересень", "жовтень", "листопад", "грудень")
    ua_monthes_gen <- locale(date_names = "uk")$date_names$mon
    logo <- readPNG("qr_code_small.png")
    regions <- read.csv("companies_regions.csv")
    system("mkdir ~/.fonts")
    download.file(url = "https://github.com/localizator/ukrainian-fonts-pack/raw/master/BlissPro-Light%20-%20Bliss%20Pro%20-%20Light.ttf",
                  destfile = "~/.fonts/Bliss Pro Light.ttf")
    system('fc-cache -fEV ~/.fonts') 
    cents_table <- read.csv("cents_table.csv")
    tr_cbrt <- scales::trans_new("cube_rt", transform =  function(n) sign(n) * (abs(n) ^ (1/3)), inverse =  function(n) n ^ 3) 
    
    find_closest_date <- function(d, dates) {
        absolutes <- abs(dates - d)
        dates[which(absolutes == min(absolutes))]
    }
    
    date_format <- function(br) {
        monthes <- as.integer(format(br, format = "%m"))
        years <- as.integer(format(br, format = "%Y"))
        str_sub(ua_monthes[monthes], 1, 3)
    }
    
    capitalize <- function(s) {
        first <- substr(s, 1, 1)
        other <- substr(s, 2, nchar(s))
        paste0(toupper(first), other)
    }
    
    to_date <- function(year, month) {
        month <- as.character(month)
        month[nchar(month) == 1] <- paste0("0", month[nchar(month) == 1])
        as.Date(paste(year, month, "01", sep = "-"))
    }
    
    make_unique <- function(values, labels) {
        un_labels <- unique(labels)
        changes <- 0.1 / seq_along(un_labels)
        for (i in 1:length(values)) {
            values[i] <- values[i] +  changes[which(un_labels == labels[i])]
        }
        values
    }

    cat_changing_react <- observeEvent(input$cons_category, {
        cat <- input$cons_category
        if (cat != "Усі") {
            types <- na.exclude(categories$consumer_type[debt$consumer_cat == cat])
            if (length(types) > 1 ) {
                types <- c("Усі", types)
            }
            updateSelectInput(session = session, inputId =  "cons_type", choices = types)
            enable('cons_type')
        } else {
            updateSelectInput(session = session, inputId =  "cons_type", choices = c("Усі"))
            disable('cons_type')
        }
    })
    
    var_changing_react <- observeEvent(input$variable_name, {
        if (input$variable_name != "") {
            enable('generate_button')
        }
    })
    
    upload_react <- observeEvent(input$file1, {
        req(input$file1)
        tryCatch(
            {
                debt <- read_delim(input$file1$datapath, delim = ",", locale = locale(decimal_mark = ".", encoding = "UTF-8"))
                debt <- debt %>% 
                    dplyr::arrange(year, month) %>% 
                    dplyr::mutate(month_string = paste(ua_monthes[month], year)) %>% 
                    dplyr::left_join(regions, by = c("oblast" = "region"))  %>% 
                    dplyr::rename(region = oblast) %>% 
                    dplyr::mutate(region = UA_NAME)
                print(sort( sapply(ls(),function(x){object.size(get(x))})) )
                #print(debt)
                categories <<- debt %>% 
                    dplyr::select(consumer_cat, consumer_type) %>% 
                    dplyr::distinct()
                # активуємо деактивовані елементи вводу
                enable("date_range")
                enable("cons_category")
                # Генеруємо список категорій
                cat_list = c("Усі", unique(debt$consumer_cat))
                #cat_list <- lapply(cat_list, function(x) c(x = x))
                #names(cat_list) <- cat_list
                #print(cat_list)
                updateSelectInput(session = session, inputId = "cons_category", choices = cat_list)
                # змінюємо значення слайдера виходячи з наявних дат
                slider_dates <-  unique(debt$month_string)
                updateSliderTextInput(session = session, "date_range", choices = slider_dates, selected = c(slider_dates[1], slider_dates[length(slider_dates)]))
                ukr_adm2 <- readOGR("./simplified_shapefiles/UKR_adm1-2.shp")
                ukrainian_names <- read.csv("map_correspondence.csv")
                map_extended <- append_data(ukr_adm2, ukrainian_names,  key.shp = "ID_1", key.data = "ID_1")
                # переносимо змінні в глобальне середовище
                debt <<- debt
                map_extended <<- map_extended
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
    })
    
    generate_react <- observeEvent(input$generate_button, {
        output$ukraine <- renderPlot ({
            var <- isolate(input$variable_name)
            date_string <- isolate(input$date_range[1])
            period1 <- strsplit(date_string, " ")[[1]]
            month1 <- which(ua_monthes == period1[1])
            year1 <- as.integer(period1[2])
            second_date_string <- isolate(input$date_range[2])
            period2 <- strsplit(second_date_string, " ")[[1]]
            month2 <- which(ua_monthes == period2[1])
            year2 <- as.integer(period2[2])
            date1_gen <- paste(ua_monthes_gen[month1], year1)
            date2_gen <- paste(ua_monthes_gen[month2], year2)
            category <- isolate(input$cons_category)
            subcategory <- isolate(input$cons_type)
            cats <- character()
            if (category != "Усі") {
                cats <- c(cats, category)
                debt <- debt %>% 
                    filter(consumer_cat == category)
                if (subcategory != "Усі") {
                    debt <- debt %>% 
                        filter(consumer_type == subcategory)
                    if (subcategory != category) {
                        cats <- c(cats, subcategory)
                    } 
                }
            }
            map_type <- isolate(input$map_type)
            if (map_type == "стан і зміни") {
                params <- switch (var,
                                  debt_month_start = list(units = "грн", title = "Сумарна заборгованість за електроенергію", legend_title = "Борг, грн"),
                                  consumption_tkwth = list(units = "кВт/год", title = "Споживання електроенергії", legend_title = "Споживання, кВт/год"),
                                  consumption_tuah = list(units = "грн", title = "Споживання електроенергії", legend_title = "Споживання, грн"))
                debt <- debt %>%
                    dplyr::filter((year == year1 & month == month1) | (year == year2 & month == month2)) %>% 
                    dplyr::group_by(region_code, region, UA_NAME, year, month) %>% 
                    dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE)) %>% 
                    na.exclude() %>% 
                    dplyr::group_by(region_code) %>% 
                    dplyr::arrange(region_code, year, month) %>% 
                    dplyr::mutate(changes = c(0, diff(total))) %>% 
                    dplyr::group_by(region_code) %>% 
                    dplyr::mutate(previous_total = rev(total)) %>% 
                    dplyr::mutate(changes_percent = paste(round(100 * (changes / previous_total), 2) * sign(previous_total), "%", sep="")) %>% 
                    data.frame()
                debt_test <<- debt
                debt$changes_percent[grepl("NaN", debt$changes_percent)] <- NA
                debt_plus <- paste0("+", debt$changes_percent)
                debt$changes_percent[debt$changes > 0] <- debt_plus[debt$changes > 0]
                debt$color <- sapply(debt$changes, function(x) {ifelse(x >= 0, "red", "green")})
                debt <- filter(debt, month == month2, year == year2)

                map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
                map_extended@data$id <-  rownames(map_extended@data)
                
                
                # Шукаємо центроїди для тексту
                cents <- coordinates(map_extended)
                cents <- SpatialPointsDataFrame(coords=cents, data=map_extended@data)
                cents_table <- broom::tidy(cents, region = "id")
                # Корегуємо розташування центроїдів для Київської та Одеської області
                cents_table$coords.x2[cents_table$region_code == "10"] <- cents_table$coords.x2[cents_table$region_code == "10"] - 0.5
                cents_table$coords.x2[cents_table$region_code == "16"] <- cents_table$coords.x2[cents_table$region_code == "16"] + 0.25
                cents_table$coords.x1[cents_table$region_code == "16"] <- cents_table$coords.x1[cents_table$region_code == "16"] + 0.4
                cents_table$coords.x1[cents_table$region_code == "18"] <- cents_table$coords.x1[cents_table$region_code == "18"] + 0.2
                cents_table$coords.x2[cents_table$region_code == "18"] <- cents_table$coords.x2[cents_table$region_code == "18"] + 0.3
                cents_table$coords.x1[cents_table$region_code == "19"] <- cents_table$coords.x1[cents_table$region_code == "19"] - 0.1
                
                map_extended@data$total <- make_unique(map_extended@data$total, map_extended@data$UA_NAME)
                # orange palette
                #sc <- scale_fill_continuous(low = "#fff7bc", high = "#d95f0e", breaks = map_extended@data$total) 
                sc <- scale_fill_gradient2(low = "#542788", high = "#d95f0e", breaks = map_extended@data$total, midpoint = 0) 
                map_extended@data$color <- sc$palette(sc$rescaler(map_extended@data$total))
                scale_value <- map_extended@data$color
                names(scale_value) <- as.character(map_extended@data$total)
                ukr_df <- broom::tidy(map_extended, region = "id")
                ukr_df <- plyr::join(ukr_df, map_extended@data, by = "id") %>% 
                    dplyr::arrange(desc(total))
                #millions <- as.character(round(ukr_df$debt_total / 1000000, 1))
                billions <- ukr_df$total / 1000000
                millions <- ukr_df$total / 1000
                number_string <- paste(ukr_df$UA_NAME, "–", format(round(ukr_df$total), trim = TRUE, big.mark = " ", nsmall = 0), "тис.")
                number_string[abs(millions) > 1] <- paste(ukr_df$UA_NAME[abs(millions) > 1], "–", 
                                                     format(round(millions[abs(millions) > 1],1), trim = TRUE, big.mark = " "), "млн")
                number_string[abs(billions) > 1] <- paste(ukr_df$UA_NAME[abs(billions) > 1], "–", 
                                                     format(round(billions[abs(billions) > 1],1), trim = TRUE, big.mark = " "), "млрд")
                ukr_df$label <- number_string
                
                
                # ukr_df$label <- mapply(function(d,r){
                #   billions <- format(round(d / 1000000, 1), decimal.mark = ",", big.mark = " ")
                #   paste(r, "–", billions, "")
                # }, ukr_df$debt_total, ukr_df$UA_NAME)
                subtitle <- paste0("Дані станом на ", 
                                   second_date_string, ". Відсотки відображають зміну з ", date1_gen, ".\nЗавантажити дані: https://data.gov.ua/dataset/af62767b-95b7-4d47-a941-0dc00a9213e6")
                ukr_df$total <- factor(as.character(ukr_df$total), 
                                            levels = as.character(unique(ukr_df$total)), ordered = TRUE)
                legend_labels <- factor(unique(ukr_df$label), levels = unique(ukr_df$label), ordered = TRUE)
                pl <<- ggplot(ukr_df, aes(x = long, y= lat, group = group, fill = total)) + 
                    ggtitle(params$title, subtitle = capitalize(paste(cats, collapse = ", "))) +
                    geom_polygon() +
                    geom_polygon(data = subset(ukr_df, !(id %in% c(10,19)))) + 
                    geom_polygon(data = subset(ukr_df, (id %in% c(10,19)))) + 
                    geom_path(color = "black", size = 0.05) +
                    geom_text(data = dplyr::filter(cents_table, region_code != "9", region_code != "19"), 
                              aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), inherit.aes = FALSE, size = 2.5, 
                              family = "Bliss Pro Light") +
                    geom_text_repel(data = dplyr::filter(cents_table, region_code == "9"), 
                                    aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), 
                                    inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.3, size = 2.5, segment.size = 0.15,
                                    family = "Bliss Pro Light", min.segment.length = 0.2) +
                    geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                                    aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), 
                                    inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, size = 2.5, segment.size = 0.15,
                                    family = "Bliss Pro Light", min.segment.length = 0.2) +
                    scale_fill_manual(name = params$legend_title, values = scale_value, labels = legend_labels) +
                    scale_color_manual(values = c(green = "darkgreen", red = "darkred"), guide=FALSE) +
                    theme(legend.key.size = unit(0.14, units = "inches")) +
                    guides(fill = guide_legend(ncol = 1)) +
                    theme(axis.line = element_blank(), 
                          axis.text = element_blank(),
                          axis.ticks = element_blank(), axis.title = element_blank(),
                          rect = element_blank(),
                          text = element_text(family = "Bliss Pro Light"),
                          legend.text = element_text(size = 8.5),
                          plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                          legend.title = element_text(face = "bold"),
                          #plot.subtitle = element_blank(),
                          plot.caption = element_text(color = "dimgrey", size = 8)) +
                    labs(caption = subtitle) +
                    coord_map()
                updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
                updateTextInput(session, inputId = "customsubtitle", value = pl$labels$subtitle)
                
                pl_local <- ggplot_gtable(ggplot_build(pl))
                leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
                caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
                pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
                pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 3
                pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
                pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
                pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
                enable("downloadData")
                grid.draw(pl_local)
            } else { if (map_type == "зміни графіком") {
                params <- switch (var,
                                  debt_month_start = list(units = "грн", title = "Сумарна заборгованість за електроенергію, грн", legend_title = "Борг, грн"),
                                  consumption_tkwth = list(units = "кВт/год", title = "Споживання електроенергії, кВт/год", legend_title = "Споживання, кВт/год"),
                                  consumption_tuah = list(units = "грн", title = "Вартість спожитої електроенергії, грн", legend_title = "Споживання, грн"))

                debt <- debt %>% 
                    dplyr::filter(to_date(year, month) <= to_date(year2, month2), to_date(year, month) >= to_date(year1, month1)) %>% 
                    dplyr::group_by(region_code, region, UA_NAME, year, month) %>% 
                    dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE)) %>% 
                    na.exclude()
                ukraine_grid <- read.csv("grid.csv")
                names(debt)[names(debt) == "region"] <- "name"
                debt$name <- gsub("місто ", "", debt$name)
                #debt$name[debt$name == "місто Київ"] <- "Київ"
                #debt$name[debt$name == "місто Севастополь"] <- "Севастополь"
                #print(unique(debt$name))
                monthes <- as.character(unique(debt$month))
                #print(monthes)
                monthes[nchar(monthes) == 1] <- paste0("0", monthes)
                pseudo_dates <- as.Date(paste("2001", monthes, "01", sep = "-"))
                debt$year <- as.character(debt$year)
                debt$month <- as.character(debt$month)
                debt$month[nchar(debt$month) == 1] <- paste0("0", debt$month[nchar(debt$month) == 1])
                #debt$date <- as.Date(paste(debt$year, debt$month, "01"), format = "%Y %m %d")
                date_range <- range(pseudo_dates)
                date_range <- gsub("[[0-9]]{4}", "2001", as.character(date_range))
                date_range <- as.Date(date_range)
                dates_diff <- as.numeric(date_range[2] - date_range[1])
                date_breaks <-  c(date_range[1] + round(dates_diff / 10), mean(date_range), date_range[2] - round(dates_diff / 10))
                
                #debt_last <- dplyr::filter(debt, date == max(date))
                subtitle <- paste0('Зміни з ', 
                                   date1_gen, " до ", date2_gen, ".\nЗавантажити дані: https://data.gov.ua/dataset/af62767b-95b7-4d47-a941-0dc00a9213e6")
                pr_br <- pretty(debt$total, n = 5)
                debt$date <- as.Date(paste("2001", debt$month, "01"), format = "%Y %m %d")
                pl <<- ggplot(debt, aes(x = date, y = total, col = year)) +
                    geom_line(alpha = 0.6, se = FALSE, size = 0.5) +
                    ggtitle(params$title, subtitle = capitalize(paste(cats, collapse = ", "))) +
                    facet_geo(~name, grid = ukraine_grid, scales = "fixed") +
                    scale_x_date(breaks = date_breaks, labels = date_format, limits = date_range) +
                    scale_y_continuous(trans = tr_cbrt, 
                                       minor_breaks = NULL,
                                       breaks = pr_br,
                                       labels = function(br){
                                           br <- pr_br
                                           all_labels <- sapply(br, function(d) {
                                               millions_n <- round(d / 1000, 1)
                                               millions <- format(millions_n, decimal.mark = ",", big.mark = "")
                                               billions_n <- round(d / 1000000, 1)
                                               billions <- format(billions_n, decimal.mark = ",", big.mark = "")
                                               ifelse(abs(billions_n) >= 1, paste0(billions, " млрд"), ifelse(abs(millions_n) > 0, paste0(millions, " млн"), ifelse(d != 0, paste0(d, " тис"), "0"  )))})
                                           null_number <- which(all_labels == "0")
                                           first_non_na <- which(!is.na(br))[1]
                                           labels <- all_labels[(first_non_na + 1):(length(all_labels)-1)]
                                           reversed_elements <- rev(seq_along(labels))
                                           labels_to_exclude <- reversed_elements[seq_along(reversed_elements) %% 3 != 0 ]
                                           
                                           labels[labels_to_exclude[labels_to_exclude != null_number & labels_to_exclude != 1]] <- ""
                                           c(all_labels[1:first_non_na], labels, all_labels[length(all_labels)])
                                       }
                                       ) +
                    theme(
                        plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                        axis.ticks = element_blank(), axis.title = element_blank(),
                        rect = element_rect(fill = "white"),
                        legend.position = "bottom",
                        text = element_text(family = "Bliss Pro Light"),
                        axis.text.y = element_text(size = 8, vjust = 1),
                        axis.text.x = element_text(vjust = 0),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        panel.background = element_rect(fill =  "#deebf7"),
                        strip.background = element_rect(fill = "#c6dbef"),
                        legend.title = element_blank()) +
                    labs(caption = subtitle)
                pl_test <<- pl
                updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
                updateTextInput(session, inputId = "customsubtitle", value = pl$labels$subtitle)
                pl_local <- print.facet_geo(pl)
                leftest_panel <- min(pl_local$layout$l[grepl("panel-", pl_local$layout$name)])
                caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
                pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2 , leftest_panel, caption_row, leftest_panel, name = "logo")
                pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
                pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
                enable("downloadData")
                grid.draw(pl_local)
            } else {
                if (map_type == "зміни кольором") {
                    params <- switch (var,
                                      debt_month_start = list(units = "грн", title = "Зміни сумарної заборгованості за електроенергію", legend_title = "Зміна боргу, грн",
                                                              caption = paste0("Різниця у сумарній заборгованості за електроенергію за ", date_string, " та ", second_date_string, ".")),
                                      consumption_tkwth = list(units = "кВт/год", title = "Зміни споживання електроенергії", legend_title = "Зміна споживання, кВт/год",
                                                               caption = paste0("Різниця у споживанні електроенергії за ", date_string, " та ", second_date_string, ".")),
                                      consumption_tuah = list(units = "грн", title = "Зміни вартості спожитої електроенергії", legend_title = "Зміна вартості, грн",
                                                              caption = paste0("Різниця у вартості спожитої електроенергії за ", date_string, " та ", second_date_string, ".")))
                    debt <- debt %>%
                        dplyr::filter( (year == year1 & month == month1) | (year == year2 & month == month2)) %>% 
                        dplyr::group_by(region_code, region, UA_NAME, year, month) %>% 
                        dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE)) %>% 
                        na.exclude() %>% 
                        dplyr::group_by(region_code) %>% 
                        dplyr::arrange(region_code, year, month) %>% 
                        dplyr::mutate(changes = c(0,diff(total))) %>% 
                        dplyr::group_by(region_code) %>% 
                        dplyr::mutate(previous_total = rev(total)) %>% 
                        dplyr::mutate(changes_percent = paste(round(100 * (changes / previous_total), 2) * sign(previous_total), "%", sep="")) %>% 
                        data.frame()
                        
                    debt <- filter(debt, year == year2, month == month2)
                    debt$changes <- make_unique(debt$changes, debt$region)
                    map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
                    map_extended@data$id <-  rownames(map_extended@data)
                    ukr_df <- broom::tidy(map_extended, region = "id")
                    total_df <- join(ukr_df, map_extended@data, by = "id") %>% 
                        dplyr::arrange(desc(total))
                    changes_df <- join(ukr_df, map_extended@data, by = "id") %>% 
                        dplyr::arrange(desc(changes))
                    sc_changes <- scale_colour_gradient2(low = "#2c7bb6", high = "#d7191c", breaks = changes_df$changes) 
                    changes_df$color_changes <- sc_changes$palette(sc_changes$rescaler(changes_df$changes))
                    scale_changes_value <- changes_df$color_changes
                    names(scale_changes_value) <- as.character(changes_df$changes)
                    
                    billions <- changes_df$changes / 1000000
                    millions <- changes_df$changes / 1000
                    number_string <- paste(changes_df$UA_NAME, "–", format(round(changes_df$changes), trim = TRUE, big.mark = " ", nsmall = 0), "тис.")
                    number_string[millions > 0] <- paste(changes_df$UA_NAME, "–", paste0("+", format(round(changes_df$changes), trim = TRUE, big.mark = " ", nsmall = 0)), "тис.")
                    number_string[millions > 1] <- paste(changes_df$UA_NAME[millions > 1], "–", 
                                                         paste0("+", format(round(millions[millions > 1], 1), trim = TRUE, big.mark = " ")), "млн")
                    number_string[billions > 1] <- paste(changes_df$UA_NAME[billions > 1], "–", 
                                                         paste0("+", format(round(billions[billions > 1], 1), trim = TRUE, big.mark = " ")), "млрд")
                    number_string[millions < -1] <- paste(changes_df$UA_NAME[millions < -1], "–", 
                                                         paste0(format(round(millions[millions < -1], 1), trim = TRUE, big.mark = " ")), "млн")
                    number_string[billions < -1] <- paste(changes_df$UA_NAME[billions < -1], "–", 
                                                         paste0(format(round(billions[billions < -1], 1), trim = TRUE, big.mark = " ")), "млрд")
                    changes_df$label <- number_string
                    
                    #changes_millions <- as.character(round(changes_df$changes / 1000000, 1))
                    
                    
                    
                    #changes_df$label <-  paste(changes_df$UA_NAME, "–", ifelse(changes_millions > 0, paste0("+", changes_millions), changes_millions), "")
                    changes_df$changes <- factor(as.character(changes_df$changes), 
                                                 levels = as.character(unique(changes_df$changes)), ordered = TRUE)
                    changes_labels <- factor(unique(changes_df$label), levels = unique(changes_df$label), ordered = TRUE)
                    subtitle <- paste0(params$caption, 
                                       ".\nЗавантажити дані: https://data.gov.ua/dataset/af62767b-95b7-4d47-a941-0dc00a9213e6")
                    pl <<- ggplot(changes_df, aes(x = long, y= lat, group = group, fill = changes)) + 
                        #ggtitle(ifelse(length(cats) > 0, paste(params$title, paste(cats, collapse = ", "), sep = " - "), params$title)) +
                        ggtitle(params$title, subtitle = capitalize(paste(cats, collapse = ", "))) +
                        geom_polygon() +
                        geom_polygon(data = subset(changes_df, !(id %in% c(10,19)))) + 
                        geom_polygon(data = subset(changes_df, (id %in% c(10,19)))) +
                        geom_path(color = "black", size = 0.05) +
                        scale_fill_manual(name = params$legend_title, values = scale_changes_value, labels = changes_labels) +
                        theme(legend.key.size = unit(0.14, units = "inches")) +
                        geom_text(data = dplyr::filter(cents_table, region_code != "09", region_code != "9", region_code != "19"), 
                                  aes(x = coords.x1, y = coords.x2, label = UA_NAME), inherit.aes = FALSE, family = "Bliss Pro Light",
                                  size = 2) +
                        geom_text_repel(data = dplyr::filter(cents_table, region_code == "9"), 
                                        aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                                        inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.5, segment.size = 0.15,
                                        family = "Bliss Pro Light", min.segment.length = 0.2, size = 2) +
                        geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                                        aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                                        inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, segment.size = 0.15,
                                        family = "Bliss Pro Light", min.segment.length = 0.2, size = 2) +
                        guides(fill = guide_legend(ncol = 1)) +
                        theme(axis.line = element_blank(), 
                              axis.text = element_blank(),
                              axis.ticks = element_blank(), axis.title = element_blank(),
                              rect = element_blank(),
                              text = element_text(family = "Bliss Pro Light"),
                              legend.text = element_text(size = 8.5),
                              plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                              legend.title = element_text(face = "bold"),
                              #plot.subtitle = element_blank(),
                              plot.caption = element_text(color = "dimgrey", size = 8)) +
                        labs(caption = subtitle) +
                        coord_map()
                    updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
                    updateTextInput(session, inputId = "customsubtitle", value = pl$labels$subtitle)
                    pl_local <- ggplot_gtable(ggplot_build(pl))
                    leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
                    caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
                    pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
                    pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 3
                    #pl_local$grobs[[which(pl_local$layout$name == "logo")]]$vjust <- 0
                    pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
                    pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
                    pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
                    enable("downloadData")
                    grid.draw(pl_local)
                } else {
                    if ((map_type == "стан")) {
                        params <- switch (var,
                                          debt_month_start = list(units = "грн", title = "Сумарна заборгованість за електроенергію", legend_title = "Борг, грн"),
                                          consumption_tkwth = list(units = "кВт/год", title = "Споживання електроенергії", legend_title = "Споживання, кВт/год"),
                                          consumption_tuah = list(units = "грн", title = "Вартість спожитої електроенергії", legend_title = "Вартість, грн"))
                        debt <- debt %>%  
                            filter(year == year2, month == month2) %>% 
                            dplyr::group_by(region_code, region, UA_NAME, year, month) %>% 
                            dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE)) %>% 
                            na.exclude() %>% 
                            data.frame()
                        debt$total <- make_unique(debt$total, debt$region)
                        map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
                        map_extended@data$id <-  rownames(map_extended@data)
                        #sc <- scale_fill_continuous(low = "#fff7bc", high = "#d95f0e", breaks = map_extended@data$total) 
                        sc <- scale_fill_gradient2(low = "#542788", high = "#d95f0e", breaks = map_extended@data$total, midpoint = 0) 
                        map_extended@data$color <- sc$palette(sc$rescaler(map_extended@data$total))
                        scale_value <- map_extended@data$color
                        names(scale_value) <- as.character(map_extended@data$total)
                        ukr_df <- broom::tidy(map_extended, region = "id")
                        ukr_df <- plyr::join(ukr_df, map_extended@data, by = "id") %>% 
                            dplyr::arrange(desc(total))
                        billions <- ukr_df$total / 1000000
                        millions <- ukr_df$total / 1000
                        number_string <- paste(ukr_df$UA_NAME, "–", format(round(ukr_df$total), trim = TRUE, big.mark = " ", nsmall = 0), "тис.")
                        number_string[abs(millions) > 1] <- paste(ukr_df$UA_NAME[abs(millions) > 1], "–", 
                                                                  format(round(millions[abs(millions) > 1],1), trim = TRUE, big.mark = " "), "млн")
                        number_string[abs(billions) > 1] <- paste(ukr_df$UA_NAME[abs(billions) > 1], "–", 
                                                                  format(round(billions[abs(billions) > 1],1), trim = TRUE, big.mark = " "), "млрд")
                        ukr_df$label <- number_string
                        subtitle <- paste0("Дані станом на ", 
                                           second_date_string, ".", ".\nЗавантажити дані: https://data.gov.ua/dataset/af62767b-95b7-4d47-a941-0dc00a9213e6")
                        ukr_df$total <- factor(as.character(ukr_df$total), 
                                                    levels = as.character(unique(ukr_df$total)), ordered = TRUE)
                        legend_labels <- factor(unique(ukr_df$label), levels = unique(ukr_df$label), ordered = TRUE)
                        pl <<- ggplot(ukr_df, aes(x = long, y= lat, group = group, fill = total)) + 
                            ggtitle(params$title, subtitle = capitalize(paste(cats, collapse = ", "))) +
                            geom_polygon() +
                            geom_polygon(data = subset(ukr_df, !(id %in% c(10,19)))) + 
                            geom_polygon(data = subset(ukr_df, (id %in% c(10,19)))) + 
                            geom_path(color = "black", size = 0.05) +
                            geom_text(data = dplyr::filter(cents_table, region_code != "9", region_code != "19"), 
                                      aes(x = coords.x1, y = coords.x2, label = UA_NAME), inherit.aes = FALSE, size = 2, 
                                      family = "Bliss Pro Light") +
                            geom_text_repel(data = dplyr::filter(cents_table, region_code == "9"), 
                                            aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                                            inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.3, size = 2, segment.size = 0.15,
                                            family = "Bliss Pro Light", min.segment.length = 0.2) +
                            geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                                            aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                                            inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, size = 2, segment.size = 0.15,
                                            family = "Bliss Pro Light", min.segment.length = 0.2) +
                            scale_fill_manual(name = params$legend_title, values = scale_value, labels = legend_labels) +
                            scale_color_manual(values = c(green = "darkgreen", red = "darkred"), guide=FALSE) +
                            theme(legend.key.size = unit(0.14, units = "inches")) +
                            guides(fill = guide_legend(ncol = 1)) +
                            theme(axis.line = element_blank(), 
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(), axis.title = element_blank(),
                                  rect = element_blank(),
                                  text = element_text(family = "Bliss Pro Light"),
                                  legend.text = element_text(size = 8.5),
                                  plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                                  legend.title = element_text(face = "bold"),
                                  #plot.subtitle = element_blank(),
                                  plot.caption = element_text(color = "dimgrey", size = 8)) +
                            labs(caption = subtitle) +
                            coord_map()
                        updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
                        updateTextInput(session, inputId = "customsubtitle", value = pl$labels$subtitle)
                        pl_local <- ggplot_gtable(ggplot_build(pl))
                        leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
                        caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
                        pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
                        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 3
                        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
                        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
                        pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
                        enable("downloadData")
                        grid.draw(pl_local)
                    }
                }
            }
            }
        })
    })
    
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            return(paste("map", input$extension, sep = "."))
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            pl <- pl + ggtitle(isolate(input$customtitle), isolate(input$customsubtitle))
            if (input$goal == "facebook") {
                w = 8.5
                h = 4.85
                dp = 130
                if (is.null(pl$data$facet_col)) {
                    pl <- pl + theme(legend.text = element_text(size = 8.5),
                                     legend.title = element_text(face = "bold", size = 9),
                                     legend.box.spacing = unit(1.2, "cm"),
                                     plot.title = element_text(size = 12)) +
                        scale_y_continuous(expand = c(0,0)) + 
                        scale_x_continuous(expand = c(0,0))
                    hjust_cor <- -1.3
                    vjust <- 0.28
                    pl_local <- ggplot_gtable(ggplot_build(pl))
                } else {
                    hjust_cor <- -3.6
                    vjust <- 0.4
                    pl <- pl +
                        theme(axis.text = element_text(size = 6),
                              axis.text.y = element_text(size = 6),
                              panel.grid.major = element_line(size = 0.1),
                              panel.grid.minor = element_line(size = 0.1),
                              strip.text = element_text(size = 6, lineheight = 0.1),
                              legend.box.spacing = unit(0.0, "cm"),
                              panel.spacing.x = unit(0.2, "lines"),
                              panel.spacing.y = unit(0.2, "lines"),
                              plot.title = element_text(hjust = 0),
                              plot.caption = element_text(size = 8, family = "Bliss Pro Light"))
                    pl_local <- print.facet_geo(pl)
                    
                    strips <- which(grepl("strip-", pl_local$layout$name))
                    tops <- unique(pl_local$layout$t[strips])
                    for (i in strips) {
                        un <- attr(pl_local$grobs[[i]]$heights, "unit")
                        n <- as.numeric(pl_local$grobs[[i]]$heights)
                        pl_local$grobs[[i]]$heights <- unit(n/2, un)  
                    }
                    
                    for (i in tops) {
                        un <- attr(pl_local$heights[[i]], "unit")
                        n <- as.numeric(pl_local$heights[[i]])
                        pl_local$heights[[i]] <- unit(n/2, un)  
                    }
                }
            } else {if (input$goal == "презентація") {
                w = 8
                h = 5.66
                dp = 300
                if (is.null(pl$data$facet_col)) {
                    pl <- pl + 
                        scale_y_continuous(expand = c(0,1)) + 
                        scale_x_continuous(expand = c(0,0)) +
                        theme(legend.text = element_text(size = 8.5),
                              legend.title = element_text(face = "bold", size = 9),
                              plot.title = element_text(size = 13, hjust = 0.5))
                    hjust_cor <- -1
                    vjust <- 0.25
                    pl_local <- ggplot_gtable(ggplot_build(pl))
                } else {
                    hjust_cor <- -2.9
                    vjust <- 0.35
                    pl <- pl +
                        theme(axis.text = element_text(size = 6),
                              axis.text.y = element_text(size = 6),
                              panel.grid.major = element_line(size = 0.1),
                              panel.grid.minor = element_line(size = 0.1),
                              strip.text = element_text(size = 6, lineheight = 0.1),
                              legend.box.spacing = unit(0.0, "cm"),
                              panel.spacing.x = unit(0.2, "lines"),
                              panel.spacing.y = unit(0.2, "lines"),
                              plot.title = element_text(hjust = 0),
                              plot.caption = element_text(size = 8, family = "Bliss Pro Light"))
                    pl_local <- print.facet_geo(pl)
                    strips <- which(grepl("strip-", pl_local$layout$name))
                    tops <- unique(pl_local$layout$t[strips])
                    for (i in strips) {
                        un <- attr(pl_local$grobs[[i]]$heights, "unit")
                        n <- as.numeric(pl_local$grobs[[i]]$heights)
                        pl_local$grobs[[i]]$heights <- unit(n/2, un)  
                    }
                    for (i in tops) {
                        un <- attr(pl_local$heights[[i]], "unit")
                        n <- as.numeric(pl_local$heights[[i]])
                        pl_local$heights[[i]] <- unit(n/2, un)  
                    }
                    
                }
            }}
            
            leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
            caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
            pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
            logo_side <- h / 5 
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(logo_side, "in")
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(logo_side, "in")
            
            hjust <- (w / 2) / logo_side + hjust_cor
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- hjust
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$vjust <- vjust
            
            # Write to a file specified by the 'file' argument
            #print(file)
            #png(file = "myplot.png", bg = "transparent")
            if (input$extension == "pdf") {
                dev <- cairo_pdf
            } else {
                dev <- "png"
            }
            pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
            ggsave(file, pl_local, width = w, height = h, dpi = dp, device = dev)
        }
    )
    
}

# Create Shiny app ----
shinyApp(ui, server)
