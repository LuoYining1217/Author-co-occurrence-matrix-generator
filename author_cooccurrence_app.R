
library(shiny)
library(XML)
library(dplyr)
library(stringr)


ui <- fluidPage(
  titlePanel("作者共现矩阵生成器"),
  sidebarLayout(
    sidebarPanel(
      fileInput("xml_file", "选择XML文件（.txt 或 .xml）", accept = c(".txt", ".xml")),
      textInput("output_path", "保存路径（包括文件名）", value = "D:/AAA留痕/毕业论文数据/矩阵/共现矩阵.csv"),
      actionButton("generate", "生成共现矩阵"),
      downloadButton("download", "下载生成的矩阵")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tableOutput("matrix_preview")
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$generate, {
    req(input$xml_file)  
    req(input$output_path)  
    
    output$status <- renderText("正在处理，请稍候...")
    
    tryCatch({
      
      xml_data <- xmlParse(input$xml_file$datapath)
      
      
      authors <- xpathSApply(xml_data, "//Author/Info/FullName", xmlValue)
      
      
      author_list <- strsplit(authors, ";")
      
      
      cooccurrence <- data.frame(author1 = character(0), author2 = character(0), cooccur = integer(0), stringsAsFactors = FALSE)
      
      
      for (i in 1:length(author_list)) {
        authors_in_paper <- author_list[[i]]
        if (length(authors_in_paper) > 1) {
          author_pairs <- combn(authors_in_paper, 2, simplify = TRUE)
          for (pair in 1:ncol(author_pairs)) {
            author1 <- trimws(author_pairs[1, pair])
            author2 <- trimws(author_pairs[2, pair])
            existing_row <- which((cooccurrence$author1 == author1 & cooccurrence$author2 == author2) |
                                    (cooccurrence$author1 == author2 & cooccurrence$author2 == author1))
            if (length(existing_row) > 0) {
              cooccurrence[existing_row, "cooccur"] <- cooccurrence[existing_row, "cooccur"] + 1
            } else {
              new_row <- data.frame(author1 = author1, author2 = author2, cooccur = 1, stringsAsFactors = FALSE)
              cooccurrence <- rbind(cooccurrence, new_row)
            }
          }
        }
      }
      
      
      authors_unique <- unique(c(cooccurrence$author1, cooccurrence$author2))
      cooccur_matrix <- matrix(0, nrow = length(authors_unique), ncol = length(authors_unique),
                               dimnames = list(authors_unique, authors_unique))
      for (i in 1:nrow(cooccurrence)) {
        author1 <- cooccurrence$author1[i]
        author2 <- cooccurrence$author2[i]
        cooccur_matrix[author1, author2] <- cooccur_matrix[author1, author2] + cooccurrence$cooccur[i]
        cooccur_matrix[author2, author1] <- cooccur_matrix[author2, author1] + cooccurrence$cooccur[i]
      }
      
      
      write.csv(cooccur_matrix, input$output_path)
      
      
      output$matrix_preview <- renderTable({
        as.data.frame(head(cooccur_matrix, 10)) 
      })
      
      
      output$download <- downloadHandler(
        filename = function() {
          paste0("共现矩阵-", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(cooccur_matrix, file)
        }
      )
      
      output$status <- renderText("矩阵生成成功！可以在指定路径查看或下载文件。")
      
    }, error = function(e) {
      output$status <- renderText(paste("发生错误：", e$message))
    })
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server)
