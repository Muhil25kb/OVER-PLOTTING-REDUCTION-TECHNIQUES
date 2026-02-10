
  student_name <- "MUHIL K B"
  roll_no <- "23BAD070"
  title_text <- paste("Name:", student_name, "| Roll No:", roll_no)
  data <- read.csv("C:/Users/Administrator/Downloads/7.social_media_interactions.csv",
                   stringsAsFactors = FALSE)
  print(colnames(data))
  clean_numeric <- function(x) {
    as.numeric(gsub("[^0-9.]", "", x))
  }
  data$Likes <- clean_numeric(data$Likes)
  data$Comments <- clean_numeric(data$Comments)
  data <- data[complete.cases(data$Likes, data$Comments), ]
  dev.new()
  plot(data$Likes, data$Comments,
       col = rgb(0, 0, 1, 0.3),
       pch = 16,
       main = paste("Alpha Blending\n", title_text),
       xlab = "Likes",
       ylab = "Comments")
  dev.new()
  plot(jitter(data$Likes),
       jitter(data$Comments),
       col = "darkgreen",
       pch = 16,
       main = paste("Jittering Technique\n", title_text),
       xlab = "Likes",
       ylab = "Comments")
  dev.new()
  likes_bin <- cut(data$Likes, breaks = 15)
  comments_bin <- cut(data$Comments, breaks = 15)
  freq <- table(likes_bin, comments_bin)
  image(freq,
        main = paste("Aggregation & Binning\n", title_text),
        xlab = "Comments (Binned)",
        ylab = "Likes (Binned)",
        col = heat.colors(25)