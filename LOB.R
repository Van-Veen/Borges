library(rvest)
library(magrittr)

borges <- function(){
  
  url <- "https://libraryofbabel.info/libraryofbabel.html"
  
  TEXT <- read_html(url) %>% 
    html_nodes(., ".borges") %>% 
    html_text(.) %>% 
    cat(., "\n")
  
  return(TEXT)
}


genWord <- function(){
  
  charArray <- c(letters, ".", "?")
  wordLength <- seq(1, 13, 1)
  
  selectedLength <- sample(wordLength, 1)
  selectedWord <- sample(charArray, selectedLength, T, prob = c(0.022, 0.029, rep(0.037, 24), 0.01308, 0.00292))
  
  hasPeriod <- ifelse("." %in% selectedWord, T, F)
  
  if(hasPeriod == T){
    selectedWord <- selectedWord[selectedWord != "."]
    selectedWord <- selectedWord[selectedWord != "?"]
    selectedWord <- paste(selectedWord, collapse = "")
    selectedWord <- paste(selectedWord, ".", sep = "")
  }
  
  hasQuestion <- ifelse("?" %in% selectedWord, T, F)
  
  if(hasQuestion == T){
    selectedWord <- selectedWord[selectedWord != "."]
    selectedWord <- selectedWord[selectedWord != "?"]
    selectedWord <- paste(selectedWord, collapse = "")
    selectedWord <- paste(selectedWord, "?", sep = "")
  }
  
  if(hasPeriod == F & hasQuestion == F){
    selectedWord <- paste(selectedWord, collapse = "")
  }
  
  return(selectedWord)
  
  
}

genSentence <- function(){
  
  sentenceStop <- F
  wordContainer <- c()
  
  while(sentenceStop == F){
    
    newWord <- genWord()
    
    if(grepl("\\.|\\?", newWord) == F){
      
      wordContainer <- c(wordContainer, newWord)
      
    }else{
      
      wordContainer <- c(wordContainer, newWord)
      sentenceStop <- T
      
    }
    
  }
  
  wordContainer <- paste(wordContainer, "", collapse = "")
  wordContainer <- trimws(wordContainer)
  firstLetter <- substr(wordContainer, 1, 1)
  wordContainer <- sub(firstLetter, toupper(firstLetter), wordContainer)
  
  return(wordContainer)
  
}

genParagraph <- function(){
  
  paraNums <- seq(1, 8, 1)
  selectedNum <- sample(paraNums, 1, prob = c(.03, .10, .18, .45, .4, .3, .2, .15))
  newSentences <- c()
  
  for(i in 1:selectedNum){
    selectedSentence <- genSentence()
    newSentences <- c(newSentences, selectedSentence)
  }
  
  newSentences <- paste(newSentences, collapse = " ")
  return(newSentences)
  
}



#genPage <- function(){
  
  target <- 80 * 40
  charNum <- 0
  allSentences <- c()
  
  while(charNum < target){
    
    newSentence <- genSentence()
    allSentences <- c(allSentences, newSentence)
    allSentences <- paste(allSentences, collapse = "")
    charNum <- nchar(allSentences)
    
  }
  
  allSentences <- gsub("\\.", "\\. ", allSentences)
  allSentences <- gsub("\\?", "\\? ", allSentences)
  allSentences <- trimws(allSentences)
  
  return(allSentences)
  
}

genPage <- function(){
  
  target <- (80 * 40) - 200
  charNum <- 0
  allSentences <- c()
  
  while(charNum < target){
    
    newParagraph <- genParagraph()
    allSentences <- c(allSentences, newParagraph)
    allSentences2 <- paste(allSentences, collapse = " ")
    charNum <- nchar(allSentences2)
    
  }
  
  allSentences <- paste("<p>", allSentences, "</p>", sep = "")
  allSentences <- paste(allSentences, collapse = " ")
  allSentences <- paste("<div style=\"width:864px;\">", allSentences, "</div>")
  
  return(allSentences)
  
}

genBook <- function(){
  
  finalBook <- c()
  
  for(i in 1:410){
    
    selectedPage <- genPage()
    pageNum <- paste("<i>", "Page #", i, "</i>")
    finalBook <- c(finalBook, selectedPage, pageNum)
    
  }
  
  writeLines(finalBook, "C:/Users/jstewart/Desktop/Test.html")
  
  browseURL("C:/Users/jstewart/Desktop/Test.html")
  
  return(selectedPage)
  
}

genBook()
