library(stringr)
library(dplyr)

treat_name <- function(x){
  nomes <- str_split(x, ' ') %>% 
    unlist() %>% 
    str_remove('\\.') %>% 
    subset(. != '')

  if(length(nomes) == 1){
    nomes <- c(nomes, nomes)
  }
  first_letters <- paste0(str_extract(nomes, '^[A-Z]'), '.')[-length(nomes)] %>% 
    paste(collapse = ' ')
  nome <- paste(c(nomes[length(nomes)], first_letters), collapse = ', ')
  return(nome)
}

treat_author <- function(x){
  string <- x %>% 
    str_extract(pattern = 'author\\s*=\\s*[\\{\\"].+?[\\}\\"]')
  if(is.na(string)){
    return('')
  }
  else{
    nomes <- str_match(string, 'author\\s*=\\s*[\\{\\"](.+)[\\}\\"]')[2]
    nomes <- str_split(nomes, '\\sand\\s') %>% 
      unlist()
    names1 <- sapply(nomes, treat_name)
    nomes_final <- paste0(names1, collapse = ' and ')
    arquivo_final <- str_replace(x, 'author\\s*=\\s*[\\{\\"].+?[\\}\\"]',
                                 paste0('author = \\{', nomes_final, '\\}'))
    return(arquivo_final)
  }
}

treat_bib <- function(x){
  x <- str_replace_all(x, '(\\s)\\"', '\\1\\{')
  x <- str_replace_all(x, '([A-Za-z\\.0-9])\\"', '\\1\\}')
  
  x
}

files <- list.files('../bib_crawler/download/', full.names = T)
i <- 0
bib_final <- ''
for(f in files){
  i <- i+1
  r <- paste(readLines(f, encoding = 'UTF-8'), collapse = '') %>%
    str_trim
  r <- treat_author(r)
  r <- treat_bib(r)
  bib_final <- paste(bib_final, r, sep = '\n')
}

write(bib_final, file = 'bib_emerald_science.bib')
