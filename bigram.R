suzanne_eng <- readLines("suzanne-cohen-eng.txt")
unigrams <- suzanne_eng %>% paste(collapse=" ") %>% 
  # tokenize by character (strsplit returns a list, so unlist it)
  strsplit(split="") %>% unlist %>% 
  # remove instances of characters you don't care about
  str_remove_all("[,.!'\"]") %>% 
  str_to_lower() %>%
  # make a frequency table of the characters
  table 
letters_space<-c(letters," ")
x<-rep(0,27);names(x)<-letters_space
for (letter in letters_space) x[letter]<-unigrams[letter]
unigrams<-x
barplot(log(unigrams/sum(unigrams,na.rm=TRUE)+1))
suzanne1<-c(" ",suzanne_eng) %>% paste(collapse="") %>% 
  # tokenize by character (strsplit returns a list, so unlist it)
  strsplit(split="") %>% unlist %>% 
  # remove instances of characters you don't care about
  str_remove_all("[,.!'\"]")  %>% 
  str_to_lower()

suzanne2<-c(suzanne_eng," ") %>% paste(collapse="") %>% 
  # tokenize by character (strsplit returns a list, so unlist it)
  strsplit(split="") %>% unlist %>% 
  # remove instances of characters you don't care about
  str_remove_all("[,.!'\"]")  %>% 
  str_to_lower()

bigrams<-table(suzanne2,suzanne1)

X<-matrix(0,27,27)
row.names(X)<-letters_space
colnames(X)<-letters_space
for (letteri in letters_space)
  for (letterj in letters_space)
    if ((letteri %in% row.names(bigrams))&&(letterj %in% row.names(bigrams))) X[letteri,letterj]<-bigrams[letteri,letterj]

bigrams<-X