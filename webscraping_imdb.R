install.packages('rvest')
library('rvest')
url='http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage=read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html = html_nodes(webpage,'.text-primary') 
#Converting the ranking data to text
rank_data = html_text(rank_data_html)
rank_data
rank_data = as.numeric(rank_data)

#Using CSS selectors to scrap the title section
title_data_html = html_nodes(webpage,'.lister-item-header a') 
#Converting the title data to text
title_data = html_text(title_data_html)
title_data

#Using CSS selectors to scrap the description section
description_data_html = html_nodes(webpage,'.ratings-bar+.text-muted') 
#Converting the description data to text
description_data = html_text(description_data_html)
head(description_data)
description_data=gsub("\n","",description_data)

#Using CSS selectors to scrap the runtime section
runtime_data_html = html_nodes(webpage,'.runtime') 
#Converting the description data to text
runtime_data = html_text(runtime_data_html)
runtime_data=runtime_data[-1]
runtime_data=gsub(" min","",runtime_data)
runtime_data=as.numeric(runtime_data)
runtime_data

#Using CSS selectors to scrap the runtime section
genre_data_html = html_nodes(webpage,'.genre') 
#Converting the description data to text
genre_data = html_text(genre_data_html)
genre_data=gsub("\n","",genre_data)
genre_data
genre_data=gsub(" ","",genre_data)
genre_data=gsub(",.*","",genre_data)
genre_data=as.factor(genre_data)
genre_data

#Using CSS selectors to scrap the ratings section
rating_data_html = html_nodes(webpage,'.ratings-imdb-rating') 
#Converting the description data to text
rating_data = html_text(rating_data_html)
rating_data=gsub("\n","",rating_data)
rating_data
rating_data=gsub(" ","",rating_data)
rating_data=as.numeric(rating_data)

#Using CSS selectors to scrap the votes section
votes_data_html = html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)') 
#Converting the votes data to text
votes_data = html_text(votes_data_html)
votes_data
votes_data=gsub(",","",votes_data)
votes_data=as.numeric(votes_data)

#Using CSS selectors to scrap the director section
director_data_html = html_nodes(webpage,'.text-muted+ p a:nth-child(1)') 
#Converting the votes data to text
director_data = html_text(director_data_html)
director_data=as.factor(director_data)
head(director_data)

#Using CSS selectors to scrap the director section
metascore_data_html = html_nodes(webpage,'.metascore') 
#Converting the metascore data to text
metascore_data = html_text(metascore_data_html)
metascore_data=gsub(" ","",metascore_data)

metascore_data
summary(metascore_data)
head(metascore_data)
length(metascore_data)
for (i in c(39,73,80,89))
{
  a=metascore_data[1:(i-1)]
  b=metascore_data[i:length(metascore_data)]
  metascore_data=append(a,list("NA"))
  metascore_data=append(metascore_data,b)
}
metascore_data=as.numeric(metascore_data)


gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the votes data
length(gross_data)
gross_data


#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)
gross_data=as.numeric(gross_data)
gross_data

for (i in c(17,39,49,52,57,64,66,73,76,77,80,87,88,89)){
  
  a<-gross_data[1:(i-1)]
  
  b<-gross_data[i:length(gross_data)]
  
  gross_data<-append(a,list("NA"))
  
  gross_data<-append(gross_data,b)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)
gross_data
length(gross_data)

movies_df=data.frame(Rank=rank_data, Title = title_data,
                     
                     Description = description_data, Runtime = runtime_data,
                     
                     Genre = genre_data, Rating = rating_data,
                     
                      Votes = votes_data,                                                             
                     
                     Director = director_data, Gross_Earning_in_Mil = gross_data)
str(movies_df)

install.packages("ggplot2")
library('ggplot2')

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)

ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
  geom_point(aes(size=Rating,col=Genre))






