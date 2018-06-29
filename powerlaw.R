if (!require(poweRlaw)) {
  install.packages("VGAM", repos="http://cran.us.r-project.org")
  install.packages("poweRlaw", repos="http://cran.us.r-project.org")
}
library("poweRlaw")
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(igraph)) {
  install.packages("igraph", repos="http://cran.us.r-project.org")
}
library("igraph")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)


workingDir = '/Users/michaeltauberg/powerlaw/'
setwd(workingDir)


# ------------------------------
# ----------------------
# Music - Billboard data
# ----------------------
# ------------------------------

csvName = "./data/songs_1970_2018_uniq.csv"
data_name = "songs"
dt = read.csv(csvName)
dt = dt[dt$date != "date", ]
dt$weeks = as.integer(as.character(dt$weeks))
#songs = dt[!duplicated(dt[,c('title','artist','weeks')], fromLast=FALSE),] 
songs = dt[order(dt$weeks, decreasing=TRUE),]

# plot top 20
top_songs = songs[1:20,]
top_songs = top_songs[order(top_songs$weeks, decreasing=TRUE),]
top_songs$title = factor(top_songs$title, levels = top_songs$title[order(top_songs$weeks, decreasing=TRUE)])
p = ggplot(top_songs, aes(x=title, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Songs on Billboard Top100 by weeks on the chart")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on Billboard Hot-100") 
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

# plot the full curve
songs$title =seq(1,nrow(songs)) # get ridd of song titles for easy plotting
p = ggplot(songs, aes(x=title, y=weeks)) 
p = p + geom_bar(stat="identity") 
p = p + ggtitle("Top Songs on Billboard Hot 100 (1970-2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on Billboard Hot-100") 
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# Plot by artists  
# remove "featuring xxxx" from artists 
#also maybe think about cases like "Jay-Z, Rihanna & Kanye West" or "Jason Aldean With Kelly Clarkson" 
dt$artist = tolower(dt$artist)
dt$artist = gsub(" featuring.*", "", dt$artist)
dt$artist = gsub(" feat.*", "", dt$artist)
dt$artist = gsub(" with .*", "", dt$artist)
dt$artist = gsub(" & .*", "", dt$artist)
dt$artist = gsub(" \\/ .*", "", dt$artist)
dt$artist = gsub(" x .*", "", dt$artist)
dt$artist = gsub(", .*", "", dt$artist)
dt$artist = gsub(" duet.*", "", dt$artist)
dt$artist = gsub(" co-starring.*", "", dt$artist)
dt$artist = gsub("travi$", "travis", dt$artist)
dt$artist = gsub("jay z", "jay-z", dt$artist)
dt$artist = gsub("\\\"misdemeanor\\\"", "misdemeanor", dt$artist)
dt$artist = gsub(" + .*", "", dt$artist)
dt$artist = gsub("jay-z +.*", "", dt$artist)
dt$artist = gsub(" vs.*", "", dt$artist)
dt$artist = factor(dt$artist)
artists = dt[!duplicated(dt[,c('title','artist')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
artists = ddply(artists,"artist",numcolwise(sum))
artists = artists[order(artists$weeks, decreasing=TRUE),]
data_name = "artists"

# plot top 20
top_artists = artists[1:20,]
top_artists$artist = factor(top_artists$artist, levels = top_artists$artist[order(top_artists$weeks, decreasing=TRUE)])
p = ggplot(top_artists, aes(x=artist, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Musicians by Total Weeks on the Billboard Hot-100")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Total Weeks on Billboard Hot-100") 
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

artists$artist=seq(1,nrow(artists)) 
p = ggplot(artists, aes(x=artist, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle("All Musicians by weeks on Billboard Hot-100 (1970-2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Total Weeks on Billboard Hot-100") 
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6)

# ------------------------------
# ----------------------
# BOOKS - NYT data
# ----------------------
# ------------------------------

csvName = "./data/bestsellers_all_uniq2.csv"
data_name = "fiction_bestsellers"
dt = read.csv(csvName)
dt$weeks_on_list = as.numeric(as.character(dt$weeks_on_list))

books = dt[!duplicated(dt[,c('title','author')], fromLast=FALSE),] 
books = books[order(books$weeks_on_list, decreasing=TRUE),]

#write.csv(books, "books_uniq_weeks.csv", row.names = FALSE)

# plot top 20
top_books = books[1:20,]
top_books = top_books[order(top_books$weeks_on_list, decreasing=TRUE),]
top_books$title = factor(top_books$title, levels = top_books$title[order(top_books$weeks_on_list, decreasing=TRUE)])
p = ggplot(top_books, aes(x=title, y=weeks_on_list)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 NYT Fiction Bestsellers (2011-2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Book") + ylab("Weeks on NYT Bestseller List") 
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

# plot the full curve
books$title =seq(1,nrow(books)) # get ridd of song titles for easy plotting
p = ggplot(books, aes(x=title, y=weeks_on_list)) 
p = p + geom_bar(stat="identity") 
p = p + ggtitle("Top 1000 NYT Fiction Bestsellers (2011-2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Book") + ylab("Weeks on NYT best seller list") 
p = p + scale_y_continuous(limits = c(0, 125)) + scale_x_continuous(limits = c(0, 1000))
#p = p + geom_smooth(method="lm", formula=log(weeks_on_list) ~ log(title), data=books)
#p = p + geom_smooth(method = "lm", formula=log(y) ~ log(x))
#p = p + geom_smooth(method = "nls", formula = nls(y ~ I(exp(1)^(a + b * x))), se = FALSE, data=books, colour="red", start = list(a=0,b=0))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# Plot by author  
authors = dt[!duplicated(dt[,c('title','author')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
authors = ddply(authors,"author",numcolwise(sum))
authors = authors[order(authors$weeks_on_list, decreasing=TRUE),]
data_name = "authors"

# plot top 20
top_authors = authors[1:20,]
top_authors = top_authors[order(top_authors$author, decreasing=TRUE),]
top_authors$author = factor(top_authors$author, levels = top_authors$author[order(top_authors$weeks_on_list, decreasing=TRUE)])
p = ggplot(top_authors, aes(x=author, y=weeks_on_list)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 NYT Bestselling Fiction Authors (2011-2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Fiction Author") + ylab("Total Weeks on NYT Bestseller List") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

authors$author=seq(1,nrow(authors)) # get ridd of song titles for easy plotting
p = ggplot(authors, aes(x=author, y=weeks_on_list)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 500 Authors on NYT best seller list") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Author") + ylab("Total Weeks on NYT Bestseller List") 
p = p + scale_y_continuous(limits = c(0, 125)) + scale_x_continuous(limits = c(0, 500))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6)

# Plot by publisher 
book_publishers = dt[!duplicated(dt[,c('title','author','publisher')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
book_publishers = ddply(book_publishers,"publisher",numcolwise(sum))
book_publishers = book_publishers[order(book_publishers$weeks_on_list, decreasing=TRUE),]
data_name = "book_publishers"

# plot top 20
top_book_publishers = book_publishers[1:20,]
top_book_publishers = top_book_publishers[order(top_book_publishers$publisher, decreasing=TRUE),]
top_book_publishers$publisher = factor(top_book_publishers$publisher, levels = top_book_publishers$publisher[order(top_book_publishers$weeks_on_list, decreasing=TRUE)])
p = ggplot(top_book_publishers, aes(x=publisher, y=weeks_on_list)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Book Publishers by Weeks on NYT Fiction Bestseller List (2011-2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Book Publisher") + ylab("Total Weeks on NYT Bestseller List") 
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

book_publishers$publisher=seq(1,nrow(book_publishers)) # get ridd of song titles for easy plotting
p = ggplot(book_publishers, aes(x=publisher, y=weeks_on_list)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Book Publishers by weeks on NYT best seller list") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Book Publisher") + ylab("Total Weeks on NYT Bestseller List") 
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6)

# ------------------------------
# ----------------------
# Games - VG sales data
# ----------------------
# ------------------------------

csvName = "./data/vgsalesGlobale.csv"
data_name = "video-games"
dt = read.csv(csvName)
dt$Global_Sales = as.numeric(as.character(dt$Global_Sales))

games = ddply(dt,"Name",numcolwise(sum))
games = games[order(games$Global_Sales, decreasing=TRUE),]

# plot top 20
top_games = games[1:20,]
top_games = top_games[order(top_games$Name, decreasing=TRUE),]
top_games$Name = factor(top_games$Name, levels = top_games$Name[order(top_games$Global_Sales, decreasing=TRUE)])
p = ggplot(top_games, aes(x=Name, y=Global_Sales)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Video Games")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Game") + ylab("Global Unit Sales (millions of copies)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

# plot the full curve
games$Name=seq(1,nrow(games)) # get ridd of song titles for easy plotting
p = ggplot(games, aes(x=Name, y=Global_Sales)) + geom_bar(stat="identity") 
p = p + ggtitle("All Video Games since 1980 by sales") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Video Game") + ylab("Global Sales - millions of copies sold") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# remove Wii sports and limit to 5000 games
#games = dt[order(dt$Global_Sales, decreasing=TRUE),]
#games_nowii = games[games$Global_Sales < 80,]
#games_nowii$Name=seq(1,nrow(games_nowii)) # get ridd of song titles for easy plotting
#p = ggplot(games_nowii, aes(x=Name, y=Global_Sales)) + geom_bar(stat="identity") 
#p = p + ggtitle("Top 5000 Video Games by Unit Sales (minus Wii Sports)") + theme(plot.title = element_text(size=18))
#p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
#p = p + xlab("Video Game") + ylab("Global Sales - millions of copies sold") 
#p = p + scale_y_continuous(limits = c(0, 44)) + scale_x_continuous(limits = c(0, 5000))
#ggsave(filename = sprintf("./%s_no_wiisports.png",data_name) , plot=p, width=8, height=6) 

game_publishers = ddply(dt,"Publisher", numcolwise(sum))
game_publishers = game_publishers[order(game_publishers$Global_Sales, decreasing=TRUE),]
data_name = "video-game-publishers"

# plot top 20
top_game_publishers = game_publishers[1:20,]
top_game_publishers$Publisher = factor(top_game_publishers$Publisher, levels = top_game_publishers$Publisher[order(top_game_publishers$Global_Sales, decreasing=TRUE)])
p = ggplot(top_game_publishers, aes(x=Publisher, y=Global_Sales)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Video Game Publishers")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Game Publisher") + ylab("Global Sales (millions of copies)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

game_publishers$Publisher=seq(1,nrow(game_publishers)) # get ridd of song titles for easy plotting
p = ggplot(game_publishers, aes(x=Publisher, y=Global_Sales)) + geom_bar(stat="identity") 
p = p + ggtitle("All Video Game Publishers") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Video Game Publisher") + ylab("Total Global Sales - millions of copies sold") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6)

# ------------------------------
# ----------------------
# Newspapers - AAM circulation data
# ----------------------
# ------------------------------

csvName = "./data/aam2017q4_newspapers_sun.csv"
data_name = "newspapers"
dt = read.csv(csvName)
dt$Circulation = as.integer(dt$Circulation)
papers = dt[order(dt$Circulation, decreasing=TRUE),]
#papers$Newspaper = factor(papers$Newspaper, levels = papers$Newspaper[order(papers$Circulation, decreasing=TRUE)])

# plot top 20
top_papers = papers[1:20,]
top_papers = top_papers[order(top_papers$Circulation, decreasing=TRUE),]
top_papers$Newspaper = factor(top_papers$Newspaper, levels = top_papers$Newspaper[order(top_papers$Circulation, decreasing=TRUE)])
p = ggplot(top_papers, aes(x=Newspaper, y=Circulation)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Newspapers by circulation")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Newspaper") + ylab("Total Circulation") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

# plot the full curve
papers$Newspaper=seq(1,nrow(papers)) # get ridd of song titles for easy plotting
p = ggplot(papers, aes(x=Newspaper, y=Circulation)) + geom_bar(stat="identity") 
p = p + ggtitle("All Major US Newspapers by Circulation") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Newspaper") + ylab("Circulation") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 


# ------------------------------
# ----------------------
# Movies - Wikipedia box office data
# ----------------------
# ------------------------------

csvName = "./data/movies_1970_2018.csv"
data_name = "movies"
dt = read.csv(csvName)
dt = dt[grep("United States", dt$country), ] # US movies only

# clean up the messy wikipedia data as best we can
dt$gross = gsub("\\$","",dt$gross) # remove dollar sign
dt$gross = gsub("\\,","",dt$gross) # remove european style commas for thousands
dt$gross = gsub("\\&nbsp;"," ",dt$gross) 
dt$gross = gsub("\\&nbsp"," ",dt$gross)
dt$gross = gsub("\\{\\{nbsp\\}\\} "," ",dt$gross) 
dt$gross = gsub("nbsp"," ",dt$gross) 
dt$gross = gsub("\\.(\\d)(\\d)(\\d)\\s+billion","\\1\\2\\3000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\.(\\d)(\\d)\\s+billion","\\1\\20000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\.(\\d)\\s+billion","\\100000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\s+billion","000000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("&.*billion","000000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\.(\\d)(\\d)\\s+million","\\1\\20000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\.(\\d)\\s+million","\\100000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\s+million","000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("&.*million","000000",dt$gross,ignore.case = TRUE)
dt$gross = gsub("\\(.*\\)","",dt$gross) # remove parentheses like (U.S. dollars)
dt$gross = gsub("<.*>","",dt$gross) # remove parentheses like <small>(United States)</small>
dt$gross = gsub("\\{.*\\}","",dt$gross) # remove parentheses like {{small|(domestic)}}
dt$gross = as.integer(dt$gross)

movies = dt[order(dt$gross, decreasing=TRUE),]
movies = movies[!duplicated(movies[,c('name','gross')], fromLast=FALSE),] 

# still missing force awakens, infinity war, black panther, deathly hallows, last jedi
# plot top 20
top_movies = movies[1:20,]
top_movies = top_movies[order(top_movies$gross, decreasing=TRUE),]
top_movies$name = factor(top_movies$name, levels = top_movies$name[order(top_movies$gross, decreasing=TRUE)])
p = ggplot(top_movies, aes(x=name, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Movies by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Movie") + ylab("Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

# plot the full curve
movies$name=seq(1,nrow(movies)) # get ridd of song titles for easy plotting
p = ggplot(movies, aes(x=name, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("All Major U.S. Movies by Box Office Gross (1970-2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Movie") + ylab("Box Office Gross (dollars)") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# directors
directors = ddply(movies,"director",numcolwise(sum))
directors = directors[order(directors$gross, decreasing=TRUE),]
directors$director = gsub("\\[\\[","",directors$director)
directors$director = gsub("\\]\\]","",directors$director)
data_name = "directors"
# fix chris columbus
directors$director = gsub("Chris Columbus \\(filmmaker\\)","",directors$director)

top_directors = directors[1:20,]
top_directors = top_directors[order(top_directors$gross, decreasing=TRUE),]
top_directors$director = factor(top_directors$director, levels = top_directors$director[order(top_directors$gross, decreasing=TRUE)])
p = ggplot(top_directors, aes(x=director, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Directors by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Director") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

directors$director=seq(1,nrow(directors)) # get ridd of song titles for easy plotting
p = ggplot(directors, aes(x=director, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("All U.S. Movie Directors by Total Box Office Gross (1970-2018)") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Movie") + ylab("Box Office Gross (dollars)") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# ------------------------------
# ----------------------
# Sports - NBA player salaries
# ----------------------
# ------------------------------

# this one looks more like an exponential decay
#csvName = "./data/nba_salaries_1990_to_2018.csv"
#data_name = "nba-player-salaries"
#dt = read.csv(csvName)
#dt$salary = as.numeric(as.character(dt$salary))/1000000

#nba_2017 = dt[dt$season_start == "2017", ]
#nba_2017 = nba_2017[order(nba_2017$salary, decreasing=TRUE),]
#nba_2017$player = factor(nba_2017$player, levels = nba_2017$player[order(nba_2017$salary, decreasing=TRUE)])

# plot the full curve
#nba_2017$player=seq(1,nrow(nba_2017)) # get ridd of song titles for easy plotting
#p = ggplot(nba_2017, aes(x=player, y=salary)) + geom_bar(stat="identity") 
#p = p + ggtitle("2017 NBA player Salaries") + theme(plot.title = element_text(size=18))
#p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
#p = p + xlab("NBA Player") + ylab("Salary (millions of dollars)") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
#ggsave(filename = sprintf("./%s_2017_curve.png",data_name) , plot=p, width=8, height=6) 

#plot lifetime salaries
#salaries = ddply(dt,"player",numcolwise(sum))
#salaries = salaries[order(salaries$salary, decreasing=TRUE),]

# plot top 20
#top_salaries = salaries[1:20,]
#top_salaries = top_salaries[order(top_salaries$salaries, decreasing=TRUE),]
#top_salaries$player = factor(top_salaries$player, levels = top_salaries$player[order(top_salaries$salary, decreasing=TRUE)])
#p = ggplot(top_salaries, aes(x=player, y=salary)) + geom_bar(stat="identity") 
#p = p + ggtitle("Top 20 NBA Players by lifetime earnings") + theme(plot.title = element_text(size=18))
#p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
#p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
#p = p + xlab("player") + ylab("Lifetime Salary (millions of dollars)") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=8, height=6)

# plot the full curve
#salaries$player=seq(1,nrow(salaries)) # get ridd of song titles for easy plotting
#p = ggplot(salaries, aes(x=player, y=salary)) + geom_bar(stat="identity") 
#p = p + ggtitle("NBA Player Lifetime Earnings") + theme(plot.title = element_text(size=18))
#p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
#p = p + xlab("NBA Player") + ylab("Lifetime Salary (millions of dollars)") 
#p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
#ggsave(filename = sprintf("./%s_all_curve.png",data_name) , plot=p, width=8, height=6) 

# ------------------------------
# ----------------------
# Powerlifting
# ----------------------
# ------------------------------

# plot number of 1,2,3 place finishes at meets
# if result <=3 , make it a 1
# sum over new place col and plot
# sum wilks scores -> misleading


# ------------------------------
# ----------------------
# Billionaires - Forbes data
# ----------------------
# ------------------------------

csvName = "./data/billionaires_2018b.csv"
data_name = "billionaires"
dt = read.csv(csvName)
dt$Net.Worth = as.numeric(as.character(dt$Net.Worth))
billionaires = dt

# plot top 20
top_billionaires = billionaires[1:20,]
top_billionaires$Name = factor(top_billionaires$Name, levels = top_billionaires$Name[order(top_billionaires$Net.Worth, decreasing=TRUE)])
p = ggplot(top_billionaires, aes(x=Name, y=Net.Worth)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 Forbes Billionaires in the world (2018)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Billionaire") + ylab("Net Worth") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png",data_name) , plot=p, width=15, height=10)


# ------------------------------
# ----------------------
# Podcast Publisher - Podtrac data
# ----------------------
# ------------------------------

csvName = "./data/podcasts_podtrac_april2018.csv"
data_name = "podcast_publishers"
dt = read.csv(csvName)
dt$global_unique_streams_and_downloads = as.numeric(as.character(dt$global_unique_streams_and_downloads))

podcasts = dt[order(dt$global_unique_streams_and_downloads, decreasing=TRUE),]
podcasts$publisher = factor(podcasts$publisher, levels = podcasts$publisher[order(podcasts$global_unique_streams_and_downloads, decreasing=TRUE)])
p = ggplot(podcasts, aes(x=publisher, y=global_unique_streams_and_downloads)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Podcast Publishers") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Podcast Publisher") + ylab("Global Unique Streams and Downloads") 
ggsave(filename = sprintf("./%s_top10.png",data_name) , plot=p, width=8, height=6)

# ------------------------------
# ----------------------
# Actors 
# ----------------------
# ------------------------------

csvName = "./data/actors_2017_data.csv"
data_name = "actor_income"
dt = read.csv(csvName)
dt$income = as.numeric(as.character(dt$income))

actors = dt[order(dt$income, decreasing=TRUE),]
actors$actor = factor(actors$actor, levels = actors$actor[order(actors$income, decreasing=TRUE)])
p = ggplot(actors, aes(x=actor, y=income)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Hollywood Actors ") + theme(plot.title = element_text(size=18))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Hollywood Actor") + ylab("Income (millions of dollars)") 
ggsave(filename = sprintf("./%s.png",data_name) , plot=p, width=8, height=6)


# ------------------------------
# ----------------------
# Fit all data to power law curves
# ----------------------
# ------------------------------
#  -------
# use igraph
# ----------

fit_songs = fit_power_law(songs$weeks) 
fit_artists = fit_power_law(artists$weeks) 
fit_books = fit_power_law(books$weeks_on_list)
fit_authors = fit_power_law(authors$weeks_on_list)
fit_book_publishers = fit_power_law(book_publishers$weeks_on_list)
fit_games = fit_power_law(games[1:200,]$Global_Sales)
fit_games2 = fit_power_law(games[200:nrow(games),]$Global_Sales)
fit_game_publishers = fit_power_law(game_publishers$Global_Sales)
fit_papers = fit_power_law(papers$Circulation)
fit_movies = fit_power_law(movies[1:200,]$gross)  # doesn't converge
fit_movies2 = fit_power_law(movies[200:nrow(movies),]$gross)  # doesn't converge
fit_directors = fit_power_law(directors[1:200,]$gross) # doesn't converge
fit_directors2 = fit_power_law(directors[200:nrow(directors),]$gross) # doesn't converge
fit_podcasts = fit_power_law(podcasts$global_unique_streams_and_downloads)
#fit_nba_2017 = fit_power_law(nba_2017$salary)
#fit_nba_salaries = fit_power_law(salaries[1:500,]$salary)
fit_billionaires = fit_power_law(as.numeric(billionaires[1:300,]$Net.Worth)*10) # doesn't converge?


#######################
# use wikipedia function
#######################
# The following function estimates the exponent in R, plotting the log–log data and the fitted line.
pwrdist <- function(u,...) {
  # u is vector of event counts, e.g. how many
  # crimes was a given perpetrator charged for by the police
  fx <- table(u)
  i <- as.numeric(names(fx))
  y <- rep(0,max(i))
  y[i] <- fx
  m0 <- glm(y~log(1:max(i)),family=quasipoisson())
  print(summary(m0))
  sub <- paste("s=",round(m0$coef[2],2),"lambda=",sum(u),"/",length(u))
  plot(i,fx,log="xy",xlab="x",sub=sub,ylab="counts",...)
  grid()
  lines(1:max(i),(fitted(m0)),type="b")
  return(m0)
}

t = pwrdist(artists$weeks)
t = pwrdist(books$weeks_on_list+1)
t = pwrdist(songs$weeks) 
t = pwrdist(games$Global_Sales*1000)

##################
# use poweRlaw lib
##################
# music
songs_pl = displ$new(songs$weeks)
est_songs = estimate_xmin(songs_pl)
plot(songs_pl)
bs = bootstrap_p(songs_pl, no_of_sims=5)
songs_p = bs$p
plot(songs$title,songs$weeks,log="xy")
qqplot(songs$title,songs$weeks)

artists_pl = displ$new(artists$weeks)
est_artists = estimate_xmin(artists_pl)
plot(artists_pl)
bs = bootstrap_p(artists_pl, no_of_sims=5)
artists_p = bs$p
plot(artists$artist,artists$weeks,log="xy")
plot = ggplot(artists, aes(x=artist, y=weeks)) + geom_point(alpha=0.5) 
plot = plot +  geom_smooth(method = "lm", formula=y ~ log(x))
qqplot(artists$artist,artists$weeks)

# books
books_pl = displ$new(books$weeks_on_list+1)
est_books = estimate_xmin(books_pl)
plot(books_pl)
bs = bootstrap_p(books_pl, no_of_sims=5)
books_p = bs$p
plot(books$title,books$weeks_on_list+1,log="xy")
qqplot(books$title,books$weeks_on_list+1)

# authors
authors_pl = displ$new(authors$weeks_on_list + 1)
est_authors = estimate_xmin(authors_pl)
plot(authors_pl)
bs = bootstrap_p(authors_pl, no_of_sims=5)
authors_p = bs$p
plot(authors$author,authors$weeks_on_list+1,log="xy")

# book_publishers
book_publishers_pl = displ$new(book_publishers$weeks_on_list + 1)
est_book_publishers = estimate_xmin(book_publishers_pl)
plot(book_publishers_pl)
bs = bootstrap_p(book_publishers_pl, no_of_sims=5)
book_publishers_p = bs$p
plot(book_publishers$publisher,book_publishers$weeks_on_list+1,log="xy")

# games
games_pl = displ$new(round(games$Global_Sales*1000))
est_games = estimate_xmin(games_pl)
bs = bootstrap_p(games_pl, no_of_sims=5)
games_p = bs$p
plot(games_pl)
plot(games$Name,games$Global_Sales,log="xy")

games_pl2 = displ$new(round(games[1:200,]$Global_Sales*1000))
est_games2 = estimate_xmin(games_pl2)
bs = bootstrap_p(games_pl2, no_of_sims=5)
games_p = bs$p
plot(games_pl2)

# game publisherse
game_publishers_pl = displ$new(round(game_publishers$Global_Sales*1000))
est_game_publishers = estimate_xmin(game_publishers_pl)
bs = bootstrap_p(game_publishers_pl, no_of_sims=5)
game_publishers_p = bs$p
plot(game_publishers_pl)
plot(game_publishers$Publisher,game_publishers$Global_Sales,log="xy")

game_publishers_pl2 = displ$new(round(game_publishers[1:200,]$Global_Sales*1000))
est_game_publishers2 = estimate_xmin(game_publishers_pl2)
bs = bootstrap_p(game_publishers_pl2, no_of_sims=5)
game_publishers_p2 = bs$p
plot(game_publishers_pl2)

# newspapers
newspapers_pl = displ$new(round(papers$Circulation))
est_newspapers = estimate_xmin(newspapers_pl)
bs = bootstrap_p(newspapers_pl, no_of_sims=5)
newspapers_p = bs$p
plot(newspapers_pl)
plot(papers$Newspaper,papers$Circulation,log="xy")
lines(newspapers_pl)

# movies
movies = movies[!is.na(movies$gross),]
movies_pl = displ$new(round(movies$gross))
est_movies = estimate_xmin(movies_pl)
bs = bootstrap_p(movies_pl, no_of_sims=5)
movies_p = bs$p
plot(movies_pl)
plot(movies$name,movies$gross,log="xy")

movies_pl2 = displ$new(round(movies[1:1000,]$gross))
est_movies2 = estimate_xmin(movies_pl2)
bs = bootstrap_p(movies_pl2, no_of_sims=5)
movies_p2 = bs$p

movies_pl3 = displ$new(round(movies[200:nrow(movies),]$gross))
est_movies3 = estimate_xmin(movies_pl3)
bs = bootstrap_p(movies_pl3, no_of_sims=5)
movies_p3 = bs$p


# directors
directors = directors[!is.na(directors$gross),]
directors_pl = displ$new(round(directors$gross))
est_directors = estimate_xmin(directors_pl)
bs = bootstrap_p(directors_pl, no_of_sims=5)
directors_p = bs$p
plot(directors_pl)
plot(directors$director,directors$gross,log="xy")

directors_pl2 = displ$new(round(directors[1:200,]$gross))
est_directors2 = estimate_xmin(directors_pl2)
bs = bootstrap_p(directors_pl2, no_of_sims=5)
directors_p2 = bs$p

###########################
### Using NLS curve fitting
###########################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#books
y = books$weeks_on_list
x = books$title
z <- nls(formula = y ~ a*x^b+c, start = list(a=100, b=-2.5, c=1), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y, breaks = 1) # plot orig points
#lines(x,predict(z))
books$prediction = predict(z)
p1 = ggplot(books, aes(y=weeks_on_list,x=title)) + geom_point()
p1 = p1 + geom_line(aes(y=prediction,x=title), color='green')
p1 = p1 + ggtitle("Fiction Books") + xlab("Book") + ylab("Weeks on NYT Bestsellers")

#book publishers
y = book_publishers$weeks_on_list+1
x = book_publishers$publisher
z <- nls(formula = y ~ a*x^b+c, start = list(a=100, b=-2.5, c=1), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
book_publishers$prediction = predict(z)
p2 = ggplot(book_publishers, aes(y=weeks_on_list,x=publisher)) + geom_point()
p2 = p2 + geom_line(aes(x=publisher,y=prediction), color='green')
p2 = p2 + ggtitle("Book Publishers") + xlab("Publisher") + ylab("Weeks on NYT Bestsellers")

#book authors
y = authors$weeks_on_list+1
x = authors$author
z <- nls(formula = y ~ a*x^b+c, start = list(a=100, b=-2.5, c=1), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
authors$prediction = predict(z)
p3 = ggplot(authors, aes(y=weeks_on_list,x=author)) + geom_point()
p3 = p3 + geom_line(aes(x=author,y=prediction), color='green')
p3 = p3 + ggtitle("Authors") + xlab("Author") + ylab("Weeks on NYT Bestsellers")

#movies
movies = movies[1:3191,]
y = movies$gross
x = movies$name
z <- nls(formula = y ~ a*x^b+c, start = list(a=1000000000, b=-2.5, c=1000), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
movies$prediction = predict(z)
p4 = ggplot(movies, aes(y=gross,x=name)) + geom_point()
p4 = p4 + geom_line(aes(x=name,y=prediction), color='green')
p4 = p4 + ggtitle("Movies") + xlab("Movie") + ylab("Box Office")

#directors
directors = directors[1:1620,]
y = directors$gross
x = directors$director
z <- nls(formula = y ~ a*x^b+c, start = list(a=1000000000, b=-2.5, c=1000), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
directors$prediction = predict(z)
p5 = ggplot(directors, aes(y=gross,x=director)) + geom_point()
p5 = p5 + geom_line(aes(x=director,y=prediction), color='green')
p5 = p5 + ggtitle("Directors") + xlab("Director") + ylab("Box Office")

# newspapers
y = papers$Circulation
x = papers$Newspaper
z <- nls(formula = y ~ a*x^b+c, start = list(a=1000000, b=-2.5, c=1000), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
papers$prediction = predict(z)
p6 = ggplot(papers, aes(y=Circulation,x=Newspaper)) + geom_point()
p6 = p6 + geom_line(aes(x=Newspaper,y=prediction), color='green')
p6 = p6 + ggtitle("Newspapers") + xlab("Newspaper") + ylab("Circulation")

# games
y = games$Global_Sales*1000
x = games$Name
z <- nls(formula = y ~ a*x^b+c, start = list(a=100000, b=-1.5, c=1000000), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
games$prediction = predict(z)
p7 = ggplot(games, aes(y=Global_Sales*1000,x=Name)) + geom_point()
p7 = p7 + geom_line(aes(x=Name,y=prediction), color='green')
p7 = p7 + ggtitle("Video Games") + xlab("Game") + ylab("Units Sold (thousands)")

# game_publishers
y = game_publishers$Global_Sales*1000
x = game_publishers$Publisher
z <- nls(formula = y ~ a*x^b+c, start = list(a=100000, b=-1.5, c=1000000), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
game_publishers$prediction = predict(z)
p8 = ggplot(game_publishers, aes(y=Global_Sales*1000,x=Publisher)) + geom_point()
p8 = p8 + geom_line(aes(x=Publisher,y=prediction), color='green')
p8 = p8 + ggtitle("Video Game Publishers") + xlab("Game Publisher") + ylab("Units Sold (thousands)")

# songs
y = songs$weeks
x = songs$title
#z <- nls(formula = y ~ a*x^b+c, start = list(a=1000, b=-0.2, c=100), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
#p = ggplot(songs, aes(y=songs$weeks,x=songs$title)) + geom_point()
#p = p + geom_line(aes(x=songs$title,y=predict(z)), color='green')

# music artists
y = artists$weeks
x = artists$artist
z <- nls(formula = y ~ a*x^b+c, start = list(a=100, b=-0.5, c=100), control = list(maxiter = 50000, minFactor=1/100000000))
#plot(x,y) # plot orig points
#lines(x,predict(z))
artists$prediction = predict(z)
p9 = ggplot(artists, aes(y=weeks,x=artist)) + geom_point()
p9 = p9 + geom_line(aes(x=artist,y=prediction), color='green')
p9 = p9 + ggtitle("Musicians") + xlab("Artist") + ylab("Weeks on Billboard Hot-100")

png("multiplot.png", width=700, height=700)
p = multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)
dev.off()


# Now do log-log plots
# --------------------
#books
p1 = ggplot(books, aes(y=weeks_on_list,x=title)) + geom_point() 
p1 = p1 + ggtitle("Fiction Books") + xlab("Book") + ylab("Weeks on NYT Bestsellers")
p1 = p1 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#book publishers
p2 = ggplot(book_publishers, aes(y=weeks_on_list,x=publisher)) + geom_point()
p2 = p2 + ggtitle("Book Publishers") + xlab("Publisher") + ylab("Weeks on NYT Bestsellers")
p2 = p2 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#book authors
p3 = ggplot(authors, aes(y=weeks_on_list,x=author)) + geom_point()
p3 = p3 + ggtitle("Authors") + xlab("Author") + ylab("Weeks on NYT Bestsellers")
p3 = p3 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#movies
movies = movies[1:3191,]
p4 = ggplot(movies, aes(y=gross,x=name)) + geom_point()
p4 = p4 + ggtitle("Movies") + xlab("Movie") + ylab("Box Office")
p4 = p4 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#directors
directors = directors[1:1620,]
p5 = ggplot(directors, aes(y=gross,x=director)) + geom_point()
p5 = p5 + ggtitle("Directors") + xlab("Director") + ylab("Box Office")
p5 = p5 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

# newspapers
p6 = ggplot(papers, aes(y=Circulation,x=Newspaper)) + geom_point()
p6 = p6 + ggtitle("Newspapers") + xlab("Newspaper") + ylab("Circulation")
p6 = p6 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

# games
p7 = ggplot(games, aes(y=Global_Sales*1000,x=Name)) + geom_point()
p7 = p7 + ggtitle("Video Games") + xlab("Game") + ylab("Units Sold (thousands)")
p7 = p7 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

# game_publishers
p8 = ggplot(game_publishers, aes(y=Global_Sales*1000,x=Publisher)) + geom_point()
p8 = p8 + ggtitle("Video Game Publishers") + xlab("Game Publisher") + ylab("Units Sold (thousands)")
p8 = p8 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

# music artists
p9 = ggplot(artists, aes(y=weeks,x=artist)) + geom_point()
p9 = p9 + ggtitle("Musicians") + xlab("Artist") + ylab("Weeks on Billboard Hot-100")
p9 = p9 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

png("multiplot_log.png", width=700, height=700)
p = multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)
dev.off()


# Calculate the 80-20 percent for comparison
# ---------------------------------------
# function to calculate how much success the top x percent have
percent_top <- function(v, top) {

  num_vals_to_count = round(length(v)*top/100)
  top_sum = sum(v[1:num_vals_to_count])
  total_sum = sum(v)
  top_ratio = (top_sum/total_sum)*100
  
  return(top_ratio)
}

r_songs = percent_top(songs$weeks, 20) 
r_artists = percent_top(artists$weeks, 20) 
r_books = percent_top(books$weeks_on_list, 20)
r_authors = percent_top(authors$weeks_on_list, 20)
r_book_publishers = percent_top(book_publishers$weeks_on_list, 20)
r_games = percent_top(games$Global_Sales, 20)
r_game_publishers = percent_top(game_publishers$Global_Sales, 20)
r_papers = percent_top(papers$Circulation, 20)
r_movies = percent_top(movies$gross, 20) 
r_directors = percent_top(directors$gross, 20) 
r_podcasts = percent_top(podcasts$global_unique_streams_and_downloads, 20)
r_nba_2017 = percent_top(nba_2017$salary, 20)
r_nba_salaries = percent_top(salaries$salary, 20)
#r_billionaires = percent_top(as.numeric(billionaires$Net.Worth), 20) 


#######
### mess around with lm fittinh
#####
#books$weeks_on_list = as.integer(books$weeks_on_list) + 1
power_model = lm(formula=log(weeks_on_list) ~ log(title), data=books)
coef(power_model)
