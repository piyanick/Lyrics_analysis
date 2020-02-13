
# Import library
#install.packages('geniusr')
library(geniusr)
library(tidyverse)
library(tidytext)


# Create a list of URL that contains lyrics from Ed Sheeran
sheeran_url <- c("https://genius.com/Ed-sheeran-shape-of-you-lyrics",
                "https://genius.com/Ed-sheeran-perfect-lyrics",
                "https://genius.com/Ed-sheeran-photograph-lyrics",
                "https://genius.com/Ed-sheeran-castle-on-the-hill-lyrics",
                "https://genius.com/Ed-sheeran-happier-lyrics",
                "https://genius.com/Ed-sheeran-galway-girl-lyrics",
                "https://genius.com/Ed-sheeran-thinking-out-loud-lyrics",
                "https://genius.com/Ed-sheeran-dive-lyrics",
                "https://genius.com/Ed-sheeran-the-a-team-lyrics",
                "https://genius.com/Ed-sheeran-beautiful-people-lyrics",
                "https://genius.com/Ed-sheeran-i-see-fire-lyrics",
                "https://genius.com/Ed-sheeran-you-need-me-i-dont-need-you-live-at-the-live-room-lyrics",
                "https://genius.com/Ed-sheeran-give-me-love-lyrics",
                "https://genius.com/Ed-sheeran-lego-house-lyrics",
                "https://genius.com/Ed-sheeran-sing-lyrics",
                "https://genius.com/Ed-sheeran-bloodstream-lyrics",
                "https://genius.com/Ed-sheeran-dont-lyrics",
                "https://genius.com/Ed-sheeran-supermarket-flowers-lyrics",
                "https://genius.com/Ed-sheeran-south-of-the-border-lyrics",
                "https://genius.com/Ed-sheeran-eraser-lyrics",
                "https://genius.com/Ed-sheeran-beautiful-people-lyrics",
                "https://genius.com/Ed-sheeran-new-man-lyrics",
                "https://genius.com/Ed-sheeran-how-would-you-feel-paean-lyrics",
                "https://genius.com/Ed-sheeran-remember-the-name-lyrics")

# Create a list of URL that contains lyrics from Bruno Mars
mars_url <- c("https://genius.com/Bruno-mars-thats-what-i-like-lyrics",
              "https://genius.com/Bruno-mars-versace-on-the-floor-lyrics",
              "https://genius.com/Bruno-mars-24k-magic-lyrics",
              "https://genius.com/Bruno-mars-when-i-was-your-man-lyrics",
              "https://genius.com/Bruno-mars-finesse-lyrics",
              "https://genius.com/Bruno-mars-just-the-way-you-are-lyrics",
              "https://genius.com/Bruno-mars-grenade-lyrics",
              "https://genius.com/Bruno-mars-locked-out-of-heaven-lyrics",
              "https://genius.com/Bruno-mars-count-on-me-lyrics",
              "https://genius.com/Bruno-mars-treasure-lyrics",
              "https://genius.com/Bruno-mars-the-lazy-song-lyrics",
              "https://genius.com/Bruno-mars-talking-to-the-moon-lyrics",
              "https://genius.com/Bruno-mars-it-will-rain-lyrics",
              "https://genius.com/Bruno-mars-marry-you-lyrics",
              "https://genius.com/Bruno-mars-young-girls-lyrics",
              "https://genius.com/Bruno-mars-perm-lyrics",
              "https://genius.com/Bruno-mars-calling-all-my-lovelies-lyrics",
              "https://genius.com/Bruno-mars-if-i-knew-lyrics",
              "https://genius.com/Bruno-mars-straight-up-and-down-lyrics",
              "https://genius.com/Bruno-mars-liquor-store-blues-lyrics",
              "https://genius.com/Bruno-mars-rest-of-my-life-lyrics",
              "https://genius.com/Bruno-mars-runaway-baby-lyrics",
              "https://genius.com/Bruno-mars-natalie-lyrics",
              "https://genius.com/Bruno-mars-the-other-side-lyrics")

# Create a list of URL that contains lyrics from Billie Eilish
eilish_url <- c("https://genius.com/Billie-eilish-when-the-partys-over-lyrics",
                "https://genius.com/Billie-eilish-bad-guy-lyrics",
                "https://genius.com/Billie-eilish-everything-i-wanted-lyrics",
                "https://genius.com/Billie-eilish-idontwannabeyouanymore-lyrics",
                "https://genius.com/Billie-eilish-bury-a-friend-lyrics",
                "https://genius.com/Billie-eilish-wish-you-were-gay-lyrics",
                "https://genius.com/Billie-eilish-ocean-eyes-lyrics",
                "https://genius.com/Billie-eilish-i-love-you-lyrics",
                "https://genius.com/Billie-eilish-you-should-see-me-in-a-crown-lyrics",
                "https://genius.com/Billie-eilish-all-the-good-girls-go-to-hell-lyrics",
                "https://genius.com/Billie-eilish-xanny-lyrics",
                "https://genius.com/Billie-eilish-copycat-lyrics",
                "https://genius.com/Billie-eilish-bellyache-lyrics",
                "https://genius.com/Billie-eilish-watch-lyrics",
                "https://genius.com/Billie-eilish-my-strange-addiction-lyrics",
                "https://genius.com/Billie-eilish-my-boy-lyrics",
                "https://genius.com/Billie-eilish-listen-before-i-go-lyrics",
                "https://genius.com/Billie-eilish-hostage-lyrics",
                "https://genius.com/Billie-eilish-six-feet-under-lyrics",
                "https://genius.com/Billie-eilish-ilomilo-lyrics",
                "https://genius.com/Billie-eilish-come-out-and-play-lyrics",
                "https://genius.com/Billie-eilish-bitches-broken-hearts-lyrics",
                "https://genius.com/Billie-eilish-8-lyrics",
                "https://genius.com/Billie-eilish-party-favor-lyrics",
                "https://genius.com/Billie-eilish-61818-lyrics",
                "https://genius.com/Billie-eilish-goodbye-lyrics",
                "https://genius.com/Billie-eilish--lyrics",
                "https://genius.com/Billie-eilish-bored-lyrics",
                "https://genius.com/Billie-eilish-when-i-was-older-lyrics",
                "https://genius.com/Billie-eilish-the-end-of-the-world-lyrics",
                "https://genius.com/Billie-eilish-fingers-crossed-lyrics",
                "https://genius.com/Billie-eilish-shes-broken-lyrics",
                "https://genius.com/Billie-eilish-see-through-lyrics")

# Create a list of URL that contains lyrics from Beyonce
beyonce_url <- c("https://genius.com/Beyonce-drunk-in-love-lyrics",
                 "https://genius.com/Beyonce-formation-lyrics",
                 "https://genius.com/Beyonce-partition-lyrics",
                 "https://genius.com/Beyonce-mine-lyrics",
                 "https://genius.com/Beyonce-hold-up-lyrics",
                 "https://genius.com/Beyonce-sorry-lyrics",
                 "https://genius.com/Beyonce-if-i-were-a-boy-lyrics",
                 "https://genius.com/Beyonce-pray-you-catch-me-lyrics",
                 "https://genius.com/Beyonce-all-night-lyrics",
                 "https://genius.com/Beyonce-flawless-lyrics",
                 "https://genius.com/Beyonce-halo-lyrics",
                 "https://genius.com/Beyonce-dont-hurt-yourself-lyrics",
                 "https://genius.com/Beyonce-listen-lyrics",
                 "https://genius.com/Beyonce-crazy-in-love-lyrics",
                 "https://genius.com/Beyonce-love-on-top-lyrics",
                 "https://genius.com/Beyonce-freedom-lyrics",
                 "https://genius.com/Beyonce-blow-lyrics",
                 "https://genius.com/Beyonce-rocket-lyrics",
                 "https://genius.com/Beyonce-sandcastles-lyrics",
                 "https://genius.com/Beyonce-7-11-lyrics",
                 "https://genius.com/Beyonce-6-inch-lyrics",
                 "https://genius.com/Beyonce-love-drought-lyrics",
                 "https://genius.com/Beyonce-pretty-hurts-lyrics")

# Create a list of URL that contains lyrics from Ariana Grande
ariana_url <- c("https://genius.com/Ariana-grande-thank-u-next-lyrics",
              "https://genius.com/Ariana-grande-7-rings-lyrics",
              "https://genius.com/Ariana-grande-god-is-a-woman-lyrics",
              "https://genius.com/Ariana-grande-side-to-side-lyrics",
              "https://genius.com/Ariana-grande-no-tears-left-to-cry-lyrics",
              "https://genius.com/Ariana-grande-breathin-lyrics",
              "https://genius.com/Ariana-grande-break-up-with-your-girlfriend-im-bored-lyrics",
              "https://genius.com/Ariana-grande-imagine-lyrics",
              "https://genius.com/Ariana-grande-needy-lyrics",
              "https://genius.com/Ariana-grande-into-you-lyrics",
              "https://genius.com/Ariana-grande-dangerous-woman-lyrics",
              "https://genius.com/Ariana-grande-one-last-time-lyrics",
              "https://genius.com/Ariana-grande-the-way-lyrics",
              "https://genius.com/Ariana-grande-problem-lyrics",
              "https://genius.com/Ariana-grande-focus-lyrics",
              "https://genius.com/Ariana-grande-almost-is-never-enough-lyrics",
              "https://genius.com/Ariana-grande-break-free-lyrics",
              "https://genius.com/Ariana-grande-greedy-lyrics",
              "https://genius.com/Ariana-grande-ghostin-lyrics",
              "https://genius.com/Ariana-grande-sweetener-lyrics",
              "https://genius.com/Ariana-grande-nasa-lyrics",
              "https://genius.com/Ariana-grande-rem-lyrics",
              "https://genius.com/Ariana-grande-in-my-head-lyrics",
              "https://genius.com/Ariana-grande-the-light-is-coming-lyrics",
              "https://genius.com/Ariana-grande-goodnight-n-go-lyrics",
              "https://genius.com/Ariana-grande-fake-smile-lyrics",
              "https://genius.com/Ariana-grande-bloodline-lyrics")

  

# Scraping all lyrics from Ed Sheeran by using geniusr
datalist = list()
for (i in 1: length(sheeran_url)) {
  dat <- scrape_lyrics_url(song_lyrics_url = sheeran_url[i])
  datalist[[i]] <- dat # add it to your list
}
sheeran_df = do.call(rbind, datalist)

# Scraping all lyrics from Bruno Mars geniusr
datalist = list()
for (i in 1: length(mars_url)) {
  dat <- scrape_lyrics_url(song_lyrics_url = mars_url[i])
  datalist[[i]] <- dat # add it to your list
}
mars_df = do.call(rbind, datalist)

# Scraping all lyrics from Billie Eilish
datalist = list()
for (i in 1: length(eilish_url)) {
  dat <- scrape_lyrics_url(song_lyrics_url = eilish_url[i])
  datalist[[i]] <- dat # add it to your list
}
eilish_df = do.call(rbind, datalist)

# Scraping all lyrics from Beyonce geniusr
datalist = list()
for (i in 1: length(beyonce_url)) {
  dat <- scrape_lyrics_url(song_lyrics_url = beyonce_url[i])
  datalist[[i]] <- dat # add it to your list
}
beyonce_df = do.call(rbind, datalist)

# Scraping all lyrics from Ariana Grande
datalist = list()
for (i in 1: length(ariana_url)) {
  dat <- scrape_lyrics_url(song_lyrics_url = ariana_url[i])
  datalist[[i]] <- dat # add it to your list
}
ariana_df = do.call(rbind, datalist)


# Tokenize lyrics from Ed Sheeran
sheeran_struc <- sheeran_df %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
print(sheeran_struc)

# Tokenize lyrics from Bruna Mars
mars_struc <- mars_df %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
print(mars_struc)

# Tokenize lyrics from Billie Eilish
eilish_struc <- eilish_df %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
print(eilish_struc)

# Tokenize lyrics from Beyonce
beyonce_struc <- beyonce_df %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
print(beyonce_struc)

# Tokenize lyrics from Ariana Grande
ariana_struc <- ariana_df %>%
  unnest_tokens(word,line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
print(ariana_struc)


# Conver to percentage
freq_sheeran_word <- sheeran_struc %>%
                      mutate(proportion = n/sum(n))

# Conver to percentage
freq_mars_word <- mars_struc %>%
                      mutate(proportion = n/sum(n))

# Conver to percentage
freq_eilish_word <- eilish_struc %>%
                      mutate(proportion = n/sum(n))

# Conver to percentage
freq_beyonce_word <- beyonce_struc %>%
                      mutate(proportion = n/sum(n))

# Conver to percentage
freq_ariana_word <- ariana_struc %>%
                      mutate(proportion = n/sum(n))

# Visualize the word's frequency in bar chart
freq_sheeran_word %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, proportion) , proportion))+
  geom_col(show.legend = FALSE, fill = 'orange', alpha = 1)+
  scale_fill_brewer(direction = -1, palette = "Blues")+
  scale_y_continuous(limits = c(0,0.07), labels = scales::percent) +
  labs(y="Ed Sheeran's word choice", x=NULL)+
  coord_flip()

# Visualize the word's frequency in bar chart
freq_mars_word %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, proportion) , proportion))+
  geom_col(show.legend = FALSE, fill = 'blue', alpha = 1)+
  scale_y_continuous(limit = c(0,0.07), labels = scales::percent) +
  labs(y="Bruno Mars's word choice", x=NULL)+
  coord_flip()

# Visualize the word's frequency in bar chart
freq_eilish_word %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, proportion) , proportion))+
  geom_col(show.legend = FALSE, fill = 'green', alpha = 1)+
  scale_y_continuous(limits = c(0,0.07),labels = scales::percent) +
  labs(y="Billie Eilish's word choice", x=NULL)+
  coord_flip()

# Visualize the word's frequency in bar chart
freq_beyonce_word %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, proportion) , proportion))+
  geom_col(show.legend = FALSE, fill = 'violet', alpha = 1)+
  scale_y_continuous(limits = c(0,0.07),labels = scales::percent) +
  labs(y="Beyonce's word choice", x=NULL)+
  coord_flip()

# Visualize the word's frequency in bar chart
freq_ariana_word %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, proportion) , proportion))+
  geom_col(show.legend = FALSE, fill = 'red', alpha = 1)+
  scale_y_continuous(limits = c(0,0.07),labels = scales::percent) +
  labs(y="Ariana Grande's word choice", x=NULL)+
  coord_flip()



# Inner join with nrc dictionary and create a proportion column
sheeran_feeling <- sheeran_struc %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
print(sheeran_feeling)

# Inner join with nrc dictionary and create a proportion column
mars_feeling <- mars_struc %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
print(mars_feeling)

# Inner join with nrc dictionary and create a proportion column
eilish_feeling <- eilish_struc %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
print(eilish_feeling)

# Inner join with nrc dictionary and create a proportion column
beyonce_feeling <- beyonce_struc %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
print(beyonce_feeling)

# Inner join with nrc dictionary and create a proportion column
ariana_feeling <- ariana_struc %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
print(ariana_feeling)


# Plotting bar chart
sheeran_feeling %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, proportion)) %>%
  ggplot(aes(sentiment , proportion))+
  geom_col(show.legend = FALSE, fill = 'orange', alpha = 1)+
  scale_y_continuous(limits = c(0,0.2), labels = scales::percent) +
  labs(y="Ed Sheeran's word choice by emotion", x=NULL)+
  coord_flip()

# Plotting bar chart
mars_feeling %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, proportion)) %>%
  ggplot(aes(sentiment , proportion))+
  geom_col(show.legend = FALSE, fill = 'blue', alpha = 1)+
  scale_y_continuous(limits = c(0,0.2), labels = scales::percent) +
  labs(y="Bruno Mars' word choice by emotion", x=NULL)+
  coord_flip()

# Plotting bar chart
eilish_feeling %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, proportion)) %>%
  ggplot(aes(sentiment , proportion))+
  geom_col(show.legend = FALSE, fill = 'green', alpha = 1)+
  scale_y_continuous(limits = c(0,0.2), labels = scales::percent) +
  labs(y="Billie Eilish's word choice by emotion", x=NULL)+
  coord_flip()

# Plotting bar chart
beyonce_feeling %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, proportion)) %>%
  ggplot(aes(sentiment , proportion))+
  geom_col(show.legend = FALSE, fill = 'violet', alpha = 1)+
  scale_y_continuous(limits = c(0,0.2), labels = scales::percent) +
  labs(y="Beyonce's word choice by emotion", x=NULL)+
  coord_flip()

# Plotting bar chart
ariana_feeling %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, proportion)) %>%
  ggplot(aes(sentiment , proportion))+
  geom_col(show.legend = FALSE, fill = 'red', alpha = 1)+
  scale_y_continuous(limits = c(0,0.2), labels = scales::percent) +
  labs(y="Ariana Grande's word choice by emotion", x=NULL)+
  coord_flip()


# Binding all artists by rows and calculate the probability that each words appear
frequency <- bind_rows(mutate(sheeran_struc, Artist="Ed Sheeran"),
                       mutate(mars_struc, Artist= "Bruno Mars"),
                       mutate(eilish_struc, Artist="Billie Eilish"),
                       mutate(beyonce_struc, Artist = 'Beyonce'),
                       mutate(ariana_struc, Artist='Ariana Grande')) %>%
  group_by(Artist) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(Artist, proportion) %>%
  gather(Artist, proportion, `Ed Sheeran`, `Bruno Mars`, `Beyonce`, `Ariana Grande`)


# Create a correlogram
ggplot(frequency, aes(x=proportion, y=`Billie Eilish`, 
                      color = abs(`Billie Eilish`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word),check_overlap = TRUE, vjust=0.15) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~Artist, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Billie Eilish", x=NULL)

