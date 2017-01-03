# Colorado analysis 
# Bryan Wilcox-Archuleta 
# Jan. 3, 2017


# header 
# this script replicates EI estimates and graphs for Colorado precinct level analysis. 


library(tidyverse)


# read data 
data <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/colorado/clean_results/analysis/merged_2016.csv")



# graphing -----
df <- gather(data,candidate, pct_vote ,c(pct_clinton, pct_trump))


weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = votes, size = votes)) + geom_point(alpha = .10) + 
  scale_color_manual(values = c('blue', 'red'), breaks = c('pct_clinton', 'pct_trump'),labels = c('Clinton', 'Trump'), name = "Candidate") + stat_smooth(aes(wt = votes), se = F) + 
  theme_bw() + scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "Colorado Presidental Vote Among Latinos", x = "Percent Latino Registered Voter in Precinct", y = "2016 Presidental Vote Share") + guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/colorado/weighted_co.png", weighted, height = 8, width = 8)



# EI ----------

library(eiCompare)
df <- data %>% dplyr::select(pct_clinton, pct_trump, votes , pct_latino) %>% na.omit()


COdf2 <- df %>%  mutate(pct_other = 1 - (pct_trump+pct_clinton),pct_nonlatino = 1-pct_latino)

cands <- c("pct_clinton", "pct_trump", "pct_other")
groups <- c("~ pct_latino", "~ pct_nonlatino") 
table_names <- c("EI: Pct Latino", "EI: Pct Non Latino")

# EI with compare
resultsCO <- ei_est_gen(cands, groups,
                        "votes", data = COdf2, 
                        table_names = table_names)

# EI with EI 
model_clinton <- pct_clinton ~ pct_latino
model_trump <- pct_trump ~ pct_latino

ei_clinton <- ei(model_clinton, total="votes", erho=.5, data=df)  

res_clinton <- eiread(ei_clinton, "maggs")  
pe_clinton <- res_clinton[1]
se_clinton <- res_clinton[3]

ei_trump <- ei(model_trump, total="votes", erho=.5, data=df)  
res_trump <- eiread(ei_trump, "maggs")  
pe_trump <- res_trump[1]
se_trump <- res_trump[3]



ei_all <- data.frame(geography = c("Entire Sample","Entire Sample"),
                     candidate = c("Clinton", "Trump"), 
                     pct_latino_vote = c(pe_clinton, pe_trump),
                     std_error = c(se_clinton,se_trump))


# 2012 and 2016 stuff 

data <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/colorado/clean_results/analysis/joined_2012_2016_w_wts.csv")


# 2016 - 2012 difference
# net votes clinton
df <- data %>% dplyr::select(precinct,obama , romney, clinton,trump ,pct_latino)
df <- df %>% mutate(clinton_margin = clinton - obama, 
                    direction = ifelse(clinton_margin > 0, "Clinton Improves", "Clinton Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = clinton_margin, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "2016 Colorado Latino Vote", 
                    y = "Net Clinton Difference \n (Clinton 16 - Obama 12)", 
                    x = "Percent Latino Registered \n Voter in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-750,750))

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/colorado/difference_co.png", plot, height = 8, width = 8)

# net votes dem 
df <- data %>% dplyr::select(precinct,obama , romney, clinton,trump ,pct_latino)
df <- df %>% mutate(obama_raw_margin = obama - romney, 
                    clinton_raw_margin = clinton - trump, 
                    raw_net_dem = clinton_raw_margin - obama_raw_margin, 
                    direction = ifelse(raw_net_dem > 0, "Clinton Improves", "Clinton Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = raw_net_dem, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "2016 Colorado Latino Vote", 
                    y = "Net Raw Difference \n (Difference 16 - Difference 12)", 
                    x = "Percent Latino Registered \n Voter in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-500,750))



# net votes
df <- data %>% dplyr::select(precinct,obama , romney, clinton,trump ,pct_latino)
df <- df %>% mutate(obama_raw_margin = obama - romney, 
                    clinton_raw_margin = clinton - trump, 
                    raw_net_dem = clinton_raw_margin - obama_raw_margin, 
                    direction = ifelse(raw_net_dem > 0, "Clinton Improves", "Clinton Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = raw_net_dem, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "2016 Colorado Latino Vote", 
                    y = "Net Raw Difference \n (Difference 16 - Difference 12)", 
                    x = "Percent Latino Registered \n Voter in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-500,750))



# weighted comparison of 2012 and 2016
df <- gather(data ,candidate, pct_vote ,c(pct_obama, pct_romney, pct_trump, pct_clinton))
df$wt <- ifelse(df$candidate=='pct_obama'| df$candidate=="pct_romney",df$votes_2012, df$votes)
head(df)


weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = wt, size = wt)) + geom_point(alpha = .15) + 
  scale_color_manual(values = c('blue', 'turquoise', 'coral3', 'red'),
                     breaks = c('pct_clinton', 'pct_obama', 'pct_romney', 'pct_trump'),
                     labels = c('Clinton', 'Obama', 'Romney', 'Trump'), 
                     name = "Candidate") + 
  stat_smooth(se = F) + 
  theme_bw() + scale_y_continuous(aes(weight = wt), limits=c(0,1)) + 
  labs(title = "2012 & 2016 Colorado Latino Vote", 
       y = "Latino Vote Share",
       x = "Percent Latino Registered \n Voter in Precinct") + guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/colorado/all_candidate_co.png", weighted, height = 8, width = 8)
