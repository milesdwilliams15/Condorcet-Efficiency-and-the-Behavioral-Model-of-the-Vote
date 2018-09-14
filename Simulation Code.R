
# Simulating a Behavioral Model of the Vote
# -----------------------------------------
library(dplyr)
library(foreach)
library(ggplot2)
library(gridExtra)

# Create "a spatial model of policy voting, in which
# the positions of both voters and candidates are randomly
# generated from uniformdistributions on the interval
# [0,1]" and where voters' nonpolicy motivations are
# captured by a random variable independently generated
# from a type 1 extreme value distribution (Adams 1997, 1254).

# Generate Data Frame with Simulated Data
x <- times(25001) %do% {
  rep(sample(seq(0,1,.01),replace=T,size=1),3)
}
k <- c(.39,.47,.71)
df <- data.frame(voter=sort(rep(1:25001,len=length(x))),
                 cand=rep(c('A','B','C'),len=length(x)),
                 ideal_voter=rep(x,len=length(x)),
                 ideal_cand=rep(k,len=length(x)),
                 noise=exp(-exp(-rnorm(length(x),sd=sqrt(0.7),
                                       mean=0.6))) ) %>% 
  mutate(utility1=-100*((ideal_voter-ideal_cand)^2) + noise, # Vary level of policy
         utility2=-20*((ideal_voter-ideal_cand)^2) + noise,  # Salience
         utility3=-3*((ideal_voter-ideal_cand)^2) + noise,
         utility4=-1*((ideal_voter-ideal_cand)^2) + noise)

## Create variable for whether voter i cast her ballot in favor of 
## candidate k.
new_df <- foreach(i=unique(df$voter),.combine='rbind') %do% {
  new <- df %>% filter(voter==i)
  new$voted1 <- 0
  new$voted1[new$utility1==max(new$utility1)] <- 1
  new$voted2 <- 0
  new$voted2[new$utility2==max(new$utility2)] <- 1
  new$voted3 <- 0
  new$voted3[new$utility3==max(new$utility3)] <- 1
  new$voted4 <- 0
  new$voted4[new$utility4==max(new$utility4)] <- 1
  new
}

# Visualize results
new_df %>% group_by(cand) %>%
  summarize(vote_share=mean(voted1)) # Plug in vote share results in fig. title
g1 <- new_df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(voted1)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel A)\nSimulated Election with 25,001 Voters, b = 100\nVote Shares: A = 42.8%; B = 15.8%; C = 41.4%') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  ylim(c(0,1))
new_df %>% group_by(cand) %>%
  summarize(vote_share=mean(voted2))
g2 <- new_df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(voted2)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel B)\nSimulated Election with 25,001 Voters, b = 20\nVote Shares: A = 42.6%; B = 16.3%; C = 41.1%') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  ylim(c(0,1))
new_df %>% group_by(cand) %>%
  summarize(vote_share=mean(voted3))
g3 <- new_df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(voted3)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel C)\nSimulated Election with 25,001 Voters, b = 3\nVote Shares: A = 34.9%; B = 30.4%; C = 34.7%') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  ylim(c(0,1))
new_df %>% group_by(cand) %>%
  summarize(vote_share=mean(voted4))
g4 <- new_df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(voted4)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel D)\nSimulated Election with 25,001 Voters, b = 1\nVote Shares: A = 34.1%; B = 35.1%; C = 30.8%') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  ylim(c(0,1))
grid.arrange(g1,g2,g3,g4)
ggsave(plot=grid.arrange(g1,g2,g3,g4),filename='Rplot.pdf',width=8.05,height=7.0)
