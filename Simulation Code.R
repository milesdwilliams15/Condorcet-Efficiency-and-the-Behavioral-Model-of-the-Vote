
# Simulating a Behavioral Model of the Vote
# -----------------------------------------
library(dplyr)
library(foreach)
library(ggplot2)
library(gridExtra)

# Create "a spatial model of policy voting, in which
# the positions of both voters and candidates are randomly
# generated from uniform distributions on the interval
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
                 ideal_voter=x,
                 ideal_cand=rep(k,len=length(x)),
                 noise=exp(-exp(-rnorm(length(x),sd=sqrt(0.7),
                                       mean=0.6))) ) %>% 
  mutate(utility1=-100*((ideal_voter-ideal_cand)^2) + noise, # Vary level of policy
         utility2=-20*((ideal_voter-ideal_cand)^2) + noise,  # salience.
         utility3=-3*((ideal_voter-ideal_cand)^2) + noise,
         utility4=-1*((ideal_voter-ideal_cand)^2) + noise) %>%
  group_by(voter) %>%
  mutate(vote1=as.numeric(utility1==max(utility1)), # Create 0-1 dummy for whether
         vote2=as.numeric(utility2==max(utility2)), # a voter voted for candidate
         vote3=as.numeric(utility3==max(utility3)), # A, B, or C.
         vote4=as.numeric(utility4==max(utility4)))

# Visualize results
out1 <- df %>% group_by(cand) %>%
  summarize(vote_share=mean(vote1))
g1 <- df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(vote1)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel A)\nSimulated Election with 25,001 Voters, b = 100') +
  annotate('text',y=c(1.05),x=c(.24,.5,.76),
           label=c(paste("A = ",round(out1[1,2]*100,2),"%;",sep=""),
                   paste("B = ",round(out1[2,2]*100,2),"%;",sep=""),
                   paste("C = ",round(out1[3,2]*100,2),"%",sep="")),
           family='serif') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     limits=c(0,1.075))
out2 <- df %>% group_by(cand) %>%
  summarize(vote_share=mean(vote2))
g2 <- df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(vote2)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel B)\nSimulated Election with 25,001 Voters, b = 20') +
  annotate('text',y=c(1.05),x=c(.24,.5,.76),
           label=c(paste("A = ",round(out2[1,2]*100,2),"%;",sep=""),
                   paste("B = ",round(out2[2,2]*100,2),"%;",sep=""),
                   paste("C = ",round(out2[3,2]*100,2),"%",sep="")),
           family='serif') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     limits=c(0,1.075))
out3 <- df %>% group_by(cand) %>%
  summarize(vote_share=mean(vote3))
g3 <- df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(vote3)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel C)\nSimulated Election with 25,001 Voters, b = 3') +
  annotate('text',y=c(1.05),x=c(.24,.5,.76),
           label=c(paste("A = ",round(out3[1,2]*100,2),"%;",sep=""),
                   paste("B = ",round(out3[2,2]*100,2),"%;",sep=""),
                   paste("C = ",round(out3[3,2]*100,2),"%",sep="")),
           family='serif') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     limits=c(0,1.075))
out4 <- df %>% group_by(cand) %>%
  summarize(vote_share=mean(vote4))
g4 <- df %>% group_by(ideal_voter,cand) %>%
  summarize(vote_share=mean(vote4)) %>%
  mutate(Candidate=cand) %>%
  ggplot(aes(ideal_voter,vote_share,linetype=Candidate)) +
  geom_line(size=.75) + 
  labs(x='Voter Location on Policy Dimension J',
       y='Candidate Vote Shares',
       title='(Panel D)\nSimulated Election with 25,001 Voters, b = 1') +
  annotate('text',y=c(1.05),x=c(.24,.5,.76),
           label=c(paste("A = ",round(out4[1,2]*100,2),"%;",sep=""),
                   paste("B = ",round(out4[2,2]*100,2),"%;",sep=""),
                   paste("C = ",round(out4[3,2]*100,2),"%",sep="")),
           family='serif') +
  theme_minimal() +
  theme(text=element_text(family='serif'),
        plot.title = element_text(hjust=0.5)) + 
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                     limits=c(0,1.075))
grid.arrange(g1,g2,g3,g4)
ggsave(plot=grid.arrange(g1,g2,g3,g4),filename='Rplot.pdf',width=10,height=7.0)

# End File