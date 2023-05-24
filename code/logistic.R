df_improve <- df_clean %>%
    pivot_wider(id_cols = c(Subject, Item, Seq), names_from = Treat, values_from = Response) %>%
    filter(A != 0 & B != 0) %>%
    mutate(Improve = A > B, Improve_level = (A %in% c(4,5)) &  (B %in% c(1,2)))

m0 <- glm(Improve ~ 1, data = df_improve)

m1 <- glm(Improve ~ 1 + Seq, data = df_improve)

anova(m0, m1)

m2 <- glmer(Improve ~ 1 + Seq + (1 | Subject), data = df_improve)
m3 <- glmer(Improve ~ 1 + Seq + (1 | Item), data = df_improve)
m4 <- glmer(Improve ~ 1 + Seq + (1 | Item) + (1 | Subject), data = df_improve)
m5 <- glmer(Improve ~ 1 + Seq + (1 + Seq | Subject), data = df_improve)
m6 <- glmer(Improve ~ 1 + Seq + (1 + Seq | Item), data = df_improve)
m7 <- glmer(Improve ~ 1 + Seq + (1 + Seq | Item) + (1 + Seq | Subject), data = df_improve)
m8 <- glmer(Improve ~ 1 + (1 | Item) + (1 | Subject), data = df_improve)


m9 <- glmer(Improve ~ 1 + (1|Item) + (1 | Subject), family=binomial(link="logit"), data = df_improve)
anova(m2, m3, m4, m5, m6, m7)
anova(m4, m8)

sum((1 / (1 + exp(predict(m4, type = "link"))) > 0.5) == df_improve$Improve) / df_improve %>% nrow()


m10 <- brm(Improve ~ 1 + (1 | Item) + (1 | Subject), family = "bernoulli", data = df_improve)

bmmb::p_check(m10)
short_summary(m10)

fixef(m10)
pp_check(m10, "bars_grouped", group = "Item", ndraws = 1000)


sum((predict(m10)[, 1] > 0.5) == df_improve$Improve) / df_improve %>% nrow()
sum((predict(m10, newdata=df_improve %>% select(Improve, Item), re_formula= ~ (1 | Item))[, 1] > 0.5) == df_improve$Impr   ove) / df_improve %>% nrow()
sum(apply(predict(brm_treat.period.subject.question, type = "response"), 1, which.max) == df_clean$Response) / nrow(df_clean)

sum(apply(predict(brm_treat.period.subject.question,
    newdata = df_clean %>% select(Treat, Item, Seq, Response, Period),
    re_formula = ~ (1 + Treat | Item), type = "response", ndraws = 100
), 1, which.max) == df_clean$Response) / nrow(df_clean)

df_improve$Prob = predict(m10)[, 1]

 df_improve %>% ggplot(aes(x = Item, y = Prob, fill = Item)) +
     geom_boxplot()

df_improve %>% group_by(Item) %>% summarize(Improve=sum(Improve)/n()) %>% ggplot(aes(x = Item, y = Improve, fill = Ques
    tion)) + 
         geom_bar(stat="identity") 

df_improve_resume <- df_improve %>%
    group_by(Item) %>%
    summarize(Improve = sum(Improve) / n()) %>% arrange(Improve)

df_improve_resume %>% ggplot(aes(y= Improve)) + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf), fill = "green", alpha = 0.2) +  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax
     = 0.5), fill = "red", alpha = 0.2)  + geom_text(size=8,position = position_jitter(height = 0, width=0.2), aes(x=0, label=Item, y=Improve)) + geom_hline(yintercept = 0.5, linetype = "dashed", size = 2) + labs(y="Frequency of improve A - B", x=NULL)  + 
      theme(axis.line.x = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand=c(0.005,0.005)) 

paste((df_improve_resume %>% head(2))$Item, collapse = ", ")
paste((df_improve_resume %>% tail(2))$Item, collapse=", ")

summary(m9)

mutate(df_improve, residuals=residuals(m9), linpred=predict(m9)) %>% group_by(breaks=cut(linpred,unique(quantile(linpred,(1:100)/101)))) %>% summarize(residuals=mean(residuals), linpred=mean(linpred)) %>% plot(residuals ~ linpred, data=.) 

qqnorm(residuals(m9))


#pag 41
df_improve %>% mutate(predprob=predict(m9, type='response'), linpred=predict(m9)) %>% group_by(cut(linpred, breaks=unique(quantile(linpred, (1:100)/101)))) %>% summarize(Improve=sum(Improve), ppred=mean(predprob), count=n()) %>% mutate(se.fit=sqrt(ppred*(1-ppred)/count)) %>% ggplot(aes(x=ppred, y=Improve/count,ymin=Improve/count-2*se.fit,ymax=Improve/count+2*se.fit)) + geom_point()+geom_linerange(color=grey(0.75))+geom_abline(intercept=0,slope=1)+xlab("Predicted Probability")+ylab("Observed Proportion")    

