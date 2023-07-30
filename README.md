# Survey Nonresponse

This project analyzes survey data using R and beta regression to provide evidence for a new factor behind survey non-response: uncertainty about what the socially acceptable answer is. 

Survey design is an extremely technical field, despite how simple it can be at the surface. Researchers have spent nearly a century improving how questions are written, the scales used, and how the surveys are presented to gather better data. Examples of this are numerous, but one of the most important is called social desirability bias. Simply put, people are prone to giving answers that they feel will not offend other people or lead to them being judged. If a person if being interviewed by Coca-Cola for example, they might be reluctant to say they prefer Pepsi. This effect is magnified when the questions become even more sensitive, extending to things like drug use, political views, and sexual orientation. 

Two key ways that social desirability bias manifests itself is by choosing the socially "correct" answer and non-response. In the former, respondents will go with whatever answer they feel is safest. Our hypothetical Coca-cola interviewee, for example, might state that they love Coke. In the latter, respondents, when given the option, might choose to answer "don't know", leave an item blank, or choose a neutral option. 

Both of these can be problematic for anyone answering survey data. In the former case, analysts might overestimate the popularity of the socially "correct" answer, while in the latter, they will be unable to determine if people simply didn't understand the question or just don't want to answer. These problems are well-known, and survey creators have worked out methods for minimizing their effect. 

However, there is a possibility that survey creators have not dealt with: when the socially "correct" answer is unknown. Let's imagine a scenario. The leadership of a company recently made a change- say switching from fully remote to hybrid work. This move was controversial both among employees and leadership. Now, several months later, management decides to conduct a survey of employees to see what their opinions are. These surveys are anonymous, but the employees don't trust management that there will be no consequences to giving the "wrong' answer. At the same time, management itself is divided, which the employees know. From employees' perspectives, therefore,  coming out in favor of either remote work or of hybrid work could lead to consequences. The safest choice is not to give an answer or to give one that's as neural as possible. 

Scenarios like this occur somewhat often in real-life- most likely everyone can think of a time when they didn't express their true thoughts out of fear of the consequences. It hasn't, however, been statistically shown in research about survey design. That's what I'll be doing here. To do this, I'll be analyzing data collected by two surveys: the Global Attitudes Project conducted by Pew Research and the World Value Survey. Each one surveyed tens of thousands of people in dozens of countries, trying to conclusively pin down global attitudes about democracy, human rights, and society. 

As a controversial subject, I chose homosexuality. Homosexuality works well here because its social acceptability varies widely across countries. In some (Sweden, Canada, etc.) supporting gay rights is the default, and those who don't are viewed poorly. In other countries (Nigeria, Egypt, etc.) supporting gay rights is an extreme minority position. Most countries lie in the middle. According to my theory then, countries where acceptance of homosexuality has a default view (either for or against) should have a smaller non-response rate than in countries where its a controversial subject. 

Let's get started. To save space, I'll only show code relating to the data from Pew, but I found the same results from the World Values Survey Data. Full code can be found in this repository. 
```
#Importing the Pew data set
topline <- read_excel(here("raw-data","topline.xlsx"))
#Now I'm creating the variables I'll need for later analysis.
topline$diff <- topline$pro_LGBT-topline$anti_LBGT
topline$abs_diff <- abs(topline$pro_LGBT-topline$anti_LBGT)
topline$sqr_diff <- (topline$pro_LGBT-topline$anti_LBGT)^2
#Plus cleaning it up a bit. NAs were present in the non-1 spaces of the dummy variables. 
topline = subset(topline, select = -c(...6))
topline[is.na(topline)] <- 0

#If the country is more anti than pro-LGBT. 
topline <- topline %>%
  dplyr::mutate(anti = ifelse(anti_LBGT >= pro_LGBT, 1, 0))
```

Once I loaded the Pew data, I could run a regression. I chose to run two models, one regressing the absolute difference on the non-response rate and the second adding a control variable for in-person interviews. For a regression technique, I'm using beta regression, which uses dependent variables that are between 0 and 1, but can't go outside those bounds, such as many percentages. 

```
#Beta regression time!
#We'll start with Pew. First, putting it into a format that beta regression can use (between, but not including 0 and 1 in the DV)
topline$perc_refused <- topline$refused/100
topline$perc_abs_diff <- topline$abs_diff/100
#Beta regression uses values between, but not including 0 and 1. So we eliminate any rows with 0
topline_no_zero <- topline %>%
  filter(perc_refused >0)
#Now I'll set up the independent variable
#I'm making some modifications for mathematical reasons- essentially this changes the variable to look at the absolute difference within the % of respondents who answered the question, not out of 100 
topline_no_zero$perc_answered <- 100 - topline_no_zero$refused
topline_no_zero$rel_abs_diff <- abs((topline_no_zero$pro_LGBT- topline_no_zero$anti_LBGT)/topline_no_zero$perc_answered)
#And run the two regressions
beta_pew <- betareg(perc_refused ~ rel_abs_diff + in_person, data = topline_no_zero)

beta_pew_small <- betareg(perc_refused ~ rel_abs_diff, data = topline_no_zero)
```
As can be seen here, there's a clear drop non-response rate as public opinion becomes more absolute on LGBT rights. Altogether, we'd expect an approximately 15% nonresponse rate in an evenly divided country, an 8% one in a country that's split 75/25, and only a 5% one in a country where almost the entire population holds one view. 

![Results from the second model](http://url/to/img.png)

