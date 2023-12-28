# CROSS-NATIONAL CULTURAL EFFECTS ON SURVEY NON-RESPONSE RATES

**This version is in English. Die deutsche Version finden Sie weiter unten.**

## The Project

This project analyzes survey data using beta regression in R to provide evidence for a new factor behind survey non-response rates: uncertainty about what the socially acceptable answer is. It's adapted from a paper I wrote for my Data & Measurement class at the University of Mannheim. I've included this paper, along with replication code, in this repository. 

## Background
Survey design is an extremely technical field, despite how simple it can appear at the surface. Researchers have spent nearly a century improving how questions are written, the scales used, and how the surveys are presented to gather better data. One of the chief problems is a psychological phenomenon called social desirability bias. Simply put, people are prone to giving answers that they feel will not offend other people or lead to them being judged. If a person is being interviewed by Coca-Cola, for example, they might be reluctant to say they prefer Pepsi. This effect is magnified when the questions become even more sensitive, extending to things like drug use, political views, and sexual orientation. 

Two key ways that social desirability bias manifests itself is by choosing the socially "correct" answer and non-response. In the former, respondents will go with whatever answer they feel is safest. Our hypothetical Coca-cola interviewee, for example, might state that they love Coke. In the latter, respondents, when given the option, might choose to answer "don't know", leave an item blank, or choose a neutral option. 

Both of these can be problematic for anyone analyzing survey data. In the former case, analysts might overestimate the popularity of the socially "correct" answer, while in the latter, they will be unable to determine if people simply didn't understand the question or just don't want to answer. These problems are well-known, and survey creators have worked out methods for minimizing their effect. 

However, there is a possibility that survey creators have not dealt with: when the socially "correct" answer is unknown. Let's imagine a scenario. The leadership of a company recently made a change- say switching from fully remote to hybrid work. This move was controversial both among employees and leadership. Now, several months later, management decides to conduct a survey of employees to see what their opinions are. These surveys are anonymous, but the employees don't trust management that there will be no consequences to giving the 'wrong' answer. At the same time, management itself is divided, which the employees know. From employees' perspectives, therefore, coming out in favor of either remote work or of hybrid work could lead to consequences. The safest choice is not to give an answer or to give one that's as neutral as possible. Although this example may sound contrived, it is very much the calculation people living in authoritarian regimes make when answering surveys- a strong answer can be dangerous when the 'correct' answer is not known. 

## Implementation

I'll be analyzing data collected by two surveys: the Global Attitudes Project, conducted by Pew Research, and the World Values Survey. Each one surveyed tens of thousands of people in dozens of countries, trying to conclusively pin down global attitudes about democracy, human rights, and society. 

As a controversial subject, I chose a question about whether homosexuality should be accepted by society. Homosexuality works well here because its social acceptability varies dramatically across countries. In some (Sweden, Canada, etc.) supporting gay rights is the default position, and those who don't do so are viewed poorly. In other countries (Nigeria, Egypt, etc.) supporting gay rights is an extreme minority position. Most countries lie in the middle. According to my theory, countries where acceptance of homosexuality has a societally 'default' view (either for or against) should have a smaller non-response rate than in countries where it's a controversial subject. 

Let's get started. To save space, I'll only show code and results relating to the data from Pew, but I found the same results from the World Values Survey Data. Full code can be found in this repository. 

To begin, I loaded the data, cleaned it, and constructed the variables. As my question dealt with the absolute difference of opinion, I created a variable that showed the absolute difference between pro- and anti-LGBT opinion. Then I cleaned up the dataset a bit, replacing NAs with 0s in the boolean columns. 

```
#Importing the Pew data set
topline <- read_excel(here("raw-data","topline.xlsx"))
#Now I'm creating the variables I'll need for later analysis.
topline$diff <- topline$pro_LGBT-topline$anti_LBGT
topline$abs_diff <- abs(topline$pro_LGBT-topline$anti_LBGT)
#Plus cleaning it up a bit. NAs were present in the non-1 spaces of the dummy variables. 
topline = subset(topline, select = -c(...6))
topline[is.na(topline)] <- 0
```

Once I loaded the Pew data, I could run a regression. I chose to run two models, one regressing the absolute difference on the non-response rate and the second adding a control variable for in-person interviews. I added this second variable as prior research has found that people are more likely to give socially acceptable answers during in-person interviews than when filling out a survey in writing. For a regression technique, I'm using beta regression. Beta regression is a somewhat uncommon type of regression, but well-fitting for this case. It functions only with dependent variables that are between 0 and 1, such as percentages and GINI scores. In this case, my dependent variable is the rate of non-response.  

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
As can be seen here, there's a clear drop in the non-response rate as public opinion becomes more absolute on LGBT rights. Altogether, we'd expect an approximately 15% nonresponse rate in an evenly divided country, an 8% one in a country that's split 75/25, and only a 5% one in a country where almost the entire population holds one view. 

<h3><img align="center" height="500" src="https://github.com/vincentium123/Survey-Nonresponse/blob/main/basic%20PEW%20plot.jpeg"></h3>

This upholds my hypothesis, as does the fact that data from the World Value Survey showed the same effect. However, I wanted to be sure. To start, I ran a few checks, first removing all data points from countries where people were either strongly for or against homosexuality (about 90% agreement). After that, I removed all outliers- observations that had unusually high nonresponse rates using [Cook's Distance](https://www.mathworks.com/help/stats/cooks-distance.html). 

Both of these tests showed the same results. 

```
topline_no_zero2 <- topline_no_zero %>%
  filter(topline_no_zero$abs_diff <= 90)

#Pew Data Regressed again
beta_pew_r1 <- betareg(perc_refused ~ rel_abs_diff + in_person, data = topline_no_zero2)
beta_pew_small_r1 <- betareg(perc_refused ~ rel_abs_diff, data = topline_no_zero2)

beta_pew_r2 <- betareg(perc_refused ~ rel_abs_diff + in_person, data = beta_pew$model[!cooks.distance(beta_pew)> (4/length(topline_no_zero$country)),])
beta_pew_small_r2 <- betareg(perc_refused ~ rel_abs_diff, data = beta_pew_small$model[!cooks.distance(beta_pew_small)> (4/length(topline_no_zero$country)),])
```

Following that, I performed a placebo test, conducting the same analysis on a case where I did not expect significant results. Item nonresponse is partially related to item sensitivity. Therefore, an item that has highly polarized answers (lots of people answering yes and no) but is not socially sensitive should see no significant connection between non-response rates and polarization. For this, I selected how often survey respondents said they read the newspaper. 

The regression did not return significant results. The graph below shows the difference from the earlier, controversial, question clearly. Firstly, non-response rates are much lower- the highest is only 4%, versus around 30% for the question about homosexuality. Secondly, the trend line is essentially flat. Even if it were significant, the real-world effect would be minimal, only a difference of a percent or two. From this, we can see that it is indeed social desirability affecting the non-response rates. 

<h3><img align="center" height="500" src="https://github.com/vincentium123/Survey-Nonresponse/blob/main/newspaper%20plot.jpeg"></h3>

***

# LÄNDERÜBERGREIFENDE KULTURELLE AUSWIRKUNGEN AUF DIE NICHTBEANTWORTUNGSRATE VON UMFRAGEN

## Das Projekt

In diesem Projekt werden Umfragedaten mit Hilfe von Beta-Regression in R analysiert, um einen neuen Faktor für die Nichtbeantwortungsrate von Umfragen nachzuweisen: die Unsicherheit darüber, was die sozial akzeptable Antwort ist. Es basiert auf einer Arbeit, die ich für meinen Kurs "Data & Measurement" an der Universität Mannheim geschrieben habe. Ich habe diese Arbeit zusammen mit dem Replikationscode in dieses Repository aufgenommen. 

## Hintergrung
Die Gestaltung von Umfragen ist ein äußerst technisches Gebiet, auch wenn es auf den ersten Blick einfach erscheint. Forscher haben fast ein Jahrhundert damit verbracht, die Formulierung der Fragen, die verwendeten Skalen und die Präsentation der Umfragen zu verbessern, um bessere Daten zu erhalten. Eines der Hauptprobleme ist ein psychologisches Phänomen, das als "Social Desirability Bias" bezeichnet wird. Einfach ausgedrückt: Menschen neigen dazu, Antworten zu geben, von denen sie glauben, dass sie andere Menschen nicht kränken oder zu einer Beurteilung führen. Wenn eine Person beispielsweise von Coca-Cola befragt wird, könnte sie zögern, zu sagen, dass sie Pepsi bevorzugt. Dieser Effekt wird noch verstärkt, wenn die Fragen noch sensibler werden und sich auf Dinge wie Drogenkonsum, politische Ansichten und sexuelle Orientierung erstrecken. 

Die Verzerrung durch soziale Erwünschtheit äußert sich vor allem durch die Wahl der sozial "richtigen" Antwort und durch Antwortverweigerung. Im ersten Fall wählen die Befragten die Antwort, die sie für am sichersten halten. Unser hypothetischer Coca-Cola-Befragter könnte zum Beispiel sagen, dass er Cola liebt. Im zweiten Fall könnten die Befragten, wenn sie die Möglichkeit haben, mit "weiß nicht" antworten, ein Element leer lassen oder eine neutrale Option wählen. 

Beides kann für jeden, der Umfragedaten analysiert, problematisch sein. Im ersten Fall könnten die Analysten die Popularität der sozial "richtigen" Antwort überschätzen, während sie im zweiten Fall nicht feststellen können, ob die Leute die Frage einfach nicht verstanden haben oder einfach nicht antworten wollen. Diese Probleme sind bekannt, und die Ersteller von Umfragen haben Methoden entwickelt, um ihre Auswirkungen zu minimieren. 

Es gibt jedoch eine Möglichkeit, mit der sich die Ersteller von Umfragen nicht beschäftigt haben: wenn die sozial "richtige" Antwort unbekannt ist. Stellen wir uns ein Szenario vor. Die Leitung eines Unternehmens hat vor kurzem eine Änderung vorgenommen - zum Beispiel die Umstellung von reiner Remote-Arbeit auf Hybrid-Arbeit. Dieser Schritt war sowohl bei den Mitarbeitern als auch bei der Unternehmensleitung umstritten. Nun, einige Monate später, beschließt die Unternehmensleitung, eine Umfrage unter den Mitarbeitern durchzuführen, um deren Meinung zu erfahren. Diese Umfragen sind zwar anonym, aber die Mitarbeiter vertrauen der Unternehmensleitung nicht, dass eine "falsche" Antwort keine Konsequenzen nach sich zieht. Gleichzeitig ist das Management selbst gespalten, was die Mitarbeiter wissen. Aus der Sicht der Mitarbeiter könnte es daher Konsequenzen haben, wenn sie sich für die Telearbeit oder für die Hybridarbeit aussprechen. Die sicherste Entscheidung ist, keine oder eine möglichst neutrale Antwort zu geben. 

## Durchführung

Ich werde die Daten von zwei Umfragen analysieren: das Global Attitudes Project von Pew Research und die World Values Survey. In beiden Umfragen wurden Zehntausende von Menschen in Dutzenden von Ländern befragt, um die weltweiten Einstellungen zu Demokratie, Menschenrechten und Gesellschaft abschließend zu ermitteln. 

Als kontroverses Thema habe ich die Frage gewählt, ob Homosexualität von der Gesellschaft akzeptiert werden sollte. Homosexualität eignet sich hier gut, weil ihre gesellschaftliche Akzeptanz in den einzelnen Ländern sehr unterschiedlich ist. In einigen Ländern (Schweden, Kanada usw.) ist die Befürwortung der Rechte von Homosexuellen Standard, und diejenigen, die dies nicht tun, werden schlecht angesehen. In anderen Ländern (Nigeria, Ägypten usw.) ist die Unterstützung der Rechte von Homosexuellen eine extreme Minderheitenposition. Die meisten Länder liegen in der Mitte. Meiner Theorie zufolge sollte in Ländern, in denen die Akzeptanz von Homosexualität eine gesellschaftliche Standardeinstellung ist (entweder dafür oder dagegen), die Nichtbeantwortungsrate geringer sein als in Ländern, in denen das Thema umstritten ist. 

Fangen wir an. Um Platz zu sparen, zeige ich nur den Code, der sich auf die Daten von Pew bezieht, aber ich habe die gleichen Ergebnisse mit den Daten der World Values Survey gefunden. Der vollständige Code kann in diesem Repository gefunden werden. 

Zu Beginn habe ich die Daten geladen, bereinigt und die Variablen konstruiert. Da sich meine Frage auf den absoluten Unterschied der Meinungen bezog, habe ich eine Variable erstellt, die den absoluten Unterschied zwischen Pro- und Anti-LGBT-Meinungen anzeigt. Dann bereinigte ich den Datensatz ein wenig, indem ich NAs durch 0s in den booleschen Spalten ersetzte. 

```
#Importing the Pew data set
topline <- read_excel(here("raw-data","topline.xlsx"))
#Now I'm creating the variables I'll need for later analysis.
topline$diff <- topline$pro_LGBT-topline$anti_LBGT
topline$abs_diff <- abs(topline$pro_LGBT-topline$anti_LBGT)
#Plus cleaning it up a bit. NAs were present in the non-1 spaces of the dummy variables. 
topline = subset(topline, select = -c(...6))
topline[is.na(topline)] <- 0
```

Sobald ich die Pew-Daten geladen hatte, konnte ich eine Regression durchführen. Ich entschied mich für zwei Modelle, von denen eines die absolute Differenz auf die Nichtbeantwortungsrate regressierte und das zweite eine Kontrollvariable für persönliche Befragungen enthielt. Ich fügte diese zweite Variable hinzu, da frühere Forschungen ergeben haben, dass Menschen bei persönlichen Befragungen eher sozialverträgliche Antworten geben als beim schriftlichen Ausfüllen einer Umfrage. Als Regressionsmethode verwende ich die Beta-Regression. Die Beta-Regression ist eine etwas ungewöhnliche, aber für diesen Fall gut geeignete Art der Regression. Sie funktioniert nur mit abhängigen Variablen, die zwischen 0 und 1 liegen, wie z. B. Prozentsätze und GINI-Werte. In diesem Fall ist meine abhängige Variable die Quote der Antwortausfälle.  

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
Wie hier zu sehen ist, sinkt die Nichtbeantwortungsrate deutlich, wenn die öffentliche Meinung zu LGBT-Rechten absoluter wird. Insgesamt würden wir eine Non-Response-Rate von etwa 15 % in einem gleichmäßig gespaltenen Land erwarten, 8 % in einem Land, das 75/25 geteilt ist, und nur 5 % in einem Land, in dem fast die gesamte Bevölkerung eine Meinung vertritt. 

<h3><img align="center" height="500" src="https://github.com/vincentium123/Survey-Nonresponse/blob/main/basic%20PEW%20plot.jpeg"></h3>

Dies bestätigt meine Hypothese, ebenso wie die Tatsache, dass die Daten der World Value Survey den gleichen Effekt zeigen. Ich wollte jedoch sichergehen. Zunächst habe ich einige Überprüfungen vorgenommen und alle Datenpunkte aus Ländern entfernt, in denen die Menschen entweder stark für oder gegen Homosexualität waren (etwa 90 % Zustimmung). Danach habe ich alle Ausreißer entfernt - Beobachtungen, die ungewöhnlich hohe Antwortausfallquoten aufwiesen, indem ich [Cook's Distance](https://www.mathworks.com/help/stats/cooks-distance.html) verwendete. 

Beide Tests ergaben die gleichen Ergebnisse. 

```
topline_no_zero2 <- topline_no_zero %>%
  filter(topline_no_zero$abs_diff <= 90)

#Pew Data Regressed again
beta_pew_r1 <- betareg(perc_refused ~ rel_abs_diff + in_person, data = topline_no_zero2)
beta_pew_small_r1 <- betareg(perc_refused ~ rel_abs_diff, data = topline_no_zero2)

beta_pew_r2 <- betareg(perc_refused ~ rel_abs_diff + in_person, data = beta_pew$model[!cooks.distance(beta_pew)> (4/length(topline_no_zero$country)),])
beta_pew_small_r2 <- betareg(perc_refused ~ rel_abs_diff, data = beta_pew_small$model[!cooks.distance(beta_pew_small)> (4/length(topline_no_zero$country)),])
```

Anschließend habe ich einen Placebo-Test durchgeführt, bei dem ich die gleiche Analyse für einen Fall durchführte, bei dem ich keine signifikanten Ergebnisse erwarte. Item-Nonresponse hängt teilweise mit der Item-Sensitivität zusammen. Daher sollte bei einem Item, das stark polarisierte Antworten aufweist (viele Personen antworten mit Ja und Nein), aber nicht sozial sensibel ist, kein signifikanter Zusammenhang zwischen Non-Response-Raten und Polarisierung bestehen. Zu diesem Zweck habe ich ausgewählt, wie oft die Befragten angaben, die Zeitung zu lesen. 

Die Regression lieferte keine signifikanten Ergebnisse. Das nachstehende Diagramm zeigt deutlich den Unterschied zu der früheren, umstrittenen Frage. Erstens ist die Nichtbeantwortungsrate viel niedriger - der höchste Wert liegt bei nur 4 %, gegenüber rund 30 % bei der Frage zur Homosexualität. Zweitens ist die Trendlinie im Wesentlichen flach. Selbst wenn sie signifikant wäre, wäre der reale Effekt minimal, nur ein Unterschied von ein oder zwei Prozent. Daraus können wir ersehen, dass die Antwortverweigerungsquoten tatsächlich durch soziale Erwünschtheit beeinflusst werden. 

<h3><img align="center" height="500" src="https://github.com/vincentium123/Survey-Nonresponse/blob/main/newspaper%20plot.jpeg"></h3>


