# Mergers, Acquisitions & Company Valuations 

**Name: Li-Yu Oon, Armen Khachatrian, Tsai Ling Jeffrey Wong**



## Introduction/Objective

Mergers and acquisition is a term familiar to many. Companies participate in mergers and acquisition for a myriad of reasons. For some, it might be to eliminate competition, for others, it might be to lower production or operational costs. During the past decade, we have observed a rise in the tech industry. The tech industry is different from traditional industries, in the sense where companies in traditional industries are valued mainly by their assets and for many tech firms, their main source of valuation is consumer confidence. In the past, it is easy to predict a company's value after a merger or acquisition because it is a lot more straightforward. If a company merges or acquires another firm with a healthy balance sheet, the newly formed alliance would usually yield positive results. However, with the new generation of tech companies, it is no longer the case. Some companies like WeWork, Uber, and many others operate at a significant deficit with negative balance sheets. However, these companies have extremely high valuation allowing them to hedge their valuations against massive loans that puts them in a position to acquire multiple subsidiaries and swallow competitions. In a situation like this, it becomes immensely harder to determine with a merger or acquisition actually contributes to a company's growth in terms of stock prices. 

For this project, we set out to determine if a causal relationship between a company's growth and an M&A event exists. Intuitively, M&As are strategic investment plans, and we should be able to find out if and how much does M&A affect a company's growth. This information can give us further insight into the firm's investment strategies and explore other reasons why firms opt for M&A instead of other investment options.



## Data Acquisition/Data Description

In this project, we used a few different datasets at different points of the project. Our initial dataset was taken from CrunchBase using the CrunchBase API, extracting data containing about 25 firms and all their public M&A data. These firms are mostly in the technology sector. Subsequently, after our initial data exploration, we needed more observations and had to export data from the Bloomberg terminal. It gave us access to data for almost every publicly listed company. However, the project's primary focus is to find out how an M&A event would affect a company's growth. We limited the project's scope by using all the M&A events for 15 tech companies, varying in size, and the percentage change in stock price ten days prior and ten days after the M&A event was announced. We also exported another dataset containing the top 2000 largest publicly announced M&A. Other data like short-term interest rate is taken from [Libor USD](http://iborate.com/usd-libor/). We combined the different data sets with M&A statistics, company statistics, economic indices, and market indices for our final methodology. Our total sample size is roughly 15,000 across all M&A events and companies. 



## Methodology and Findings (1) - Regression Discontinuity 

Our preliminary plot focuses on three companies with the most amount of M&A events from 1988 to 2016. Each dot on the plot below represents an M&A event, and the y-axis denotes the percentage change in price when that M&A event is announced. We can see the distribution in the figure below; there are slightly more dots above the x-axis telling us that for these three companies, M&A events seem to net them more positive growth than negative. We can also see some seasonality from the density of the dots. Before 2000 there appears to be a higher density of dots compared to 2001-2003, the same can be said for 2006 compared to 2008-2009. This is most likely due to the financial crisis during that period. 

<img src="https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291564023_WhatsApp+Image+2021-02-25+at+4.06.35+PM.jpeg" alt="img"  />

We use the same methodology on a dataset consisting of the top 2600 most significant M&A events from the Bloomberg terminal with some preliminary results. From the figure below, we observed that the distribution of dots is now more symmetrical than before. However, we can still see slightly more dots above the x-axis than below, matching our findings from our initial data analysis.

![img](https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291310457_WhatsApp+Image+2021-02-28+at+11.31.25+AM.jpeg)

Because there are so many overlapping dots, it is hard to determine exactly what is going on. To get a clearer picture, we decide to split the data visualization by industries. We start to see that the distribution of dots begins to symmetrical, going against our initial intuition of M&A events having a positive impact on a company’s growth. Throughout all the major industries, the trend follows a very symmetrical distribution on both sides of the x-axis. The only variation between industries is the variance for growth.

![img](https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291313969_WhatsApp+Image+2021-02-28+at+11.36.45+AM.jpeg)

![img](https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291319322_WhatsApp+Image+2021-02-28+at+11.36.57+AM.jpeg)

![img](https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291323212_WhatsApp+Image+2021-02-28+at+11.41.52+AM.jpeg)

With conflicting results, we realized that just using percentage growth from 10 days and ten days before is not necessarily the best index to encapsulate a company's growth. We begin to fine-tune our growth index. We decided to compare the 10-day raise before an M&A event was announced and the 10-day growth after instead.

We can see from the graphs presented above that we have a kind of "positive" effect. We want to understand if this impact is causal in nature. We ran a regression with the observed data and got non-significant results (p-value is more than the threshold). Based on the nature of M&A events, it is infeasible to do a Randomized controlled trial (RCT) for comparing what is going on before and after the announcement date. That is why we applied Regression Discontinuity (RD) since this analysis applies to cases when the assignment for treatment (M&A) is based on a specific rule. That’s why we took a look at one of the most well-known approaches, which doesn’t depend on RCT. RD is one of the most credible identification strategies in non-experimental settings. This method requires only weak and easily interpretable identifying assumptions and permits a very robust estimation of causal treatment effects within a given neighborhood. The Key Idea of RD is that some structured rule creates a jump or a discontinuity in treatment status according to otherwise continuous running variable. Formally, we construct a treatment dummy that is 1 after the cutoff on the x-axis (vote margin) and zero before. After that, we regress outcomes on this treatment and the variable on the x-axis (and other covariates). In our case, the control group includes the period (10 days) before the announcement date, while the treatment group involves ten days after the announced M&A. RD can be run in two ways: "discontinuity at the cutoff point" and "local randomization". The first approach is related to the direction and magnitude of the "jump" to measure the treatment's causality. Local randomization shows that differences just below and above the threshold are random, and the presence of treatment must cause the difference in the subsequent mean values. Besides, the RD method must meet key assumptions to present unbiased causality results:

- Model specification
- Continuity 

Model specification is solvable since after running a simple regression, we implemented different transformations such as polynomial and logarithmic regressions. However, in our case, we did not get any significant results even though we applied the abovementioned improvements. We also restricted data around the cutoff (from day -2 to day 1) for approximately linear functions (yields a bias-variance tradeoff). Regarding the second assumption, we also did not violate it because we looked at the period over 20 days, and the general trend of the market was still going up until it faced the recession (the market fell). The market trend is much stronger than the M&A event. In the next section, the features importance plot will illustrate that the market indices (NASDAQ, Dow Jones, S&P500) prove crucial in comparison with either the M&A event or interest rates. In other words, if the market goes up, stock prices will also increase without focusing on the M&A. Although, if the market is stable and the jump in the stock prices occurs, it will be definitely caused by M&A. 

Despite the non-violation of RD design's critical characteristics, the box plots reveal that the medians remained almost similar at all. These graphs lead us to conclude that using this method does not prove the causality of M&A on the companies' growth.

We can do a box plot to compare the average percentage growth before and after an M & M&A event is announced. We use percentage growth because it automatically normalizes all the companies' prices, so we can plot all of them in one figure. From the box plot below, we can see that the variance is smaller after an M&A event. However, the mean growth percentage is still very close to zero for both before and after. 

<img src="https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291397191_WhatsApp+Image+2021-03-05+at+1.30.12+AM.jpeg" alt="img"  />

To get a better understanding, we look at the boxplot for one individual company. The company we selected is Apple(AAPL). By looking at a single company, we can see a decrease on average in company growth after an M & M&A event for Apple. We started to realize that recording the impact of an M&A event is not as straightforward as we thought. A company such as Apple may acquire another company now only to use its IPs a few years down the road to creating a new product which in turn would cause the company’s valuation to skyrocket. (Eg. Siri) A company can also plan to buy multiple companies for their investment plan, and the growth will not be noticeable until all the companies in their investment plan have been acquired, which might take a few years to complete. Taking a look at ten days prior and after will not capture the growth of such investment strategies adopted by companies. Intuitively, we can lengthen the horizon before and after the M&A event to ‘capture’ more growth but increasing the number of days into months/years before and after may introduce more noises into the dataset, which could make singling out the impact of M&A almost impossible. 

```python
plt.figure(figsize=(16,8))
plt.xlabel('Annoucement Day', fontsize=12)
plt.ylabel('Growth', fontsize=12)
sns.boxplot(x = 'day', y = 'quote', data = disc01.loc[(disc01['day']>-3)&(disc01['Ticker']=='AAPL')])
plt.show()
```

![img](https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618604609494_image.png)



Next, we tried to normalize the growth index by day and included market growth as a control variable. We wanted to use market growth as a control because the M&A event can happen at different business cycles. Ultimately, the growth index is based on the stock price, and the price is highly correlated to market sentiment. Our intuition is that a slight growth during a recession should be more significant than a considerable growth during a bull market. To test the correlation of our growth function with market growth, we ran a simple regression. The result showed that the market only affected the growth slightly with a coefficient of 1.0349. The regression also tells us that an M&A event has a close to zero effect on our growth index. However, due to the high p-value, the result is not significant and would require us to improve our model or find another approach.

```python
model = ols(formula='quote ~ MA + market', data = df02).fit()
model.summary()
```

<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618605007415_image.png" alt="img" style="zoom:50%;" />





Moreover, we tried to add more explanatory variables to get more reasonable results. We ran new OLS regression since we extracted the year from the announcement date and encoded both industry and ticker. The table below pointed out that industry and day are insignificant in this model. We scaled all the columns to obtain normalization.

Simultaneously, we get a significant result for dummy variable M&A based on the t value. 

<img src="https://paper-attachments.dropbox.com/s_812388E5C112C9D4AF24637589260570C1F375ABE2BF0A694701A6547E2309F9_1615310572807_Screenshot+2021-03-09+at+20.21.53.png" alt="img" style="zoom:50%;" />

The result of the above mentioned regression led us to switch the model to polynomial. The squared day was also insignificant based on the summary table presented below. This model emphasized that we reject the null hypothesis related to M&A with 99%+ probability. 

<img src="https://paper-attachments.dropbox.com/s_812388E5C112C9D4AF24637589260570C1F375ABE2BF0A694701A6547E2309F9_1615310656907_Screenshot+2021-03-09+at+20.23.37.png" alt="img" style="zoom:50%;" />

Next, we decided to select only significant explanatory variables. Excluding days led to fixing only the quote based on the fact before/after the merger without considering its day. The updated model includes only ticker, year, market, and M&A. 

```python
reg2 = smf.ols(formula = "quote ~ Ticker_num + year + MA + market", data = dff).fit()
print(reg2.summary())
```

The output is presented below.

<img src="https://paper-attachments.dropbox.com/s_812388E5C112C9D4AF24637589260570C1F375ABE2BF0A694701A6547E2309F9_1615310681777_Screenshot+2021-03-09+at+20.22.01.png" alt="img" style="zoom:50%;" />

We have laid out the price ratio distribution based on the days before and after the M&A announcement (on day 0). If the company stock value is lower than the price on day 0, the ratio will be smaller than 1, and vice versa. We can see that the price variation was still relatively symmetric before and after the M&A. 

<img src="https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291635858_WhatsApp+Image+2021-03-08+at+8.19.44+AM.jpeg" alt="img"  />

In the following phase, we wanted to see whether the distribution was “clouded” by both uptrend and downtrend of different companies on the same day. Therefore, we filtered out only one company (in this case, Microsoft) to see if a more distinctive pattern would be found. Unfortunately, we continued to see the non-differentiable distribution. Therefore, we will need to add more complexity to the model, such as increasing the number of features. The reason that we could not see a pattern could be due to an unspecified model.

<img src="https://paper-attachments.dropbox.com/s_39FD7C4012DF6E8551C2500F8761F28CFEDA709081B02411C662F12230013BE5_1615291659738_WhatsApp+Image+2021-03-09+at+2.04.23+PM.jpeg" alt="img"  />



## Methodology and Findings (2) - Honest Causal Forest

In the previous model, we could not find a distinctive "dis-continual" pattern that could show the M&A treatment might impact the company's value. It could be because the previous model we used is just "too simple" to explain the reality. In reality, there could definitely be more than just the M&A activities impacting the price of the companies. To further specify explanatory variables and look for potential "heterogeneous treatment effects" (HTEs) of the corresponding features. We were to introduce the use of Athey and her colleagues' "Honest Causal Forest" model. Random Forests have a strong ability to find a complex model, and the causal forest model designed by Athey's team adds more layers into it. The package is named `grf` in `R`.

Instead of finding the average treatment effect (ATE), we would also like to understand whether the treatment will have a different impact among different populations. The most intuitive example is whether the medicine will have different influences between younger and older generations. The HTE of the younger group could be 4 and older people 2, whereas the ATE is 3. Had we known the treatment was relatively lighter in older generations, one could have used another complimentary means on top to get the best result in the various target group.

This model is “causal” because it searches the subgroup and decision threshold (e.g: for market index above 200) based on the largest $$Y(treatment)-Y(control)$$, rather than the MSE in a typical regression forest. In this case, we assumed that the rest of the attributes are similar because we used the same company before and after the M&A. To do better control, on top of the variables in the previous model (acq ID, company ID, days before and after M&A), we also added variables such as market index (SP500, NASDAQ, Dow Jones) and the interest rates (Libor 1M, 2M, 3M, 6M, 12M). We believe these variables will help to explain the model better, market indices and interest rates could be the key control features to "filter out" the M&A effect, if any. We used the logarithm version of the price and market indices for the purpose of normalization. 

![img](https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618286735793_image.png)

The model is called “honest” because it was also designed to prevent overfitting of the data. The sample will be divided into the “splitting” set and “estimating” set. The splitting set is used to “train” the model with the decision threshold mentioned above and used to identify HTEs if any. Meanwhile, the magnitude of the estimated treatment effects will be“predicted” by the estimating set. It occurs because the double-check approach makes it less likely that spurious HTEs will survive: The spurious heterogeneity which survives the first subsample will show a close to zero effect in the second subsample. The additional benefit of fitting the data in such a way, according to Athey et al., is that the results would be asymptotically normal (Statistically, the estimated results will be distributed normally if the sample size goes indefinitely) 

Now let us test the data with this model. Since M&A events are considered timely activities, randomly selecting the data split might cause more recent data used in the splitting set instead of estimating set. To avoid such data leakage, we first set the data in chronological order in terms of the acquisition announcement date and picked the first 60 percent of the data as the splitting set.

```R
# split the data into splitting and estimating subsamples
id <- unique(data$acqID)
cases <- id[1:round(length(id) * .6)] #set the chronological order
train <- data[which(data$acqID %in% cases), c(1, 4:ncol(data))]
test <- data[-which(data$acqID %in% cases), c(1, 4:ncol(data))]
```

We use the splitting set to “train” the causal forest. Y is the response variable, `log_price` , whereas W is the treatment, in this case, the M&A announcement action. X would be the rest of the variables. We started the forest with 5000 trees.

```R
library(grf)
cf <- causal_forest(
 X = model.matrix(~ ., data = train[, -c(4,11)]),
 Y = train$log_price,
 W = train$M_A,
 num.trees = 5000,
 seed = 12345,
 min.node.size = 10)
```

Then we use the estimating set to “predict” the treatment effects.

```R
preds <- predict(
 object = cf, 
 newdata = model.matrix(~ ., data = test[, -c(4,11)]), 
 estimate.variance = TRUE)
test$preds <- preds$predictions
```



The first round of data analysis is to look at the distribution of the treatment effects. 

```R
ggplot(test, aes(x=preds)) + geom_histogram() + theme_light() + 
  labs(x='Estimated Treatment', y='Frequency')
```

There were 2 distinctive groups of treatment effects. It seems that there were some HTEs, so we will need to do further investigation.

<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618470222550_image.png" alt="img" style="zoom:50%;" />

Unlike of linear regression, the significance of the features in Random Forests cannot simply be decided by reading the p-values. However, we instead can run a feature importance test to see which explanatory variables contribute the most to our model, the top features are shown in the figure below. We obtained the result is not surprising and relatively intuitive, with the market indices and interest rates hold the top spots. With our findings, we focus our analysis on the top 4 features, `Log_NASDAQ`, ` log_DJ`, ` libor_1M` and `libor_12M`.

```R
features <- cf %>% 
 variable_importance() %>% 
 as.data.frame() %>% 
 mutate(variable = colnames(cf$X.orig)) %>% 
 arrange(desc(V1))
features <- features[features$V1>0.004,]
ggplot(data=features, aes(x=V1, y=reorder(variable, V1))) + geom_point() + 
 theme_light() + labs(x="Importance", y="")
```

<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618338288081_image.png" alt="img" style="zoom:50%;" />

The graphs below tell us more about how the treatment effect varies based on the top 4 variables.

<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618343159248_image.png" alt="img" style="zoom:50%;" />

It gives us some insight into the nature of the relationship between our treatment effect and the other covariates. We can then use these results to limit our scope of the model further if necessary. We see some exciting results, the 2 market indices have a similar relationship with our treatment effect. The inverted parabola pattern that we observe is not immediately intuitive. The results tell us that the treatment effect seems to be highest when market indices are in "mid-ranges". When the market indices were under or over performing, the treatment effect due to M&A seems to fall off. It is also worth noting that the fall-off trend is steeper when the market is underperforming. There appears to be a certain "pessimistic threshold", which the investor believes M&A would have no impact on the companies' value whatsoever. Meanwhile, the "optimistic threshold" of the market was not as abrupt as the pessimistic one. The risk-adversity of the investor could explain it. People are more risk-averse when the stock market is bearish and vice-verse when bullish.

The relationship between interest rates goes against the traditional macroeconomics school of thought, where a higher short-term interest rate will reduce borrowing and investments. However, in real-world situations, many corporations' and companies' borrowing activities do not align with interest rate fluctuation. Companies usually do not borrow capital at the time frame where M&A deals are being negotiated. Majority of the cases, companies borrow when interest rates/opportunity costs are lower and hedge their positions with future contracts before any massive investment event. Furthermore, larger companies can use different financial instruments, such as shares and bond exchanges, to fund M & M&A deals and not just rely on cash alone. From the figure above, we can see that past a certain interest rate level, the effect becomes a flat line, so it has no further impact on the model.

Now we want to see whether there are actually any HTEs based on a certain group of the data. Without reinventing the wheel, we used the function from [Mark White's blog](https://www.markhw.com/blog/causalforestintro):

```R
plot_htes <- function(cf_preds, ci = FALSE, z = 1.96) {
 if (is.null(cf_preds$predictions) )
  stop("cf_preds must include a matrix called 'predictions'")
 
 out <- ggplot(mapping = aes(
   x = rank(cf_preds$predictions), 
   y = cf_preds$predictions)) +
  geom_point() + labs(x = "Rank", y = "Estimated Treatment Effect") +
  theme_light()
 if (ci) {
  out <- out +
   geom_errorbar(
    mapping = aes(
     ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
     ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates)))}
 return(out)}
```

<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618348596039_image.png" alt="img" style="zoom:50%;" />

To test and see if the treatment effects are heterogeneous, we plot the predicted treatment effects by rank. Our estimated treatment effect on the y-axis, and we order them in ascending order on the x-axis. At first glance, it would seem that we have some sort of heterogeneous treatment effect, however, we need to note that we have a limited range on the y-axis.  Since the casual forest can quantify our uncertainty, we included confidence interval in our plot, and immediately, the treatment effect does not look like a variable. From the figure below, we can see that the treatment effects are still quite close to zero and almost half of the confidence interval are below the zero line. With that said, it may still be the fact that the the treatment effect were washed out among different groups of companies. So we will try to use the quantile analysis as the last resort.



<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618560675702_image.png" alt="img" style="zoom:50%;" />



## Methodology and Findings (2.1) - Quantile Analysis

In the previous section, we found that the treatment effect was still relatively homogenous or we should say that the model is still unable to capture any meaningful HTE signals. The HTEs, however, might be averaged out in all different companies. Let us try the last resort to see whether there will be HTEs within a company's specific size. We now divided the data into 4 quantiles based on the size (based on `log_price`) of the acquirers before the M&A (`M_A` = 0), and then we re-ran the causal forests in each group separately.

Looking at the first three quartiles, we got a totally different pattern from our previous causal forest. These are the smallest companies in terms of size in our dataset. It would seem that the treatment effect for this group of companies are still very close to zero line and almost half of the confidence intervals were below the line.

Meanwhile, in the fourth quantile, the estimated treatment effect is the highest among four and with a more distinctive confidence interval. Most of the confidence interval is above the zero line, hence more meaningful treatment effect. It could be due to the fact that higher companies have more resources to tackle M&A and were able to get the best results among other smaller scale firms.



<img src="https://paper-attachments.dropbox.com/s_7D2764BC2BA0FF0E80EC95CD2D6CF181C039CA73AD889DAC62613FA3AC2226D3_1618557502588_image.png" alt="img" style="zoom:50%;" />



## Conclusions / Future Usage

In summary, when we are using the initial 5000 tree analysis on the entire dataset, naturally, we have a lot of noise present. As suspected, estimated the treatment effect for different size companies are different, some are larger than others. Some are more positive, and some are more negative. When we group them all and try to find the average treatment effect, we ended up with a homogenous treatment effect that is relatively close to zero. After doing quantile causal forest regression, the treatment effect for each quartile is more clearly shown.

After analyzing our result, one of the theory that we have is that network effect could be present in the first 3 quartiles . When a company witnesses a competitor acquiring companies within the same industry for either expansionary or predatory reasons, a smaller company might feel compelled to also participate in M&A to buy out smaller companies to keep costs down (from out-source to in-house) or to retain market share. Smaller companies do not have access to resources that larger companies have and are working with a smaller and tighter balance sheet and budget. When a company in such a situation acquires other companies, this might lead to cash flow problems in the short run, and the growth effect of the M&A event will be overshadowed by the issues that it may bring. It will result in a smaller or sometimes even negative treatment effect.

Companies in the 4th quartile show heterogenous treatment effect, this could be because companies like Apple, Google and Microsoft have large amount of resources and they can engage in larger amount of M&A events. This increases the probability of a larger successful M&A events. Companies in the 4th quartile have huge valuations, making the impact of negative M&A events less significant to the overall value of the company. For these companies, M&A events have a small downside and a way larger upside. Another theory that we have is that larger companies are able to acquire multiple smaller companies and their IP. This allow them to combine the newly acquired IP to create new products or services. Compared against smaller companies which only have resources to acquire 1 or 2 firms within their supply chain to reduce operational cost, the impact of M&A for these larger companies tend to be greater and more obvious. A combination of one or more of these reasons can lead to the a non-zero treatment effect as shown by our graphs previously.

The two theories above still requires us to dig deeper into the individual quartiles to test for network effects and regression discontinuity to confirm our hypothesis. This could serve as a future expansion feature for the project.

In conclusion, trying to estimate a company’s growth through an M&A event is extremely difficult. Many factors can contribute to a company’s growth. To single out the treatment effect of M&A without a randomized control trial is close to impossible. We can only give a very rough estimate and try our best to use the best explanatory variables that we can get a  hold on. Companies participate in M&A events for many reasons. Sometimes it might not even be for growth. Investment strategies adopted by these big corporations may span over a long period, which our simple model will not be able to account for. There is also selection bias as we only picked companies within the technology sector, in which M&A activities occurred more frequently than other industries. Lastly, reverse causality may also be present because companies with more resources are more likely to acquire than those with limited revenue. All these limitations can contribute to our less-than-ideal results.



## Reference

- Hausman, C., Rapson, D. (2018), Regression Discontinuity In Time: Considerations for Empirical Applications, https://www.nber.org/system/files/working_papers/w23602/w23602.pdf
- Lee, D.,  Lemieux, T. (2010), Regression Discontinuity Designs in Economics, https://www.princeton.edu/~davidlee/wp/RDDEconomics.pdf
- Athey, S. (2019), Estimating Treatment Effects with Causal Forests: An Application, https://arxiv.org/pdf/1902.07409.pdf
- Tibshirani, J. (2020), Generalized Random Forests, https://cran.r-project.org/web/packages/grf/grf.pdf
- White, M. (2018), Explicitly Optimizing on Causal Effects via the CausalRandom Forest: A Practical Introduction and Tutorial, https://www.markhw.com/blog/causalforestintro