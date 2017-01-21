--- 
layout: post
title: "Baby names"
published: true
tags: R
type: post
status: publish
---
 
 
 

 
The dataset is available on data.gov. R can take care of downloading and unzipping the file.
 

{% highlight r %}
# Temporarily change working directory to a temp folder
# ...Don't worry, I'll change it back
old_wd <- getwd()
setwd(tempdir())
 
# Download and unzip dataset from data.gov
download.file("https://www.ssa.gov/oact/babynames/names.zip",
              destfile = "names.zip")
unzip("names.zip")
{% endhighlight %}
 
You now have a bunch of .csv files scattered across a temporary folder. Let's combine all of them into one tibble.
 

{% highlight r %}
setwd(tempdir())
 
file_list <- list.files(pattern = "yob[0-9][0-9][0-9][0-9]\\.txt$")
dataset <-
lapply(file_list, function(x) {
  dat <- read_csv(x, col_names = FALSE, col_types = cols(X2 = col_character()))
  dat$year <- str_extract(x, "[0-9][0-9][0-9][0-9]")
  return(dat)
})
 
dataset <- dataset %>%
  do.call("rbind", .) %>%
  tbl_df()
 
# See? I told you I'd change it back
setwd(old_wd)
{% endhighlight %}
 
I ran into an interesting problem with the `readr` package early in my analysis. The SSA puts all of the female baby names first in its datasets and by default `readr` guesses the data type for each column by looking at the first 1000 rows. Since it sees that the gender column for the first 1000 rows are all equal to 'F' it assumes that the 'F' must be shorthand for 'FALSE' so the column is logical. We can fix this behavior either by having `readr` look at more rows in its guess or by manually setting the column type. It's better to go with the latter if you already know the data type.
 
Another issue with the dataset is that while each file represents a separate year, there is nothing about the year in the actual data. I used regex and an anonymous function to strip the year out of each file name and assign the value to the `year` variable.
 
## Hooray hooray for EDA
Now that we have the data in a useable format, let's look at it.
 

{% highlight r %}
head(dataset)
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 × 4
##          X1    X2    X3  year
##       <chr> <chr> <int> <chr>
## 1      Mary     F  7065  1880
## 2      Anna     F  2604  1880
## 3      Emma     F  2003  1880
## 4 Elizabeth     F  1939  1880
## 5    Minnie     F  1746  1880
## 6  Margaret     F  1578  1880
{% endhighlight %}
 
The data look good, but the default column names aren't very informative.
 

{% highlight r %}
names(dataset) <- c("name", "gender", "count", "year")
{% endhighlight %}
 
That's better. Since we're looking at gender balance, let's make sure that the underlying data represent a roughly equal distribution of gender.
 

{% highlight r %}
dataset %>%
  group_by(gender) %>%
  tally(wt = count) %>%
  rename(count = n)
{% endhighlight %}



{% highlight text %}
## # A tibble: 2 × 2
##   gender     count
##    <chr>     <int>
## 1      F 168861581
## 2      M 171990331
{% endhighlight %}
The numbers are extremely close. Are all years in the dataset just as equal?
 

{% highlight r %}
gender_balance <- dataset %>%
  group_by(year, gender) %>%
  tally(wt = count) %>%
  rename(count = n)
 
ggplot(gender_balance, aes(x = year, y = count)) +
  geom_point(aes(color = gender)) +
  scale_x_discrete(breaks = seq(from = 1890, to = 2010, by = 10)) +
  theme_bw() +
  scale_color_manual(values=c("red", "blue")) +
  ggtitle("Names by gender by year")
{% endhighlight %}

![plot of chunk gender_balance](/figures/gender_balance-1.png)
 
The gender balance in the dataset is roughly equal for all years. The Social Security Act was passed in 1935, so there are understandably more people recorded in the Social Security Administration's database born in the 1920s and onward than the 1880s. Let's remove everybody born before 1920.
 

{% highlight r %}
dataset <- dataset %>%
  filter(year > 1919)
{% endhighlight %}
 
## Analysis
We're finally ready to take a crack at answering our initial question: what names have changed their most common gender over the years? To make it clearer what's going on, I created a function that returns the number of names that are male or female for a given time range. It's possible to do this in fewer lines of code with a `dplyr` chain.
 

{% highlight r %}
summarize_names <- function(start = 0, end = Inf, gender_arg = "F") {
  dataset %>%
    filter(count > 9) %>%
    filter(year >= start) %>%
    filter(year <= end) %>%
    group_by(name, gender) %>%
    tally(wt = count) %>%
    rename(count = n) %>%
    arrange(name, gender) %>%
    filter(gender == gender_arg) %>%
    select(name, count)
}
{% endhighlight %}
 
I defined the old timeframe as 1920-1950 and the new timeframe as 1985-2015.
 

{% highlight r %}
men_old <- summarize_names(1920, 1950, "M") %>% rename(men = count)
women_old <- summarize_names(1920, 1950, "F") %>% rename(women = count)
men_new <- summarize_names(1985, 2015, "M") %>% rename(men = count)
women_new <- summarize_names(1985, 2015, "F") %>% rename(women = count)
 
dataset_old <- inner_join(men_old, women_old) %>%
  group_by(name) %>%
  mutate(per_f_old = women / sum(men + women)) %>%
  select(name, per_f_old)
{% endhighlight %}



{% highlight text %}
## Joining, by = "name"
{% endhighlight %}



{% highlight r %}
dataset_new <- inner_join(men_new, women_new) %>%
  group_by(name) %>%
  mutate(per_f_new = women / sum(men + women)) %>%
  select(name, per_f_new)
{% endhighlight %}



{% highlight text %}
## Joining, by = "name"
{% endhighlight %}
 
Now we're ready to see the names with the greatest changes over time. Drumroll please!

{% highlight r %}
inner_join(dataset_old, dataset_new) %>%
  mutate(delta = per_f_old - per_f_new) %>%
  filter(abs(delta) > 0.69) %>%
  arrange(desc(abs(delta))) %>%
  rename(Name = name, `Per female old` = per_f_old, `Per female new` = per_f_new, `Change` = delta) %>%
  kable(digits = 2)
{% endhighlight %}



{% highlight text %}
## Joining, by = "name"
{% endhighlight %}



|Name    | Per female old| Per female new| Change|
|:-------|--------------:|--------------:|------:|
|Berlin  |           0.01|           0.98|  -0.97|
|Kelsey  |           0.03|           0.98|  -0.95|
|Ashley  |           0.05|           0.99|  -0.94|
|Gael    |           0.93|           0.00|   0.93|
|Karon   |           1.00|           0.07|   0.92|
|Lacey   |           0.07|           1.00|  -0.92|
|Whitney |           0.06|           0.98|  -0.92|
|Lindsey |           0.08|           0.99|  -0.91|
|Aubrey  |           0.04|           0.94|  -0.90|
|Morgan  |           0.00|           0.89|  -0.89|
|Kelley  |           0.06|           0.91|  -0.85|
|Clair   |           0.13|           0.97|  -0.84|
|Romie   |           0.01|           0.85|  -0.84|
|Ivon    |           0.12|           0.95|  -0.83|
|Tobey   |           0.82|           0.01|   0.81|
|Lorin   |           0.01|           0.81|  -0.80|
|Lacy    |           0.16|           0.97|  -0.80|
|Kelly   |           0.13|           0.93|  -0.80|
|Paris   |           0.04|           0.83|  -0.79|
|Marvel  |           0.92|           0.14|   0.78|
|Vernell |           0.79|           0.02|   0.77|
|Lindsay |           0.24|           0.99|  -0.76|
|Emerald |           0.25|           1.00|  -0.75|
|Ivey    |           0.25|           0.99|  -0.75|
|Merlyn  |           0.25|           0.97|  -0.72|
|Charley |           0.04|           0.76|  -0.72|
|Kendall |           0.03|           0.74|  -0.71|
|Alpha   |           0.83|           0.13|   0.71|
|Stevie  |           0.06|           0.76|  -0.71|
|Arlyn   |           0.27|           0.97|  -0.70|
|Brook   |           0.17|           0.87|  -0.70|
|Blair   |           0.02|           0.72|  -0.69|
 
 
There you have it, the name Berlin has experienced the greatest male-female shift over the past several decades, while Gael has the distinction of having moved the most in the opposite direction.
 
