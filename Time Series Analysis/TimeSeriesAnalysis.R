# import the data
mauna_loa = read.csv("MaunaLoa.csv")

# Part 1: Detrend the entire data set using a linear model
y = mauna_loa$interpolated
x = 1:length(y)
fit1 = lm(y~x)
detrendedY = y - fitted(fit1)
plot.ts(detrendedY)

# Part 2: Detrend a subset of the data in the range 203:394
y = mauna_loa$interpolated[203:394]
x = 1:length(y)
fit2 = lm(y~x)
detrendedY = y - fitted(fit2)
plot.ts(detrendedY)

# Part 3: Remove the cyclic component by subtracting monthly average
monthlyAverage_2 = function(detrendedY, month) {
  observations = which(mauna_loa$Month == month)
  tmp = which(observations >= 203 & observations <= 394)
  observations = observations[tmp] - 202
  monthly_values = detrendedY[observations]

  return (sum(monthly_values) / length(observations))
}

deCycle = function(detrendedY) {
  results = c()
  for (i in 1:12) {
    monthly_average = monthlyAverage_2(detrendedY, i)
    results = c(results, monthly_average)
  }

  return (results)
}

plot.ts(detrendedY)
interval = seq(from=1, to=length(y), by=16)
decycledY = deCycle(detrendedY)
points(interval, decycledY, col="red")
lines(interval, decycledY, col="red")
