# Connect to R Studio in Hadoop using
# website: vm-hadoop-11.cse.sc.edu:8787
# username: student
# password: same_as_the_lab
# Set environmental variables
Sys.setenv(HADOOP_CMD="/usr/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/usr/hdp/2.3.0.0-2557/hadoop-mapreduce/hadoop-streaming-2.7.1.2.3.0.0-2557.jar")

# Load the following packages in the following order
library(rhdfs)
library(rmr2)

# Initialize the connection from rstudio to hadoop
hdfs.init()

# Specify the path
hdfs.root = '/user/share/student'

# Append the data filename to the pathname
hdfs.data = file.path(hdfs.root,'test_25K.csv')

# Problem 1: Determine the number of flight cancellations for each
#            origin airport
map_1 = function(k, flights) {
  return (keyval(as.character(flights[[17]]),
                as.numeric(flights[[22]])))
}

reduce_1 = function(origin, counts) {
  keyval(origin, sum(counts, na.rm=TRUE))
}

map_reduce_1 = function(input, output = NULL) {
  mapreduce(input = input,
            output = output,
            input.format = make.input.format("csv", sep = ","),
            map = map_1,
            reduce = reduce_1)
}

# Append the output filename to the pathname
hdfs.out = file.path(hdfs.root,'out')

# Invoke the mapreduce functions on the input and output files
out = map_reduce_1(hdfs.data, hdfs.out)

# Fetch the results from HDFS
results = from.dfs(out)
results.df = as.data.frame(results, stringsAsFactors=F)
colnames(results.df) = c('Origin', 'Cancelled')
results.df

################################################################################
# Problem 2: Find the maximum taxi in time for each destination airport
map_2 = function(k, flights) {
  return (keyval(as.character(flights[[18]]),
                 as.numeric(flights[[20]])))
}

reduce_2 = function(destination, taxi_int) {
  keyval(destination, max(taxi_int, na.rm=TRUE))
}

map_reduce_2 = function(input, output = NULL) {
  mapreduce(input = input,
            output = output,
            input.format = make.input.format("csv", sep = ","),
            map = map_2,
            reduce = reduce_2)
}

# Append the output filename to the pathname
hdfs.out = file.path(hdfs.root,'out2')

# Invoke the mapreduce functions on the input and output files
out_2 = map_reduce_2(hdfs.data, hdfs.out)

# Fetch the results from HDFS
results_2 = from.dfs(out_2)
results_2.df = as.data.frame(results_2, stringsAsFactors=F)
colnames(results_2.df) = c('Airport', 'Max Taxi In')
results_2.df
