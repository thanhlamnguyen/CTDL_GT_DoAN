

selection_sort <- function(x)
{
  n <- length(x)
  for (i in 1 : (n - 1))
  {
    min_index <- i
    for (j in (i + 1) : (n))
    {
      if (x[j] < x[min_index]) {
        min_index = j
      }
    }
    temp <- x[i]
    x[i] <- x[min_index]
    x[min_index] <- temp
  }
  x
}

arr <- sample(1 : 100, 10)
sorted_arr <- selection_sort(arr)
sorted_arr
