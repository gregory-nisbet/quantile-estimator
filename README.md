# quantile-estimator
Simple OCaml library to estimate quantiles

This library essentially implements a dynamic histogram of sorts. Each level in the tree corresponds to a closed interval on the real line with positive and negative infinity, also known as _the two point compactification of the real line_. Each interval has a capacity, a current count, and a left and right child. If an interval exceeds its capacity, then subsequent items are added to the left or right child.

Each interval in the interval tree keeps track of its minimum and maximum, which are used for estimating quantiles. When estimating the _empirical distribution function_ (analogous to a CDF for a sample), we assume that, within each level, the data is drawn from a uniform distribution between the minimum at that level and the maximum at that level.

For instance, if we observed 0, 0, 0, 0, 0, and then 1 and stored it on a single level and later asked for the median, we would get back an estimate of 0.5 for the median. This is not equal to the true sample median, 0, but it does serve to demonstrate how estimation works within a single level.

When an interval reaches capacity, we split it in half using the *mean* of the interval. One of the additional pieces of bookkeeping information that we keep track of for a given interval is the *sum* of the values observed so far. Since we know the number of items, this gives us the sample mean as well. In principle, we could use the sample mean when computing our estimate to the EDF as described above, but as of right now we do not. A uniform distribution is the maximum entropy distribution that is constrained by a finite population maximum and population minimum. Presumably, you get a different family of maximum entropy distributions if you have a finite population maximum, finite population minimum, and a given mean.

Splitting the _jurisdiction_ of an interval based on the mean of the items inside it is supposed to guide the point where the tree is deepest to the places where the data is clustered.

I have asked the Math StackExchange community for help in identifying a more appropriate distribution here: https://math.stackexchange.com/questions/3123162/maximum-entropy-distribution-given-constrained-minimum-maximum-and-mean
