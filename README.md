# quantile-estimator
Simple OCaml library to estimate quantiles

This library essentially implements a dynamic histogram of sorts. Each level in the tree corresponds to a closed interval on the real line with positive and negative infinity. Each interval has a capacity, a current count, and a left and right child. If an interval exceeds its capacity, then subsequent items are added to the left or right child.
