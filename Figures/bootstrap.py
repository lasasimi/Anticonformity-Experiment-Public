import numpy

def bootstrap(data, n: int = 10000, confidence: float = 0.95) -> list[float]:
    """
    Returns the lower and upper quantiles of the bootstrapped confidence interval for the mean statistic of the input data.

    # Arguments
    - `data`: an array-like dataset.
    - `n`: the number of bootstrap samples to draw (default is 10_000).
    - `confidence`: the confidence level for the quantiles (default is 0.95).
    """
    data = numpy.asarray(data)
    n_data = len(data)
    
    resampled_data = numpy.random.choice(data, size=(n, n_data), replace=True)
    bootstrapped_means = numpy.mean(resampled_data, axis=1)

    lower_quantile, upper_quantile = numpy.quantile(bootstrapped_means, [(1 - confidence) / 2, 1 - (1 - confidence) / 2])
    
    return [float(lower_quantile), float(upper_quantile)]