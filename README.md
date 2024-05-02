Lab2 - Estymacja parametrów
================
Dzmitry Navumau
February 2024

**Zadanie 1.** Chcemy obliczyć dla próbki x nieobciążony estymator
wariancji oraz estymatory największej wiarygodności wariancji i
odchylenia standardowego

``` r
x <- seq(from = 1, to = 10, by = 0.1)
v <- mean((x-mean(x))^2)
n <- 700
mean <-  1
sd <-  2
observations <- rnorm(n, mean, sd)
unbiased_variance <- sum((observations - mean(observations))^2) / (n - 1)
mle_variance <- sum((observations - mean(observations))^2) / n
mle_std_dev <- sqrt(mle_variance)
```

    ## Wariancja, estymator nieobciążony: 4.298541

    ## Wariancja, estymator ML: 4.292401

    ## Odchylenie, estymator ML_STD_DEV: 2.071811

**Zadanie 2.** *Trzy estymatory wariancji* Wylosujmy 5000 obserwacji z
rozkładu normalnego o średniej 0 i wybranym przez siebie odchyleniu
standardowym $\sigma$.  
Policzmy nieobciążony estymator wariancji:
$$\hat{S}^2 = \frac{1}{n-1}\sum_{i=1}^{n} (X_i - \bar{X})^2.$$ Dodatkowo
policzmy jeszcze dwa estymatory:
$$\hat{S}_1^2 = \frac{1}{n}\sum_{i=1}^{n} (X_i - \bar{X})^2$$ oraz
$$\hat{S}_2^2 = \frac{1}{n+1}\sum_{i=1}^{n} (X_i - \bar{X})^2.$$

``` r
n <- 5000
sigma <- 2
true_variance <- sigma^2
observations <- rnorm(n, mean = 0, sd = sigma)
matrix <- matrix(observations, nrow = 10, ncol = 500)
S <- apply(matrix, 2, var)
S1 <- S * (10 - 1) / 10
S2 <- S * (10 - 1) / (10 + 1)
```

Policzmy obciązenia wsystkich estymatorów. Widzimy że estymator
$\hat{S}^2$ ma najmniejsze obciązenie

``` r
bias_S <- mean(S) - true_variance
bias_S1 <- mean(S1) - true_variance
bias_S2 <- mean(S2) - true_variance
```

    ## Bias for S Variance Estimator: 0.0462

    ## Bias for S1 Variance Estimator: -0.3584

    ## Bias for S2 Variance Estimator: -0.6895

Liczymy odchylenie standartowe.

``` r
std_S <- sd(S)
std_S1 <- sd(S1)
std_S2 <- sd(S2)
```

    ## Standart derivation for S Variance Estimator: 1.9593

    ## Standart derivation for S1 Variance Estimator: 1.7634

    ## Standart derivation for S2 Variance Estimator: 1.6031

Widzimy ze najmniejsze odchylenie standartowe ma estymator $\hat{S}_2^2$

Mozemy rowniez obliczyc błędy średniokwadratowe:
$$RMSE = \sqrt{\mathbb{E}(\hat{\theta}-\theta)^2},$$
$$\hat{RMSE} = \sqrt{\frac{1}{n}\sum_{i=1}^n (\hat{\theta}_i - \theta)^2}.$$

``` r
RMSE_S <- sqrt(mean((S - true_variance)^2))
RMSE_S1 <- sqrt(mean((S1 - true_variance)^2))
RMSE_S2 <- sqrt(mean((S2 - true_variance)^2))

RMSE_mean_S <- sqrt(mean((S - true_variance)^2) * 1 / 10)
RMSE_mean_S1 <- sqrt(mean((S1 - true_variance)^2) * 1 / 10)
RMSE_mean_S2 <- sqrt(mean((S2 - true_variance)^2) * 1 / 10)
```

    ## RMSE for S Variance Estimator: 1.9579

    ## RMSE for S1 Variance Estimator: 1.7977

    ## RMSE for S2 Variance Estimator: 1.7436

    ## RMSE mean for S Variance Estimator: 0.6191

    ## RMSE mean for S1 Variance Estimator: 0.5685

    ## RMSE mean for S2 Variance Estimator: 0.5514

Widzimy ze najmniejszy błąd ma estymator $\hat{S}_1^2$

**Zadanie 3.** *Estymator odchylenia standardowego.* Zdefiniujmy funkcje
dla generacji probki dla standartowego odchylenia 2

``` r
sample_sd <- function(N, n) {
  samples <- matrix(rnorm(N * n, sd = 2), nrow = N)
  sds <- apply(samples, 1, sd)
  return(sds)
}
```

Wylosujmy probe dla róznych liczbnosci i zrobimy z tego jedyny
`data.frame`

``` r
n <- 2:100
sd_estimates <- sapply(n, sample_sd, N = 100)
sd_to_plot <- data.frame('SD'=c(sd_estimates), 'n'=rep(n, each=100))
```

Przedstawmy otrzymaną zależność na wykresie

``` r
plot <- ggplot(sd_to_plot, aes(x = n, y = SD)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  labs(title = "Estimated Standard Deviation vs. Sample Size",
       x = "Sample Size",
       y = "Estimated Standard Deviation")
plot
```

![](parameters-estimation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

**wnioski:**

**Zmniejszenie zmienności:** W miarę zwiększania się rozmiaru próbki
zmienność estymat odchylenia standardowego wydaje się zmniejszać. Można
to zauważyć po zmniejszeniu rozproszenia punktów wzdłuż osi pionowej, w
miarę przesuwania się w prawo wzdłuż osi poziomej.

**Zbieganie do prawdziwej wartości:** Wraz ze zwiększaniem się rozmiaru
próbki, estymaty odchylenia standardowego prawdopodobnie zbliżają się do
prawdziwej wartości odchylenia standardowego całej populacji. Jeśli
prawdziwa wartość jest znana, może być dodana do wykresu dla wizualnego
porównania.

**Efekt prawa wielkich liczb:** Wykres demonstruje prawo wielkich liczb,
ponieważ zwiększając rozmiar próbki, estymaty stają się mniej
rozproszone i bardziej skoncentrowane wokół prawdziwej wartości
parametru.

**Zadanie 4.**

``` r
data("iris")
setosa_data <- iris[iris$Species == 'setosa', ]
```

Obliczmy srednie i odchylenia standartowe

``` r
mean_sepal_width <- mean(setosa_data$Sepal.Width)
sd_sepal_width <- sd(setosa_data$Sepal.Width)
```

    ## mean Sepal.Width: 3.428

    ## sd Sepal.Width: 0.3790644

``` r
mean_sepal_length <- mean(setosa_data$Sepal.Length)
sd_sepal_length <- sd(setosa_data$Sepal.Length)
```

    ## mean Sepal.length: 5.006

    ## sd Sepal.length: 0.3524897

Obliczmy wspolczynniki korelacji Pirsona

``` r
correlation_sepal <- cor(setosa_data$Sepal.Width, setosa_data$Sepal.Length)
correlation_petal <- cor(setosa_data$Petal.Length, setosa_data$Sepal.Length)
```

    ## Korelacja między Sepal.Width oraz Sepal.Length: 0.7425467

    ## Korelacja między Petal.Length oraz Sepal.Length: 0.2671758

Na podstawie czego można wnioskować, że jest liniowa zalęzność pomiędzy
`Sepal.Width` oraz `Sepal.Length`. Jednak `Petal.Length` oraz
`Sepal.Length` słabo są ze sobą powiązane.

Przedstawmy tę zależnosci na wykresach, by się upewnić

``` r
ggplot(setosa_data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  labs(title = "Sepal Width vs. Sepal Length for Setosa",
       x = "Sepal Length",
       y = "Sepal Width") +
  theme_minimal()
```

![](parameters-estimation_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggplot(setosa_data, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  labs(title = "Petal Length vs. Sepal Length for Setosa",
       x = "Sepal Length",
       y = "Petal Length") +
  theme_minimal()
```

![](parameters-estimation_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Jak widzimy, informacja na wykresach potwierdza po raz kolejny
prawdziwość tezy o liniowej zależnosci.

**Zadanie 5(dodatkowe).** Chcemy wyestymować pole koła o promieniu 1
metodą Monte Carlo

Będzimy potrzebowali dwie funckje:

Pierwsza sprawdza czy pukt trafił do koła

``` r
is_in_circle <- function(point) {
  x <- point[1]
  y <- point[2]
  return(x^2 + y^2 <= 1)
}
```

Druga oblicza jaka część punktów trafiła do koła

``` r
calculate_proportion <- function(points_matrix) {
  inside <- apply(points_matrix, 2, is_in_circle)
  mean(inside)
}
```

Teraz tworzymy macierz punktów:

``` r
n <- 10000
random_matrix <- matrix(runif(2 * n), nrow = 2)
```

Estymatorem może byc cześć punktów która trafiła:

    ## [1] 0.7854

Zbadaymy obciążenie estymatora:

``` r
calculate_circle_area <- function(n) {
  points_matrix <- matrix(runif(2 * n), nrow = 2)
  proportion_inside <- calculate_proportion(points_matrix)
  estimated_area <- proportion_inside
  return(estimated_area)
}

num_experiments <- 100

area_estimates <- replicate(num_experiments, calculate_circle_area(n))

mean_estimate <- mean(area_estimates)

true_area <- pi

bias <- mean_estimate - true_area
```

    ## Mean estimate: 0.785191

    ## True area: 3.141593

    ## Bias: -2.356402
