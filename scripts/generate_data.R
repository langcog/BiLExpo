
generate_data <- function(n_children = 100,
                          n_concepts = 100,
                          n_languages = 2,
                          age_effect = .05) {
  set.seed(123)

  # Define dimensions
  I <- n_children 
  W <- n_concepts * n_languages # assumes all languages have a word for each concept
  C <- n_concepts 
  L <- n_languages   # two languages
  N <- I * W * L # total number of observations

  # Simulate mappings:
  child <- rep(1:I, each = W * L)  # Each child takes 100 words in 2 languages
  word <- rep(rep(1:W, each = L), times = I)  # Same words for each child in both languages
  concept <- rep(1:C, L)  # Each word is mapped to a unique concept, same concept for both languages
  lang <- rep(1:L, times = I * W)  # Language indicator (1 for L1, 2 for L2)
  # Assuming W = 200 words (100 words per language), and C = 100 concepts:

  # Simulate age and exposure data
  age <- runif(I, 12, 36)  # ages in months for each child
  age <- rep(age, each = W * L)  # replicate age for each observation per child

  exposure_L1 <- runif(I, 0.1, 0.9)  # exposure to L1 for each child
  exposure_L2 <- 1 - exposure_L1  # exposure to L2 is complementary
  exposure <- cbind(exposure_L1, exposure_L2)
  exposure <- exposure[rep(1:I, each = W * L), ]  # replicate exposure for each child
  
  # Generate true concept difficulty parameters:
  mu_theta_true <- 0.0
  sigma_theta_true <- 1.0
  theta_true <- rnorm(C, mu_theta_true, sigma_theta_true)  # concept difficulties
  
  sigma_delta_true <- 0.5
  delta_true <- rnorm(W, 0, sigma_delta_true)  # word-specific deviations
  
  alpha_true <- rnorm(I, 0, 1)  # child-specific random effects
  alpha_true <- rep(alpha_true, each = W * L)  # replicate for all observations
  
  # age and language exposure effects
  gamma_true <- age_effect 
  lambda_true <- c(1.0, 0.8)  # language exposure effects for L1 and L2

  # Generate probabilities and simulated responses
  eta <- numeric(N)
  for (n in 1:N) {
    w <- word[n]  # word index
    c <- concept[w]  # concept index
    l <- lang[n]  # language index
    beta <- theta_true[c] + delta_true[w]  # combined concept and word effect
    eta[n] <- alpha_true[n] + gamma_true * age[n] + lambda_true[l] * exposure[n, l] - beta  # linear predictor
  }
  prob <- plogis(eta)  # convert to probabilities using logistic function
  y <- rbinom(N, size = 1, prob = prob)  # binary responses (whether word is produced)

  tibble(child=child, age=age, lang=lang, exposure=exposure,
         word=word, N=N, I=I, W=W, C=C, L=L, y=y) # concept=concept,
}

dd <- generate_data()

get_stan_data <- function(df) {
  # Package data for Stan
  list(
    N = df$N,
    I = df$I,
    W = df$W,
    C = df$C,
    L = df$L,
    child = df$child,
    word = df$word,
    #concept = df$concept,
    lang = df$lang,
    age = df$age,
    exposure = df$exposure,
    y = df$y
  )
}