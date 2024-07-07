library(tidyverse)

sample_submission <- read.csv('web_news_recommendation/sample_submission.csv')
view_log <- read.csv('web_news_recommendation/view_log.csv')
article_info <- read.csv('web_news_recommendation/article_info.csv')

# 10개 이상 본 사람 뽑기
population <- view_log %>%
  group_by(userID) %>%
  mutate(n = n()) %>%
  filter(n >= 10) %>% select(userID) %>% unique() %>% pull() # 749명

view_log_VSA = view_log %>%
  filter(userID %in% population) %>%
  mutate(id = row_number())

# 각자 5개 스크리닝
set.seed(123)

# 각 userID마다 5개씩 랜덤으로 선택하여 두 데이터프레임으로 나눔
validation_set <- view_log_VSA %>%
  group_by(userID) %>%
  sample_n(min(n(), 5)) %>%
  ungroup()

training_set <- view_log_VSA %>%
  filter(!id %in% validation_set$id) %>%
  select(-id)

validation_set <- validation_set %>% select(-id) %>%
  select(-c(userRegion, userCountry))


n = length(unique(article_info$articleID)) # 3008
m = length(unique(view_log_VSA$userID)) # 749

# hyper-parameter
lambda1 = 0.01
lambda2 = 0.01
init_sd1 = 0.2
init_sd2 = 0.2
k = 1000 # implicit elts count

set.seed(123)

# Initialize data : rand
lambda1_matrix = matrix(0, ncol = k, nrow = k)
for (i in 2:k){ # 1번 = Bias term
  lambda1_matrix[i, i] = lambda1
}
lambda2_matrix = matrix(0, ncol = k, nrow = k)
for (i in 2:k){
  lambda2_matrix[i, i] = lambda2
}

# Construct P
P_matrix = matrix(-1, nrow = m, ncol = n)
colnames(P_matrix) = unique(article_info$articleID) # V means Virtual article (Bias)
rownames(P_matrix) = unique(view_log_VSA$userID) # V means Virtual user (Bias)

for (i in 1:nrow(training_set)){
  if (P_matrix[training_set[i, 1], training_set[i, 2]] == -1){
    P_matrix[training_set[i, 1], training_set[i, 2]] = 0
  }
  else{
    P_matrix[training_set[i, 1], training_set[i, 2]] = 
      log(exp(P_matrix[training_set[i, 1], training_set[i, 2]]) + 1)
  }
}

U_matrix = matrix(rnorm(k * m, sd = init_sd1), nrow = k, ncol = m)
colnames(U_matrix) = unique(view_log_VSA$userID)

A_matrix = matrix(rnorm(k * n, sd = init_sd2), nrow = k, ncol = n) # k + 1 : add language info
colnames(A_matrix) = unique(article_info$articleID)

# 실험 1 : 강한 가정 cij = 1, pij = 1 (관측 시), pij = 0 (미관측 시)
# 실험 2: 여러 번 본 횟수만큼 pij 계수 부여.
# 실험 3: c_ij 도입 - 어러 번 본 경우 confidence 증가시켜주기

i = 1
while (i <= 10){
  # user_i gradient
  new_U_matrix = solve(lambda2_matrix + A_matrix %*% t(A_matrix)) %*% A_matrix %*% t(P_matrix)
  print(paste0(i, "th train - U norm diff: ", norm(new_U_matrix - U_matrix, type = "F")))
  U_matrix = new_U_matrix
  
  # article_j gradient
  new_A_matrix = solve(lambda1_matrix + U_matrix %*% t(U_matrix)) %*% U_matrix %*% P_matrix
  print(paste0(i, "th train - A norm diff: ", norm(new_A_matrix - A_matrix, type = "F")))
  A_matrix = new_A_matrix
  
  i = i + 1
}

# 결과 만들기
P_hat_matrix = t(U_matrix) %*% A_matrix

get_top5_colnames <- function(row) {
  top_indices <- order(row, decreasing = TRUE)[1:5]
  return(colnames(P_hat_matrix)[top_indices])
}

top5_colnames_list <- apply(P_hat_matrix, 1, get_top5_colnames)

top5_colnames_P <- as.data.frame(t(top5_colnames_list))
rownames(top5_colnames_P) <- rownames(P_hat_matrix)
colnames(top5_colnames_P) <- paste0("Top", 1:5)

top5_colnames_P <- top5_colnames_P %>%
  mutate(userID = rownames(top5_colnames_P))

top5_long <- top5_colnames_P %>%
  pivot_longer(cols = starts_with("Top"),
               names_to = "Rank",
               values_to = "articleID")

top5_long <- top5_long %>%
  select(-Rank) %>%
  mutate(V = 'V')

validation_set %>%
  left_join(top5_long) %>%
  summarize(rate = sum(!is.na(V)) / n()) %>% pull()
