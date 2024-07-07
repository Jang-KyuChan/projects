library(tidyverse)

sample_submission <- read.csv('web_news_recommendation/sample_submission.csv')
view_log <- read.csv('web_news_recommendation/view_log.csv')
article_info <- read.csv('web_news_recommendation/article_info.csv')

sample_submission %>% str()
view_log %>% str()
article_info %>% str()

article_info %>% head() %>% select(-Content)

# Goal : To find appropriate news for a user, we make item-user priority table and fitted with ALS.

# article_info의 userID는 createrID임. 혼동 주의!
# pt = 포르투갈어

# Item-User Sparse Matrix 제작 (Binary)
n = length(unique(article_info$articleID)) # 3008
m = length(unique(view_log$userID)) # 1415

lambda1 = 0.1
lambda2 = 0.1
k = 315 # implicit elts count

set.seed(123)

# Initialize data : rand
lambda1_matrix = matrix(0, ncol = k, nrow = k)
for (i in 1:k){
  lambda1_matrix[i, i] = lambda1
}
lambda2_matrix = matrix(0, ncol = k, nrow = k)
for (i in 1:k){
  lambda2_matrix[i, i] = lambda2
}

# Construct P
P_matrix = matrix(0, nrow = m, ncol = n)
colnames(P_matrix) = unique(article_info$articleID)
rownames(P_matrix) = unique(view_log$userID)

for (i in 1:nrow(view_log)){
  P_matrix[view_log[i, 1], view_log[i, 2]] = 1
}

U_matrix = matrix(rnorm(k * m, sd = 0.1), nrow = k, ncol = m)
colnames(U_matrix) = unique(view_log$userID)
A_matrix = matrix(rnorm(k * n, sd = 0.1), nrow = k, ncol = n)
colnames(A_matrix) = unique(article_info$articleID)

# 실험 1 : 강한 가정 cij = 1, pij = 1 (관측 시), pij = 0 (미관측 시)
i = 1
while (i <= 1000){
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

P_hat_matrix = t(U_matrix) %*% A_matrix

# 각 행마다 상위 5개의 열의 이름을 가져오는 함수 정의
get_top5_colnames <- function(row) {
  top_indices <- order(row, decreasing = TRUE)[1:5]
  return(colnames(P_hat_matrix)[top_indices])
}

# apply 함수를 사용하여 각 행에 대해 상위 5개의 열 이름 추출
top5_colnames_list <- apply(P_hat_matrix, 1, get_top5_colnames)

# 결과를 데이터 프레임으로 변환
top5_colnames_P <- as.data.frame(t(top5_colnames_list))
rownames(top5_colnames_P) <- rownames(P_hat_matrix)
colnames(top5_colnames_P) <- paste0("Top", 1:5)

# 결과 출력
# 사용자 ID 추가
top5_colnames_P <- top5_colnames_P %>%
  mutate(userID = rownames(top5_colnames_P))

top5_long <- top5_colnames_P %>%
  pivot_longer(cols = starts_with("Top"),
               names_to = "Rank",
               values_to = "articleID")

top5_long <- top5_long %>%
  select(-Rank)

write.csv(top5_long, "web_news_recommendation/result/logP_bias_model_315.csv", row.names = FALSE)


