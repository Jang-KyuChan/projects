# metric: rmse
library(tidyverse)
data <- read.csv('open/data.csv')
data

data <- data %>%
  mutate(numeric_id = as.integer(substr(id, 8, 12)))

sample_submission <- read.csv('open/sample_submission.csv') # after interpolation

# 1. spline function
y <- data$Value
x <- data$numeric_id

# 스플라인 보간을 위한 데이터 준비
complete_x <- x[!is.na(y)]
complete_y <- y[!is.na(y)]

# 3차 스플라인 보간 수행
spline_result <- spline(complete_x, complete_y, xout = x)

# 보간된 데이터 생성
y_spline <- y
y_spline[is.na(y)] <- spline_result$y[is.na(y)]

# 보간된 데이터 출력
print("스플라인 보간된 데이터:")
print(data.frame(x, y_spline))

# 시각화를 통해 결과 확인
plot(x, y, main = "Cubic Spline Interpolation of Missing Values", xlab = "X", ylab = "Y", pch = 16, col = "red")
lines(x, y_spline, col = "blue")
legend("topright", legend = c("Original Data", "Cubic Spline Interpolated Data"), col = c("red", "blue"), pch = c(16, -1), lty = c(0, 1))

# 2. Smooth Spline
# 스무딩 스플라인 보간 수행

# 스무딩 스플라인 보간 수행
smooth_spline_result <- smooth.spline(complete_x, complete_y, all.knots = TRUE, keep.data = TRUE, 
                                      penalty = 1, df.offset = 0, spar = 0.6)

# submission 1: spar NA
# submission 2: spar = 0.6

# 보간된 데이터 생성
y_smooth_spline <- predict(smooth_spline_result, x)$y

# 보간된 데이터 출력
print(data.frame(x, y_smooth_spline))

table = data.frame(x, y_smooth_spline) %>%
  cbind(real_value = data$Value)

table %>%
  ggplot()+
  geom_point(aes(x=x, y=real_value), color = 'red') +
  geom_line(aes(x=x, y=y_smooth_spline), color = 'blue')

data.frame(id = data$id, Value = y, smooth_spline = y_smooth_spline) %>%
  head()

# 검증
data.frame(id = data$id, Value = y, smooth_spline = y_smooth_spline) %>%
  mutate(diff = abs(Value - smooth_spline)) %>%
  arrange(-diff) %>%
  head()


# 출력물 제작
result2 <- data.frame(id = data$id, Value = ifelse(!is.na(y), y, y_smooth_spline))

write.csv(result2, 'result2.csv', row.names = FALSE)



