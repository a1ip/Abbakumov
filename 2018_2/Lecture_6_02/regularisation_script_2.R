#  Регуляризация    Версия 3 (15.10.2018)


#  Дополнительно теория
#  1.  Новые критерии качества
#      вместо Гаусс - Лаплас - Фишер
#  2   Не надо думать об отбрасывании неинформативных переменных
#  3   Elastic Net
#  4   L1 =>  bi = 0


#  Аппроксимация точек многочленом

#  Шаг 0. Создаем данные


x<- seq(0, 6, 0.5)

set.seed(1234)
y <- sin(x)+ rnorm(length(x), 0, 0.8)


plot(x, y, type="p", col="red")


#  Шаг 1 
#   Многочлен 10 степени  Готовим данные

#  степень полинома
polinom.power <- 10

data.train <- data.frame(x0=rep(1, length(x)), 
                         x1=x, x2= x^2, x3= x^3, x4= x^4, x5= x^5, 
                         x6=x^6, x7= x^7, x8=x^8, x9= x^9,x10=x^10, y=y)

x.test <- seq(min(x), max(x), 0.01)
data.test <- data.frame(x0=rep(1, length(x.test)), 
                        x1=x.test, x2= x.test^2, x3= x.test^3, x4= x.test^4, 
                        x5= x.test^5,  x6=x.test^6, x7= x.test^7, x8=x.test^8, 
                        x9= x.test^9,x10=x.test^10)


#  Шаг 2. Используем процедуру lm

res.lm <- lm(y~.-x0 , data= data.train)

y.pred <- predict.lm(res.lm, data.test)

plot(x, y, type="p", col="red")
lines(x.test, y.pred, col = "blue")


#  Шаг 3. Прямыми вычислениями, не привлекая процедуру lm

#  В чем ошибка?
#  beta.1 <- solve(t(data.2[ , -ncol(data.2)]) %*% data.2[ , -ncol(data.2)]) %*% 
#    t(data.2[ , -ncol(data.2)]) %*% data.2[ , ncol(data.2)]

#  str(data.2)

#  Преобразуем таблицы в матрицы
#  удаляем столбец y

x.1 <- as.matrix(data.train[ , -ncol(data.train)])

x.3 <- as.matrix(data.test)


#  оценки параметров модели
beta.1 <- solve(t(x.1) %*% x.1) %*% t(x.1) %*% y

y.pred.2 <- x.3 %*% beta.1

#  plot(x, y, type="p", col="red")
#  lines(x.test, y.pred.2, col = "blue")

#  максимальное отличие между решениями, полученными на шагах 1 и 2?
max(abs(y.pred.2 - y.pred))


#######################1

# Шаг 4
#  вариант 2 (плохо обусловленная матрица данных)
#  степень полинома  уменьшаем до 7

polinom.power <- 7

data.train.7 <- data.frame(x0=rep(1, length(x)), 
                         x1=x, x2= x^2, x3= x^3, x4= x^4, x5= x^5, 
                         x6=x^6, x7= x^7, y=y)

x.test <- seq(min(x), max(x), 0.01)
data.test.7 <- data.frame(x0=rep(1, length(x.test)), 
                        x1=x.test, x2= x.test^2, x3= x.test^3, x4= x.test^4, 
                        x5= x.test^5,  x6=x.test^6, x7= x.test^7)


#  Шаг 5. Используем процедуру lm

res.lm <- lm(y~.-x0 , data= data.train.7)

y.pred.71 <- predict.lm(res.lm, data.test.7)

plot(x, y, type="p", col="red")
lines(x.test, y.pred, col = "blue")


#  Шаг 6. Прямыми вычислениями, не привлекая процедуру lm

#  В чем ошибка?
#  beta.1 <- solve(t(data.2[ , -ncol(data.2)]) %*% data.2[ , -ncol(data.2)]) %*% 
#    t(data.2[ , -ncol(data.2)]) %*% data.2[ , ncol(data.2)]

#  str(data.2)

#  Преобразуем таблицы в матрицы
#  удаляем столбец y

x.10 <- as.matrix(data.train.7[ , -ncol(data.train.7)])

x.30 <- as.matrix(data.test.7)


#  оценки параметров модели
beta.17 <- solve(t(x.10) %*% x.10) %*% t(x.10) %*% y

y.pred.72 <- x.30 %*% beta.17

#  plot(x, y, type="p", col="red")
#  lines(x.test, y.pred.2, col = "blue")

#  максимальное отличие между решениями, полученными на шагах 1 и 2?
max(abs(y.pred.71 - y.pred.72))




#######################1



#  Шаг 7. Снова приближаем многочленом 10 степени.
#  На этот раз с регуляризацией


#  удаляем столбец y
x.1 <- as.matrix(data.train[ , -ncol(data.train)])

x.3 <- as.matrix(data.test)




lambda <- 9

#  не учитываем столбец с y
num.parameters <- ncol(data.train)-1

reg.1 <- lambda * diag(num.parameters)
reg.1[1,1] <- 0


beta.2 <- solve(t(x.1) %*% x.1 + reg.1) %*% t(x.1) %*% y


y.pred.3 <- x.3 %*% beta.2

plot(x, y, type="p", col="red")
lines(x.test, y.pred.3, col = "blue")









