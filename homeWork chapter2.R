
# 문제 2 조건

name <- c("최민수", "유관순", "이순신", "김유신", "홍길동")
age <- c(55, 45, 45, 53, 15)
gender <- c(1,2,1,1,1)
job <- c("연예인", "주부", "군인", "직장인", "학생")
sat ,- c(3,4,2,5,5)
grade <- c("C", "C", "A", "D", "A")
total <- c(44.4, 28.5, 43.5, NA, 27.1)


# 문제 2-1 풀이
user <- data.frame(Name = name, Age = age, Gender = gender, Job = job, Sat = sat, Grade = grade, Total = total)
user 


# 문제 2-2 풀이
hist(user$Gender)

# 문제 2-3 풀이
user
user2 <- user[seq(from=0,to=nrow(user),by=2),]
user2




# 문제 3 조건

kor <- c(90, 85, 90)
eng <- c(70, 85, 75)
mat <- c(86, 92, 88)

# 문제 3-1 풀이

subjectGrade <- data.frame(kor, eng, mat)
subjectGrade

# 문제 3-2 풀이

apply(subjectGrade, 1, max)
apply(subjectGrade, 2, max)


# 문제 3-3 풀이
round(apply(subjectGrade, 1, mean), 2)
round(apply(subjectGrade, 2, mean), 2)

# 문제 3-4 풀이
apply(subjectGrade, 1, var)
apply(subjectGrade, 1, sd)


# 문제 4
Data2 <- c("2017-02-05 수입3000원",
           "2017-02-06 수입4500원",
           "2017-02-07 수입2500원")
library(stringr)


# 문제 4-1
unlist(str_extract_all(Data2, "[0-9]{4}[가-힣]"))


# 문제 4-2
str_replace_all(Data2,"[0-9]", "")

# 문제 4-3
str_replace_all(Data2,"-","/")

# 문제 4-4
paste(Data2, collapse=",")
                