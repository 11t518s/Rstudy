# 6장 dplyr 패키지와 iris 데이터셋을 사용한다.
install.packages("dplyr")
library(dplyr)
data(iris)
iris


# 6-1 iris의 Petal.Lenght칼럼을 대상으로 1.5 이상의 값만 필터링하시오
filter(iris, Petal.Length>=1.5)

  # 파이프 연산자 활용
  iris %>% filter(., Petal.Length>=1.5)


# 6-2 1의 결과에서 1,3,5번 칼럼을 선택하시오
filterdIris = filter(iris, Petal.Length>=1.5)
select(filterdIris, c(1,3,5))
  
  # 파이프 연산자 활용
  iris %>% filter(Petal.Length>=1.5) %>% select(c(1,3,5))

  
# 6-3 2의 결과에서 1~3번 칼럼의 값을 뺸 diff 파생 변수를 만들르고, 앞 부분 6개만 출력하시오
SelectedAndfilterdIris = select(filterdIris, c(1,3,5))
SelectedAndFilterdIrisIncludediffColumn = mutate(SelectedAndfilterdIris, diff=filterdIris[1]-filterdIris[3])
head(SelectedAndFilterdIrisIncludediffColumn, 6)

  # 파이프 연산자 활용
  iris %>% filter(Petal.Length>=1.5) %>% select(c(1,3,5)) %>% mutate(diff= Sepal.Length-Petal.Length) %>% head(6)


  # 6-4 3의 결과에서 꽃 종(Species)별로 그룹화하여 Sepal.Length와 Petal.Length 변수의 평균을 계산하시오
groupedSelectedAndFilterdIrisIncludediffColumn = group_by(SelectedAndFilterdIrisIncludediffColumn, Species)
summarise(groupedSelectedAndFilterdIrisIncludediffColumn, meanOfSepalLength=mean(Sepal.Length), meanOfPatalLength=mean(Petal.Length))

  # 파이프 연산자 활용
  iris %>% filter(Petal.Length>=1.5) %>% select(c(1,3,5)) %>% mutate(diff= Sepal.Length-Petal.Length) %>% group_by(Species) %>% summarise(meanOfSepalLength=mean(Sepal.Length), meanOfPatalLength=mean(Petal.Length))


  

# 6-5 reshape2 패키지를 이용하여 단계별로 iris 데이터 셋을 처리하시오
# 6-5-1 꽃의 종류(Species)를 기준으로 '넓은 형식'을 '긴 형식'으로 변경하기

longIris = melt(iris, id="Species")
longIris
  
  # 파이프 연산자 활용
  iris %>% melt(id="Species")
 
   
# 6-5-2 꽃의 종류별로 나머지 4가지 변수의 합계 구하기
sumofIrisbySpecies = dcast(longIris, Species ~ variable, value.var = "value", sum)
sumofIrisbySpecies

  # 파이프 연산자 활용
  iris %>% melt(id="Species") %>% dcast(Species ~ variable, value.var = "value", sum)
