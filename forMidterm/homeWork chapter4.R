#4. 백터 EMP는 입사연도 이름 급여 순으로 사원정보가 기록된 데이터이다. 
#백터 EMP를 티용하여 다음과 같은 출력결과가 나타나도록 함수를 정의하시오

EMP = c("2015홍길동220", "2002이순신300", "2010유관순260")

library(stringr)

emp_pay = function(x) {
  x = str_replace(EMP, "[0-9]{4}", "")
  mean = mean(as.numeric(str_extract(x, "[0-9]{3}")))
  cat("전체 급여 평균 : ", mean, '\n', '평균 이상 급여 수령자', '\n', sep="")
  for (i in x) {
    if (str_extract(i, "[0-9]{3}")>=mean){
      cat(str_extract(i, "[가-힣]{3}"), " => ", str_extract(i, "[0-9]{3}"),'\n')
    }    
  }
}




emp_pay(EMP)
