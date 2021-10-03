# 2 R에서 제공하는 CO2 데이터 셋을 대상으로 파일을 저장하시오

Co2Data <- CO2

# Treatment 칼럼 값이 nonchiled인 경우 CO2_df1.csv파일로 행번호를 제외하고 저장한다.

CO2_df1 = subset(Co2Data, Treatment=="nonchilled")
write.csv(CO2_df1,"CO2_df1.csv", row.names=F, quote=F)

# Treatment 칼럼 값이 chiled인 경우 CO2_df2.csv파일로 행번호를 제외하고 저장한다.

CO2_df2 = subset(Co2Data, Treatment=="chilled")
write.csv(CO2_df1,"CO2_df2.csv", row.names=F, quote=F)
