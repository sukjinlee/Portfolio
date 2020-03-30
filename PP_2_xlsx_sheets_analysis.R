install.packages('readxl')
install.packages('XML')
install.packages("rJava")
install.packages("memoise")


setwd("D://PP")

library(readxl)
library(dplyr)
library(XML)
library(tidyr)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_201")
library(xlsx)
#Sys.setlocale("LC_ALL", "korean")


#차량을 구성하는 필요 데이터 구분을 위한 xls 파일 불러오기
V_parts <- read_excel("./Fullconfigu/FILTER/V-Parts_List.xls")

#`TARGET NO` 의 결측치 제거 (Target이 없으면 차량구성에 사용X)
V_parts %>% 
  filter(!is.na(`TARGET NO`)) -> 
  V_parts_remove_na

#Needed Parts 가 있으면 따로 저장, 없으면 없다고 출력 (Needed Part가 있을 경우 Data요청 해야함)
if(any(!is.na(V_parts_remove_na$`NEEDED \nPART`))){
  V_parts_remove_na %>%
    filter(!is.na(`NEEDED \nPART`)) -> NeededParts
}else{
  print("Needed Parts 없음")
}

#분석에 필요한 xml 데이터 DataFrame 형식으로 불러오기  (데이터가 커서 좀 오래 걸림,,,)
PartList <- xmlToDataFrame("./Fullconfigu/XML/PartListwithENGRGOption.xml")

#Target_Number 중복 제거
unique(V_parts_remove_na$`TARGET NO`) -> Target_No

#차량 구성에 필요한 파트 및 컨디션만 구분
PartList %>% 
  filter(PART_NO %in% Target_No) %>% 
  select(contains("Part_NO"),contains("C_OPT")) ->
  PartList

#Option 이름 확인을 위해 옵션 코드와 이름이 있는 파일 불러오기
Option <- xmlToDataFrame('./Fullconfigu/XML/ENGRGOptionProfileList.xml')
Option_remaster <- select(Option,OPT_ID,OPT_VAL,OPT_DESC)

Option_remaster %>% 
  mutate(OPT = paste0(OPT_ID,OPT_VAL)) %>% 
  select(OPT,OPT_DESC) -> 
  Option_remaster
Option_remaster <- as_tibble(unique(Option_remaster))

  
  
#파트별로 많이 사용되는 옵션 뽑아서 옵션이름을 inner_join을 통해 붙여준 후 저장
for(i in Target_No){
  print(i)
  PartList %>% 
  filter(PART_NO == i) %>% 
  gather(key = PART_NO,na.rm = TRUE) %>% 
  count(value) -> Part_Option
  
  Part_Option_remaster <- inner_join(Part_Option, Option_remaster,by = c("value"="OPT"))
  
  write.table(i, "D://PP/Part_Option.txt", 
              sep = "\n", 
              row.names = FALSE, 
              quote = FALSE, 
              append = TRUE, 
              na = "NA"
  ) 
  
  write.table(Part_Option_remaster, "D://PP/Part_Option.txt", 
              sep = "\t", 
              row.names = FALSE, 
              quote = FALSE, 
              append = TRUE, 
              na = "NA"
  ) 
  
}





