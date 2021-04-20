
install.packages("gmodels")
install.packages("sqldf")
library(gmodels)
library(sqldf)


setwd('C:/Users/KimSungWoon/Desktop/Data')

claim<-read.csv("BGCON_CLAIM_DATA.csv", header=TRUE)
cntt <-read.csv("BGCON_CNTT_DATA.csv", header=TRUE)
cust <-read.csv("BGCON_CUST_DATA.csv", header=TRUE)
fpinfo<-read.csv("BGCON_FPINFO_DATA.csv", header=TRUE)
family<-read.csv("BGCON_FMLY_DATA.csv", header=TRUE)
###############################################################3

ctab(fpinfo$, cust0)
ttest(csut0$MAIN_INSR_AMT, cust0)


# Data Explore
cust0 <- cust[!cust$SIU_CUST_YN=="",] 
xtest <- cust[cust$SIU_CUST_YN=="",] # cust0 를 만들어 트레이닝 해서 맞춰본후 X테스트로 맞추면 맞을것이다
cust0$FRAUD<-ifelse(cust0$SIU_CUST_YN=="Y",1,0)
cust0$FRAUD<-as.factor(cust0$FRAUD)

ctab<-function(name, dsn) {
  CrossTable(x = name, y = dsn$SIU_CUST_YN, prop.t=FALSE, expected=TRUE, chisq =TRUE)
}    #카이스퀘어 함수
ttest<-function(name, dsn) {
  t.test(name~dsn$SIU_CUST_YN)
}  
#######################cust data 분석과 점수화################################


score1<- function(dsn){
  dsn$RESI_TYPE_SCORE <- 0
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==11,-2.53,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==12,5.4,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==13,2.8,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==20,-13,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==30,8.6,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE <- ifelse(dsn$RESI_TYPE_CODE==40,7.9,dsn$RESI_TYPE_SCORE)
  
  
  dsn$FP_CAREER_SCORE <- 0
  dsn$FP_CAREER_SCORE <- ifelse(dsn$FP_CAREER=="Y",62, dsn$FP_CAREER_SCORE)
  dsn$FP_CAREER_SCORE <- ifelse(dsn$FP_CAREER=="N",-4 ,dsn$FP_CAREER_SCORE)
  
  #직업 점수화
  dsn$OCCP_GRP_SCORE <-0
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="1차산업 종사자",-9 ,dsn$OCCP_GRP_SCORE) #-8.61
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="2차산업 종사자",-39,dsn$OCCP_GRP_SCORE) #-38.8
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="3차산업 종사자",49 ,dsn$OCCP_GRP_SCORE)#48.7
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="공무원",        -5 ,dsn$OCCP_GRP_SCORE) #-5.2
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="교사",         -11 ,dsn$OCCP_GRP_SCORE) #-11
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="교육관련직",    -2 ,dsn$OCCP_GRP_SCORE) #-1.9
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="기타",		      -3 ,dsn$OCCP_GRP_SCORE) #-2.9
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="사무직",		  -0.1 ,dsn$OCCP_GRP_SCORE)#-0.13
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="운전직",		     -3,dsn$OCCP_GRP_SCORE) #-2.8
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="의료직 종사자",  -4,dsn$OCCP_GRP_SCORE) #-3.9
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="자영업", 		     46,dsn$OCCP_GRP_SCORE)#46
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="전문직", 		  -4.2 ,dsn$OCCP_GRP_SCORE) #-4.2
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="종교인/역술인",   3,dsn$OCCP_GRP_SCORE)#3.1
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="주부",  		     33,dsn$OCCP_GRP_SCORE)#33
  dsn$OCCP_GRP_SCORE <- ifelse(dsn$OCCP_GRP_2=="학생", 		      -32,dsn$OCCP_GRP_SCORE) #-32
  
  #지역점수화
  dsn$CTPR_SCORE <- 0
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="강원", 17 , dsn$CTPR_SCORE)#17
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="경기", -13, dsn$CTPR_SCORE)#-13
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="경남", -9 , dsn$CTPR_SCORE)#-9.1
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="경북", -14, dsn$CTPR_SCORE)#-14
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="광주", 106, dsn$CTPR_SCORE)#106
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="대전", -0.017, dsn$CTPR_SCORE)
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="대구", -7 , dsn$CTPR_SCORE)#-7
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="부산", 48 , dsn$CTPR_SCORE)#48
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="서울", 0.3, dsn$CTPR_SCORE) #0.3
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="세종", -0.5,dsn$CTPR_SCORE)#-25
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="인천", 35 , dsn$CTPR_SCORE)#35
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="전남", 1  , dsn$CTPR_SCORE)#1.4
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="전북", 7  , dsn$CTPR_SCORE)#7.3
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="제주", -15.249, dsn$CTPR_SCORE)
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="충남", -7.7  , dsn$CTPR_SCORE)#-7.7
  dsn$CTPR_SCORE <- ifelse(dsn$CTPR=="충북", -24.5 , dsn$CTPR_SCORE)#-24.5
  
  
  #자녀수
  dsn$CHLD_CNT_SCORE <- 0
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==0, -10.6, dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==1, 14.2 , dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==2, 0.1  , dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==3, 5.8  , dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==4, 26.7 , dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==5, -0.3 , dsn$CHLD_CNT_SCORE)
  dsn$CHLD_CNT_SCORE <- ifelse(dsn$CHLD_CNT==6, 9.4  , dsn$CHLD_CNT_SCORE)

  #FP 경력
  dsn$FP_CAREER_SCORE<- 0
  dsn$FP_CAREER_SCORE<- ifelse(dsn$FP_CAREER==0,-3.5,dsn$FP_CAREER_SCORE)
  dsn$FP_CAREER_SCORE<- ifelse(dsn$FP_CAREER==1,+61 ,dsn$FP_CAREER_SCORE)
  return(dsn)
}
cust0 <- score1(cust0)
xtest <- score1(cust0)

######################################################################





#####CNTT DATA CUST0 에 merge 함#########################################################

#CNTT 증권번호 그 증번번호 옆에 CUST 아이디
#noinsu: 인당 보험증권수
#CNTT(계약정보)의 key는 증권번호

temp1 <- sqldf("select CUST_ID, count(*) as noInsu,
               AVG(MAIN_INSR_AMT) as MAIN_INSR_AMT,
               AVG(SUM_ORIG_PREM) as SUM_ORIG_PREM
               from cntt group by CUST_ID order by CUST_ID")
cust0 <- merge(cust0, temp1, by ="CUST_ID")
xtest <- merge(xtest, temp1, by ="CUST_ID") # Cust0에서 같은 방식으로 변수 변환을 해줘야지 예측 가능함


cntt1 <- merge(cust0, cntt , by ="CUST_ID")

cntt2 <-merge (cntt1, fpinfo, by ="CLLT_FP_PRNO")
##############################################################################################

ctab(cntt2$CUST_ROLE, cntt1)



##CNTT DATA 분석과 점수화##############################################################################

ctab(cntt1$CUST_ROLE, cntt1)
#유의
ctab(cntt1$GOOD_CLSF_CDNM, cntt1)
#상품분류re
# 교육(2.1) 변액CI(-4.7) 변액연금(-11.1) 변액저축(0.013) 변액종신(-1.3)
# 보장(6.8) 실손(-13) 암(-6.1) 어린이(-9.3) 어린이연금(-4.7)
#어린이연금_변액(-3.6) 어린이저축(4.1) 어린이저축_변액(0.3)
#일반CI(-1.03) 일반연금 (3.8) 일반저축(19) 일반종신(0.4) 정기(41)

ctab(cntt1$SALE_CHNL_CODE, cntt1)
#판매채널 유의
# 1(0.3) 2(-175) 3(30) 4(93.5) 5(-10.6) 6(-4.7) 7(148.3)

ctab(cntt1$PAYM_CYCL_CODE, cntt1) 
#납입주기코드
# 0(-15.5) 1(15.5) 3(85.5) 6(-0.05) 12(-286.7)
ttest(cntt1$MAIN_INSR_AMT, cntt1)
#주 보험   30142618 N    33964701  Y
ttest(cntt1$SUM_ORIG_PREM, cntt1)
#합계보험료   558591 N   404624 Y 낮을 수록 사기 가능성 높아
ttest(cust0$noclaim, cust0)


#####################################

cntt$CUST_ROLE_SCORE <- 0
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==0, 78.5 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==1, 20.5 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==2, -74 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==3, 17 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==4, 44 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==5,-50 , cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE <- ifelse(cntt$CUST_ROLE==21,-72.7 , cntt$CUST_ROLE_SCORE)

#상품코드
cntt$CUST_GOOD_SCORE <- 0
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='교육'    , 2.1  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='변액CI'  ,-4.7  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='변액연금',-11.1  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='변액저축',0.013 ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='변액종신',-1.33 ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='보장',6.8,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='실손', -13   ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='암', -6.1  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='어린이', -9.3   ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='어린이연금', -4.7,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='어린이연금_변액',-3.6    ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='어린이저축',4.1  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='어린이저축_변액',0.3  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='일반CI' ,  -1.3  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='일반연금', 3.8  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='일반저축', 19   ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='일반종신', 0.4  ,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE <- ifelse(cntt$GOOD_CLSF_CDNM=='정기',  41   ,cntt$CUST_GOOD_SCORE)

#판매채널
cntt$SALE_CHNL_SCORE <- 0
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==1    ,  0.3    ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==2    , -175   ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==3    , 30     ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==4    , 93.5   ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==5    ,-10.6   ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==6    ,-4.7    ,cntt$SALE_CHNL_SCORE)
cntt$SALE_CHNL_SCORE <- ifelse(cntt$SALE_CHNL_CODE==7    , 148.3  ,cntt$SALE_CHNL_SCORE)

#납입주기
cntt$PAYM_CYCL_SCORE <- 0
cntt$PAYM_CYCL_SCORE <- ifelse(cntt$PAYM_CYCL_CODE==0    , -13.5  ,cntt$PAYM_CYCL_SCORE)
cntt$PAYM_CYCL_SCORE <- ifelse(cntt$PAYM_CYCL_CODE==1    , 15.5   ,cntt$PAYM_CYCL_SCORE)
cntt$PAYM_CYCL_SCORE <- ifelse(cntt$PAYM_CYCL_CODE==3    , 85.5   ,cntt$PAYM_CYCL_SCORE)
cntt$PAYM_CYCL_SCORE <- ifelse(cntt$PAYM_CYCL_CODE==6    , -0.05  ,cntt$PAYM_CYCL_SCORE)
cntt$PAYM_CYCL_SCORE <- ifelse(cntt$PAYM_CYCL_CODE==12   , -286.7 ,cntt$PAYM_CYCL_SCORE)

#점수화한것을 평균내 cust0에 merge 시킨다.
temp2<- sqldf("select CUST_ID ,count(*) as nocntt,
              AVG(CUST_ROLE_SCORE) as CUST_ROLE_SCORE,
              AVG(CUST_GOOD_SCORE) as CUST_GOOD_SCORE,
              AVG(SALE_CHNL_SCORE) as SALE_CHNL_SCORE,
              AVG(PAYM_CYCL_CODE) as PAYM_CYCL_SCORE
              from cntt group by CUST_ID order by CUST_ID")

cust0 <- merge(cust0,temp2, BY="CUST_ID")
xtest <- merge(xtest,temp2, BY="CUST_ID")


#####################claim########################
claim <- merge(cust0,claim,by ="CUST_ID")

ctab(claim1$ACCI_DVSN, claim1)  
#ACCI_DVSN		사고구분 : 1이면 9.7% 2이면 10.7% 3이면 8.6%
# 1:(-85.6) 2:(18.351) 3:(21.4) 

ctab(claim1$CHANG_FP_YN, claim1)  
#FP 변경여부 청구FP와 수금 FP다를경우  5%가 높음

ctab(claim1$DMND_RESN_CODE, claim1)
#DMND_RESN_CODE	청구사유코드 1-3% 2-15% 3-3.4% 4-27% 5-5.1% 6-5.6%
# 1(-61.4) 2(3013.3 ) 3(-2045.78) 4(45.5  ) 5(-751.54 ) 6(-672.1 ) 7(-27) 9(-1)

ctab(claim1$DMND_RSCD_SQNO, claim1) 
#DMND_RSCD_SQNO 청구사유코드일련번호 1- 9.1% 2- 9.4% 3- 7.3%
#1( 5.77 ) 2(-1.5  ) 3(-22.41) 4(-22.24) 5(-13.5 ) 6(-8.9) 7(-4.1 ) 8(-2.5) 9(-3.1) 10 (-0.13) 11(-0.1) 12(-1) 12(-0.2) 13(-0.2) 14(-0.2) 15(-0.4)

ttest(claim1$VLID_HOSP_OTDA, claim1) 
# VLID_HOSP_OTDA	유효입원/통원일수 사기일 경우 평균 14.9일 아닐경우 6.9일

ttest(claim1$HOUSE_HOSP_DIST,claim1)
#병원과 거리

ctab(claim1$HOSP_SPEC_DVSN, claim1)
#HOSP_SPEC_DVSN	병원종별구분
#10(-1823) 20(180) 25(417) 30(25) 40(-30) 45(-81) 60(-1) 70(-2) 80 (2403) 85(211) 90(-4) 95(1027)

ttest(claim1$NON_PAY,claim1)
#비급여 가 많을 경우 보험 사기 확률이 올라간다 285903(X) 541864(O)

ttest(claim1$NON_PAY_RATIO,claim1)
#실손비급여비율 10.17%(X) 2.24(O)

ttest(claim1$PATT_CHRG_TOTA,claim1)
#환자부담총액 415552(X) 683607(o)

ctab(claim1$PMMI_DLNG_YN, claim1)
#실손 처리 하지 않을경우 20% 차이남


#비급여 /비급여비율 
claim$ACCI_DVSN_SCORE <- 0
claim$ACCI_DVSN_SCORE <- ifelse(claim$ACCI_DVSN==1,-85.6 ,claim$ACCI_DVSN_SCORE)
claim$ACCI_DVSN_SCORE <- ifelse(claim$ACCI_DVSN==2, 18.4 ,claim$ACCI_DVSN_SCORE)
claim$ACCI_DVSN_SCORE <- ifelse(claim$ACCI_DVSN==3, 21.4 ,claim$ACCI_DVSN_SCORE)

#FP 변경 여부
#CHANG_FP_YN N+96 Y-142
claim$CHANG_FP_YN_SCORE <- 0 
claim$CHANG_FP_YN_SCORE <- ifelse(claim$CHANG_FP_YN=="N",+96 ,claim$CHANG_FP_YN_SCORE)
claim$CHANG_FP_YN_SCORE <- ifelse(claim$CHANG_FP_YN=="Y",-142,claim$CHANG_FP_YN_SCORE)

#실손 처리 여부
#PMMI_DLNG_YN N+437 Y -2291
claim$PMMI_DLNG_YN_SCORE <- 0 
claim$PMMI_DLNG_YN_SCORE <- ifelse(claim$PMMI_DLNG_YN=="N",+437,claim$PMMI_DLNG_YN_SCORE)
claim$PMMI_DLNG_YN_SCORE <- ifelse(claim$PMMI_DLNG_YN=="Y",-2291,claim$PMMI_DLNG_YN_SCORE)


#청구사유
claim$DMND_RESN_CODE_SCORE <- 0
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==1,-61.4 ,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==2,3013 ,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==3,-2045,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==4,45.5,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==5,-751.5,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==6,-672.1,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==7,-27   ,claim$DMND_RESN_CODE_SCORE)
claim$DMND_RESN_CODE_SCORE <- ifelse(claim$DMND_RESN_CODE==9,-1    ,claim$DMND_RESN_CODE_SCORE)


#청구사유 일련번
claim$DMND_RSCD_SQNO_SCORE <- 0
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==1,-5.77,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==2,-1.5,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==3,-22.4 ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==4,-22.24,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==5,-13.5  ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==6,-8.9   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==7,-4.1   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==8,-2.5   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==9,-3.1   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==10,-0.13  ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==11,-0.1   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==12,-1     ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==13,-0.2   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==14,-0.2   ,claim$DMND_RSCD_SQNO_SCORE)
claim$DMND_RSCD_SQNO_SCORE <- ifelse(claim$DMND_RSCD_SQNO==15,-0.4   ,claim$DMND_RSCD_SQNO_SCORE)


#병원종별
claim$HOSP_SPEC_DVSN_SCORE <- 0
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==10,-1823  ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==20, 180   ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==25, 147   ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==30, 25    ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==40,-30    ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==45,-81    ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==60,-1     ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==70,-2     ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==80,2043   ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==85, 211   ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==90, -4    ,claim$HOSP_SPEC_DVSN_SCORE)
claim$HOSP_SPEC_DVSN_SCORE <- ifelse(claim$HOSP_SPEC_DVSN==95, 1027  ,claim$HOSP_SPEC_DVSN_SCORE)


#dsn$VLID_HOSP_OTDA_SCORE <- ifelse(dsn$VLID_HOSP_OTDA<=13,0,1)


temp3 <- sqldf("select CUST_ID ,count(*) as noclaim,
               AVG(ACCI_DVSN_SCORE) as ACCI_DVSN_SCORE,
               AVG(DMND_RESN_CODE_SCORE) as DMND_RESN_CODE_SCORE,
               AVG(DMND_RSCD_SQNO_SCORE) as DMND_RSCD_SQNO_SCORE,
               AVG(CHANG_FP_YN_SCORE) as CHANG_FP_YN_SCORE,
               AVG(PMMI_DLNG_YN_SCORE) as PMMI_DLNG_YN_SCORE,
               AVG(HOSP_SPEC_DVSN_SCORE) as HOSP_SPEC_DVSN_SCORE
               from claim group by CUST_ID order by CUST_ID")
cust0 <-merge(cust0,temp3,by ="CUST_ID")
xtest <-merge(xtest,temp3,by ="CUST_ID")


#납입총 보험료 결측치 처리
cust0$TOTALPREM[is.na(cust0$TOTALPREM)] <- 30102163
 # mean(cust0$TOTALPREM[!is.na(cust0$TOTALPREM)])


cust0$TOTAL_CT_SCORE <- ((cust0$MAIN_INSR_AMT-(cust0$SUM_ORIG_PREM*cust0$noInsu))/cust0$TOTALPREM)*cust0$noclaim

####################################3

#결측치 처리 평균으로 대체
ctab(cust0$PAYM_CYCL_SCORE, cust0)

mean(cust0$TOTAL_CT_SCOR)
ttest(cust0$noInsu, cust0)


cust0$RESI_TYPE_SCORE[is.na(cust0$RESI_TYPE_SCORE)]<- -13
cust0$CHLD_CNT_SCORE[is.na(cust0$CHLD_CNT_SCORE)]<- -11
cust0$PAYM_CYCL_SCORE[is.na(cust0$PAYM_CYCL_SCORE)] <-mean(cust0$PAYM_CYCL_SCORE[!is.na(cust0$PAYM_CYCL_SCORE)])
cust0$TOTAL_CT_SCORE[is.na(cust0$TOTAL_CT_SCORE)]  <- mean(!is.na(cust0$TOTAL_CT_SCORE))


cust1<- cust0[order(cust0$SIU_CUST_YN),]




#트레이닝/테스트 데이터셋 디바이딩
#샘플링 이렇게 꼭안해도 되고 업샐링해서 하는게 좋음 
#8:2인데 업샘플링으로 50:50 으로 맞춤
n0 <- 1500
n1 <- 306
samp <- c(sample(1:18801,(n0+n1)), 18802:20607)
samp0 <- append(samp[1:n0],samp[(n0+n1+1):(n0*2+n1)])
samp1 <- append(samp[(n0+1):(n0+n1)],samp[(n0*2+n1+1):((n0+n1)*2)])

fds.tr<-cust1[samp0,]
fds.te<-cust1[samp1,]
x <- subset(fds.te, select=-FRAUD)
y <- fds.te$FRAUD                
# X,Y분리 한것은 테스트를 봐서 오차를 비교하기위해서


#로지스틱 만들고
#FRAUD ~ RESI_TYPE_SCORE+CTPR_SCORE+CHLD_CNT_SCORE+ OCCP_GRP_SCORE+FP_CAREER_SCORE+CUST_ROLE_SCORE+CUST_GOOD_SCORE+SALE_CHNL_SCORE+PAYM_CYCL_SCORE+ACCI_DVSN_SCORE	+DMND_RESN_CODE_SCORE+DMND_RSCD_SQNO_SCORE+HOSP_SPEC_DVSN_SCORE+CHANG_FP_YN_SCORE+PMMI_DLNG_YN_SCORE
# 추가 

#pred 의 숫자는 확률임 8024번째 손님이 0.8939의 부정고객의 손님

# Logistic Regression

install.packages('rms')
library(rms)
lout<-glm(FRAUD ~ RESI_TYPE_SCORE+CTPR_SCORE+CHLD_CNT_SCORE
           + OCCP_GRP_SCORE+FP_CAREER_SCORE
           +CUST_ROLE_SCORE+CUST_GOOD_SCORE
           +SALE_CHNL_SCORE+PAYM_CYCL_SCORE 
           +ACCI_DVSN_SCORE+DMND_RESN_CODE_SCORE
           +DMND_RSCD_SQNO_SCORE+HOSP_SPEC_DVSN_SCORE
           +CHANG_FP_YN_SCORE+PMMI_DLNG_YN_SCORE
           +TOTAL_A_SCORE
          ,family = binomial(link = "logit"))
pred<-predict(lout,x, type="response")
pred<-ifelse(pred>0.5,1,0)
table(pred, y)

# Tree Model
library(party)
ctree <-ctree (FRAUD ~ RESI_TYPE_SCORE+CTPR_SCORE+CHLD_CNT_SCORE
               + OCCP_GRP_SCORE+FP_CAREER_SCORE
               +CUST_ROLE_SCORE+CUST_GOOD_SCORE
               +SALE_CHNL_SCORE+PAYM_CYCL_SCORE 
               +ACCI_DVSN_SCORE+DMND_RESN_CODE_SCORE
               +DMND_RSCD_SQNO_SCORE+HOSP_SPEC_DVSN_SCORE
               +CHANG_FP_YN_SCORE+PMMI_DLNG_YN_SCORE
               +TOTAL_CT_SCORE
              , data=fds.tr)
pred <-predict(ctree, x)
table(pred, y)
# Random Forest Model
memory.limit(40000)
install.packages('randomForest')
library(randomForest)
rf<-randomForest(FRAUD ~ RESI_TYPE_SCORE+CTPR_SCORE+CHLD_CNT_SCORE
                 + OCCP_GRP_SCORE+FP_CAREER_SCORE
                 +CUST_ROLE_SCORE+CUST_GOOD_SCORE
                 +SALE_CHNL_SCORE+PAYM_CYCL_SCORE 
                 +ACCI_DVSN_SCORE+DMND_RESN_CODE_SCORE
                 +DMND_RSCD_SQNO_SCORE+HOSP_SPEC_DVSN_SCORE
                 +CHANG_FP_YN_SCORE+PMMI_DLNG_YN_SCORE
                 +TOTAL_CT_SCORE
                 , data=fds.tr, ntree=100, proximity=TRUE)
pred<-predict(rf,x)
table(pred,y)

install.packages('nnet')
library(nnet)
nnetModel <-nnet(FRAUD ~ RESI_TYPE_SCORE+CTPR_SCORE+CHLD_CNT_SCORE
                 + OCCP_GRP_SCORE+FP_CAREER_SCORE
                 +CUST_ROLE_SCORE+CUST_GOOD_SCORE
                 +SALE_CHNL_SCORE+PAYM_CYCL_SCORE 
                 +ACCI_DVSN_SCORE+DMND_RESN_CODE_SCORE
                 +DMND_RSCD_SQNO_SCORE+HOSP_SPEC_DVSN_SCORE
                 +CHANG_FP_YN_SCORE+PMMI_DLNG_YN_SCORE
                 +TOTAL_CT_SCORE
, data=fds.tr, size = 10, maxit=500)
pred<-predict(nnetModel,x, type = "class")
table(pred,y)
