INTERFACE
SUBROUTINE SITNU(PSTACK, KPSTSZ, KPSTPT, KIDIA,KFDIA,KLON,KFLEVG,PD,PT,PSP)
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCT0   , ONLY : LNHDYN
USE YOMDIMV  , ONLY : YRDIMV
USE YOMCST   , ONLY : RKAPPA
USE YOMDYN   , ONLY : YRDYN
USE YOMVERT  , ONLY : YRVETA, YRVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB
IMPLICIT NONE
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KPSTPT
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KLON)
END SUBROUTINE SITNU
END INTERFACE
