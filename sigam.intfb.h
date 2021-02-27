INTERFACE
SUBROUTINE SIGAM(YRVFE, YRVETA, YRDYN, KIDIA,KFDIA,KLON,KFLEVG,PD,PT,PSP,KSTPT,KSTSZ,PSTACK)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCT0   , ONLY : LNHDYN
USE YOMCST   , ONLY : RD
USE YOMDYN   , ONLY : TDYN
USE YOMVERT  , ONLY : TVETA, TVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB

TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TVETA),      INTENT(IN) :: YRVETA
TYPE (TVFE),       INTENT(IN) :: YRVFE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD(KLON,KFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KLON) 

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE SIGAM

END INTERFACE
