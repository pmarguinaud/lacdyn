INTERFACE
!$acc routine(LAVABO) seq
SUBROUTINE LAVABO(YGFL, YRDYN, YRDIMV, YRDIM, &
                & KLON,KIDIA,KFDIA,LD2TLFF1,PB1,KSTPT,KSTSZ,PSTACK)

USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCT0   , ONLY : LNHDYN, LSLPHY
USE YOMDIM   , ONLY : TDIM
USE YOMDIMV  , ONLY : TDIMV
USE YOMDYNA  , ONLY : LVERCOR, LRWSDLR, LRWSDLW, NVDVAR, LSLINL, LSLINLC1, LSLINLC2
USE YOMDYN   , ONLY : TDYN
USE PTRSLB1  , ONLY : YRPTRSLB1
USE YOM_YGFL , ONLY : TYPE_GFLD

TYPE (TDIM),       INTENT(IN) :: YRDIM
TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TYPE_GFLD),  INTENT(IN) :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
LOGICAL           ,INTENT(IN)    :: LD2TLFF1 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(KLON,YRPTRSLB1%NFLDSLB1)

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE LAVABO

END INTERFACE
