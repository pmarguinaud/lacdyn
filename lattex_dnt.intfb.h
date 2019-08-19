INTERFACE
SUBROUTINE LATTEX_DNT(KST,KPROF,LDSETTLS,KXLAG,PESGP,PESGM,PXT0,PXT9,PMOY1X,&
 & PXSI,PXNLT9,PXT1,PXL0,PXL9,PXLF9,PCXNLT9,&
 & PSIDDHXT1,PSIDDHXT9,PSIDDHXL0,LDNESC)
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMDYNA  , ONLY : LNESC, LPC_FULL, LPC_CHEAP
USE YOMCT3   , ONLY : NSTEP
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYN   , ONLY : YRDYN
USE YOMLDDH  , ONLY : YRLDDH
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
LOGICAL           ,INTENT(IN)    :: LDSETTLS
INTEGER(KIND=JPIM),INTENT(IN)    :: KXLAG
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGM
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXT0(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXT9(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMOY1X(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXSI(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXNLT9(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXT1(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXL0(YRDIM%NPROMA,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXL9(YRDIM%NPROMA,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXLF9(YRDIM%NPROMA,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCXNLT9(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXT1(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXT9(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXL0(YRDIM%NPROMA,YRDIMV%NFLSA:YRDIMV%NFLEN)
LOGICAL,OPTIONAL  ,INTENT(IN)    :: LDNESC
END SUBROUTINE LATTEX_DNT
END INTERFACE