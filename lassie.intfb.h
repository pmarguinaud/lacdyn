INTERFACE
SUBROUTINE LASSIE(YDGMV,KST,KPROF,PRCORI,PGMV,PGMVS,PGFL,&
 & PSDIV0,PSDIV9,PTOD0,PTOD9,PGAGT0L,PGAGT0M,PGAGT9L,PGAGT9M)
USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LTWOTL, LSPRT
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYN   , ONLY : YRDYN
USE YOM_YGFL , ONLY : YGFL
IMPLICIT NONE
TYPE(TGMV) , INTENT(INOUT) :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRCORI(YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMV(YRDIM%NPROMA,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMVS(YRDIM%NPROMA,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(YRDIM%NPROMA,YRDIMV%NFLEVG,YGFL%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV0(YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV9(YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD0(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD9(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0L(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0M(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9L(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9M(YRDIM%NPROMA,YRDIMV%NFLEVG)
END SUBROUTINE LASSIE
END INTERFACE
