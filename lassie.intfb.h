INTERFACE
SUBROUTINE LASSIE(PSTACK, KPSTSZ, KLON, YDGMV,KIDIA,KFDIA,PRCORI,PGMV,PGMVS,PGFL,&
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
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) , INTENT(INOUT) :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRCORI(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMVS(KLON,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(KLON,YRDIMV%NFLEVG,YGFL%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV0(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV9(KLON)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD0(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD9(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0L(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0M(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9L(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9M(KLON,YRDIMV%NFLEVG)
END SUBROUTINE LASSIE
END INTERFACE
