INTERFACE

!$acc routine(LAVENT) seq
SUBROUTINE LAVENT(YRVETA, YRPARAR, YRDYN, YRDIMV, YRDIM, &
 & KLON, YDGMV,KIDIA,KFDIA,KSETTLOFF,LD2TLFF1,PVCRS0,PRDLR0,PDTS2,PRDELP,PEVEL,PATND,&
 & PGMV,PB1,PB2,PWRL9,KSTPT,KSTSZ,PSTACK)

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RA
USE YOMCT0   , ONLY : LAROME, LTWOTL
USE YOMCT3   , ONLY : NSTEP
USE YOMCVER  , ONLY : LVERTFE
USE YOMDIM   , ONLY : TDIM
USE YOMDIMV  , ONLY : TDIMV
USE YOMDYNA  , ONLY : LVERCOR, LRWSDLR, LPC_FULL, LNESCT, LNESCV, LELTRA, LSETTLST, LSETTLSV
USE YOMDYN   , ONLY : TDYN
USE YOMVERT  , ONLY : TVETA
USE YOMPARAR , ONLY : TPARAR
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE PAR_RDLR , ONLY : JP_ASRF, JP_DIMR0
USE INTDYN_MOD,ONLY : YYTVC0, YYTTND

TYPE (TDIM),       INTENT(IN) :: YRDIM
TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TPARAR),     INTENT(IN) :: YRPARAR
TYPE (TVETA),      INTENT(IN) :: YRVETA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) ,       INTENT(IN)    :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSETTLOFF(YRDIMV%NFLEVG)
LOGICAL           ,INTENT(IN)    :: LD2TLFF1 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDTS2 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVEL(KLON,0:YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PATND(KLON,YRDIMV%NFLEVG,YYTTND%NDIM)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(KLON,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PB2(KLON,YRPTRSLB2%NFLDSLB2)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWRL9(KLON,YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE LAVENT

END INTERFACE
