INTERFACE
SUBROUTINE LATTES(YRVFE, YRVETA, YRVAB, YRSTA, YRDYN, YRDIMV, YRDIM, &
 & KLON, YDGMV,KIDIA,KFDIA,PDTS2,PBDT,PESGP,PESGM,&
 & PVCRS0,PRDLR0,YDOROG,POROGL,POROGM,&
 & PSDIV0,PSDIV9,PSDVBC,PRES0,PGMVS,&
 & PGMV,PGMVT1S,PB1,PB2,KSTPT,KSTSZ,PSTACK)

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LTWOTL
USE YOMCT3   , ONLY : NSTEP
USE YOMCVER  , ONLY : LVERTFE, LVFE_INTB
USE YOMDIM   , ONLY : TDIM
USE YOMDIMV  , ONLY : TDIMV
USE YOMVERT  , ONLY : TVAB, TVETA, TVFE
USE YOMOROG  , ONLY : TOROG
USE YOMDYNA  , ONLY : LVERCOR, LRWSDLR, LPC_FULL, LPC_CHEAP, LNESC, LSETTLS, LSLINLC1
USE YOMDYN   , ONLY : TDYN
USE YOMSTA   , ONLY : TSTA
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE PAR_RDLR , ONLY : JP_ASRF, JP_DIMR0
USE INTDYN_MOD,ONLY : YYTVC0

TYPE (TDIM),       INTENT(IN) :: YRDIM
TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TSTA),       INTENT(IN) :: YRSTA
TYPE (TVAB),       INTENT(IN) :: YRVAB
TYPE (TVETA),      INTENT(IN) :: YRVETA
TYPE (TVFE),       INTENT(IN) :: YRVFE
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) ,       INTENT(IN)    :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDTS2 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBDT(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGM 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
TYPE(TOROG)       ,INTENT(IN)    :: YDOROG
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDIV0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDIV9(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDVBC(KLON,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRES0(KLON,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMVS(KLON,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1S(KLON,YDGMV%YT1%NDIMS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(KLON,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB2(KLON,YRPTRSLB2%NFLDSLB2)
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE LATTES

END INTERFACE
