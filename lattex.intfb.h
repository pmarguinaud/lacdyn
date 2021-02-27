INTERFACE
SUBROUTINE LATTEX(YGFL, YRVAB, YRSTA, YRMDDH, YRLDDH, YRDYN, YRDIMV, YRDIM, KLON, YDGMV, &
 
 & KIDIA,KFDIA,PDTS2,PBT,PBDT,PESGP,PESGM,&
 & YDGSGEOM,YDOROG,POROGL,POROGM,&
 & PVCRS0,PVCRSSA9F,PRDLR0,PRDLR9,PVCW0F,PWDLW0F,&
 & PGAGT0L,PGAGT0M,PTOD0,&
 & PGAGT9L,PGAGT9M,PTOD9,&
 & PRDELP,PEVEL,PATND,&
 & PNHXT0,PGWT0,PNHXT9,PGWT9,PGFL,&
 
 & PGMV,PGMVT1,PGMVTNDSI,PB1, &
 
 & PB2,KSTPT,KSTSZ,PSTACK)

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LNHDYN, LTWOTL
USE YOMCT3   , ONLY : NSTEP
USE YOMCVER  , ONLY : LVERTFE
USE YOMDIM   , ONLY : TDIM
USE YOMDIMV  , ONLY : TDIMV
USE YOMDYN   , ONLY : TDYN
USE YOMDYNA  , ONLY : LGWADV, NPDVAR, NVDVAR, ND4SYS, LNH_PDVD, LNH_GEOGW, &
 & LVERCOR, LRWSDLW, LRWSDLR, LPC_FULL, LPC_CHEAP, LNESC, LSETTLS
USE YOMVERT  , ONLY : TVAB
USE YOMGSGEOM, ONLY : TGSGEOM
USE YOMOROG  , ONLY : TOROG
USE YOMSTA   , ONLY : TSTA
USE YOM_YGFL , ONLY : TYPE_GFLD
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE PAR_RDLR , ONLY : JP_ASRF, JP_RSAF, JP_DIMR0, JP_DIMR9
USE INTDYN_MOD,ONLY : YYTVC0, YYTTND
USE YOMMDDH  , ONLY : TMDDH
USE YOMLDDH  , ONLY : TLDDH

TYPE (TDIM),       INTENT(IN) :: YRDIM
TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TLDDH),      INTENT(IN) :: YRLDDH
TYPE (TMDDH),      INTENT(IN) :: YRMDDH
TYPE (TSTA),       INTENT(IN) :: YRSTA
TYPE (TVAB),       INTENT(IN) :: YRVAB
TYPE (TYPE_GFLD),  INTENT(IN) :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) ,       INTENT(IN)    :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDTS2 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBDT(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGM 
TYPE(TGSGEOM)     ,INTENT(IN)    :: YDGSGEOM
TYPE(TOROG)       ,INTENT(IN)    :: YDOROG
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRSSA9F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR9(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR9)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCW0F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWDLW0F(YRDIM%NPROMDLW,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAGT0L(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAGT0M(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTOD0(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAGT9L(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGAGT9M(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTOD9(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVEL(KLON,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PATND(KLON,YRDIMV%NFLEVG,YYTTND%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT0(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT0(YRDIM%NPROMNH,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT9(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT9(YRDIM%NPROMNH,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(KLON,YRDIMV%NFLEVG,YGFL%NDIM) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1(KLON,YRDIMV%NFLEVG,YDGMV%YT1%NDIM)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVTNDSI(KLON,YRDIMV%NFLEVG,YRMDDH%NDIMSIGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(KLON,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB2(KLON,YRPTRSLB2%NFLDSLB2)
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE LATTEX

END INTERFACE
