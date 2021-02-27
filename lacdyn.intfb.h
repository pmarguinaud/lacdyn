INTERFACE
!$acc routine(LACDYN) seq
SUBROUTINE LACDYN(YRVFE, YRLDDH, YRSTA, YRVAB, YRPARAR, YRVETA, YRGEM, YRPHY, YGFL, &
 & YRMDDH, YRDYN, YRDIMV, YRDIM, KLON, YDGMV, &
 
 & KIDIA,KFDIA,PBETADT,PDT,PSLHDA,PSLHDD0, &
 & YDGSGEOM,YDOROG,POROGL,POROGM,&
 & PVCRSSA9F,PNHXT9,PVCRS0,PVCW0F,PWDLW0F,PRDLR0,PRDLR9,PNHXT0,&
 & PRES0,PRDELP0,PCTY0,PUVH0,&
 & PATND,PDBBC,PRDPHI,PGWT0,PGWT9,PGFL,&
 
 & KSETTLOFF,PGMV,PGMVS,PB1,PB2,PGMVT1,PGMVT1S,PGWS,PGMVTNDSI,PWRL95,KSTPT,KSTSZ,PSTACK)

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMDIM   , ONLY : TDIM
USE YOMDIMV  , ONLY : TDIMV
USE YOMDYNA  , ONLY : LGWADV, LRDBBC, LSLHD, LNH_PDVD, LNH_GEOGW,&
                    & LCOMAD, LCOMADH, LCOMADV, LPC_FULL, LPC_CHEAP
USE YOMDYN   , ONLY : TDYN
USE YOM_YGFL , ONLY : TYPE_GFLD
USE PAR_RDLR , ONLY : JP_DIMR0, JP_DIMR9
USE YOMMDDH  , ONLY : TMDDH
USE YOMGSGEOM, ONLY : TGSGEOM
USE YOMOROG  , ONLY : TOROG
USE INTDYN_MOD,ONLY : YYTVC0, YYTTND, YYTHW0, YYTCTY0
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE YOMPHY   , ONLY : TPHY
USE YOMGEM   , ONLY : TGEM
USE YOMVERT  , ONLY : TVETA
USE YOMPARAR , ONLY : TPARAR
USE YOMVERT  , ONLY : TVAB
USE YOMSTA   , ONLY : TSTA
USE YOMLDDH  , ONLY : TLDDH
USE YOMVERT  , ONLY : TVFE

TYPE (TVFE),       INTENT(IN) :: YRVFE
TYPE (TLDDH),      INTENT(IN) :: YRLDDH
TYPE (TSTA),       INTENT(IN) :: YRSTA
TYPE (TVAB),       INTENT(IN) :: YRVAB
TYPE (TPARAR),     INTENT(IN) :: YRPARAR
TYPE (TVETA),      INTENT(IN) :: YRVETA
TYPE (TGEM),       INTENT(IN) :: YRGEM
TYPE (TPHY),       INTENT(IN) :: YRPHY
TYPE (TDIM),       INTENT(IN) :: YRDIM
TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TMDDH),      INTENT(IN) :: YRMDDH
TYPE (TYPE_GFLD),  INTENT(IN) :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) ,       INTENT(IN)    :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSETTLOFF(YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBETADT
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLHDA(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLHDD0(KLON)
TYPE(TGSGEOM)     ,INTENT(IN)    :: YDGSGEOM
TYPE(TOROG)       ,INTENT(IN)    :: YDOROG
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGL(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRSSA9F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT9(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCW0F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWDLW0F(YRDIM%NPROMDLW,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR9(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR9)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT0(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRES0(KLON,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP0(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCTY0(KLON,0:YRDIMV%NFLEVG,YYTCTY0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUVH0(KLON,0:YRDIMV%NFLEVG,YYTHW0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PATND(KLON,YRDIMV%NFLEVG,YYTTND%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDBBC(YRDIM%NPROMNH)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDPHI(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT0(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT9(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(KLON,YRDIMV%NFLEVG,YGFL%NDIM) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVS(KLON,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(KLON,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB2(KLON,YRPTRSLB2%NFLDSLB2)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1(KLON,YRDIMV%NFLEVG,YDGMV%YT1%NDIM)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1S(KLON,YDGMV%YT1%NDIMS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGWS(YRDIM%NPROMNH)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVTNDSI(KLON,YRDIMV%NFLEVG,YRMDDH%NDIMSIGMV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWRL95(KLON,YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

END SUBROUTINE LACDYN

END INTERFACE
