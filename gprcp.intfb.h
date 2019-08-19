INTERFACE
SUBROUTINE GPRCP(KPROMA,KSTART,KPROF,KFLEV,PQ,PQI,PQL,PQR,PQS,PQG,&
 & PCP,PR,PKAP,PGFL,KGFLTYP)
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCST   , ONLY : RD, RV, RCPD, RCPV, RCW, RCS
USE YOM_YGFL , ONLY : YGFL
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTART
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQ(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQI(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQL(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQR(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQS(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQG(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PCP(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PR(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PKAP(KPROMA,KFLEV)
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PGFL(KPROMA,KFLEV,YGFL%NDIM)
INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN)    :: KGFLTYP
END SUBROUTINE GPRCP
END INTERFACE