SUBROUTINE GPRCP(KLON,KIDIA,KFDIA,KFLEV,PQ,PQI,PQL,PQR,PQS,PQG,&
 & PCP,PR,PKAP,PGFL,KGFLTYP)

!**** *GPRCP* - Computes Cp, R and R/Cp from Q

!     Purpose.
!     --------
!           COMPUTES CP AND R  AND R/CP FROM Q

!**   Interface.
!     ----------
!        *CALL* *GPRCP(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KLON               - dimensioning.
!          KIDIA               - start of work.
!          KFDIA                - depth of work.
!          KFLEV                - number of layers.
!          PQ(KLON,KFLEV)     - specific humidity.
!          PQI(KLON,KFLEV)    - ice.
!          PQL(KLON,KFLEV)    - liquid water.
!          PQR(KLON,KFLEV)    - rain.
!          PQS(KLON,KFLEV)    - snow.
!          PQG(KLON,KFLEV)    - graupel.

!        OUTPUT:
!          PCP(KLON,KFLEV)    - CP
!          PR(KLON,KFLEV)     - R
!          PKAP(KLON,KFLEV)   - KAPPA

!        Implicit arguments :  Physical constants from YOMCST
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 88-02-04

!     Modifications.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      Modified by Y.Seity  04-02-13 (Rain, Snow and Graupel)
!      M.Hamrud  15-Jan-2006  Revised GPRCP
!      K. Yessad (Jan 2011): more compact rewriting.
!      R. El Khatib 28-Aug-2014 Optimizations :
!       - compute R or CP only if required
!       - loop collapsing whenever possible, through pure array syntax
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD, RV, RCPD, RCPV, RCW, RCS
USE YOM_YGFL , ONLY : YGFL

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQ(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQI(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQL(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQR(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQS(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PQG(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(IN)    :: PGFL(KLON,KFLEV,YGFL%NDIM) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PCP(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PR(KLON,KFLEV) 
REAL(KIND=JPRB),OPTIONAL   ,INTENT(OUT)   :: PKAP(KLON,KFLEV) 
INTEGER(KIND=JPIM),OPTIONAL,INTENT(IN)    :: KGFLTYP

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZCP(KLON,KFLEV)
REAL(KIND=JPRB) :: ZR(KLON,KFLEV)
REAL(KIND=JPRB) :: ZGFL_R(YGFL%NUMFLDS), ZGFL_CP(YGFL%NUMFLDS)
REAL(KIND=JPRB) :: ZGFL_X(YGFL%NUMFLDS), ZX

INTEGER(KIND=JPIM) :: JGFL
INTEGER(KIND=JPIM) :: IACT(YGFL%NUMFLDS),IPT(YGFL%NUMFLDS),INUMACT,IGFLTYP
LOGICAL :: LLGFL,LLQ,LLQL,LLQI,LLQR,LLQS,LLQG,LLKAP, LLR, LLCP


#include "abor1.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*       1.    COMPUTES R AND CP AND KAPPA.
!              ----------------------------

LLGFL = PRESENT(PGFL)
IF(.NOT. LLGFL) THEN
  LLQ  = PRESENT(PQ)
  LLQL = PRESENT(PQL)
  LLQI = PRESENT(PQI)
  LLQR = PRESENT(PQR)
  LLQS = PRESENT(PQS)
  LLQG = PRESENT(PQG)
ENDIF

LLKAP=PRESENT(PKAP)
LLR=PRESENT(PR).OR.LLKAP
LLCP=PRESENT(PCP).OR.LLKAP

! * compute IPT:
IF(LLGFL) THEN
  IGFLTYP = 0
  IF(PRESENT(KGFLTYP)) IGFLTYP=KGFLTYP
  INUMACT = 0
  DO JGFL=1,YGFL%NUMFLDS
    IF(YGFL%YCOMP(JGFL)%LTHERMACT) THEN
      INUMACT = INUMACT+1
      IACT(INUMACT) = JGFL
      IF(IGFLTYP == 0) THEN
        IPT(INUMACT) = YGFL%YCOMP(JGFL)%MP
      ELSEIF(IGFLTYP == 1) THEN
        IPT(INUMACT) = YGFL%YCOMP(JGFL)%MP1
      ELSEIF(IGFLTYP == 5) THEN
        IPT(INUMACT) = YGFL%YCOMP(JGFL)%MP5
      ELSEIF(IGFLTYP == 9) THEN
        IPT(INUMACT) = YGFL%YCOMP(JGFL)%MP9_PH
      ELSEIF(IGFLTYP == 101) THEN
        IPT(INUMACT) = YGFL%YCOMP(JGFL)%MP_SL1
      ELSE
        CALL ABOR1('GPRCP:UNKNOWN GFL TYPE')
      ENDIF
    ENDIF
  ENDDO
ENDIF

! * compute ZR,ZCP:
IF(LLGFL) THEN
  IF(LLR) THEN
    IF(INUMACT == 0) THEN
      ZR(KIDIA:KFDIA,:) = RD
    ELSE
!     Does not vectorize:
      DO JGFL=1,INUMACT
        ZGFL_X(JGFL) = YGFL%YCOMP(IACT(JGFL))%R
      ENDDO
!     Vectorizes:
      ZGFL_R(1:INUMACT) = ZGFL_X(1:INUMACT)-RD
      ZR(KIDIA:KFDIA,:) = ZGFL_R(1)*PGFL(KIDIA:KFDIA,:,IPT(1))
      DO JGFL=2,INUMACT
        CALL ACCUMULATE(ZGFL_R(JGFL),PGFL(1,1,IPT(JGFL)),ZR)
      ENDDO
      ZR(KIDIA:KFDIA,:) = RD+ZR(KIDIA:KFDIA,:)
    ENDIF
  ENDIF
  IF(LLCP) THEN
    IF(INUMACT == 0) THEN
      ZCP(KIDIA:KFDIA,:) = RCPD
    ELSE
!     Does not vectorize:
      DO JGFL=1,INUMACT
        ZGFL_X(JGFL) = YGFL%YCOMP(IACT(JGFL))%RCP
      ENDDO
!     Vectorizes:
      ZGFL_CP(1:INUMACT) = ZGFL_X(1:INUMACT)-RCPD
      ZCP(KIDIA:KFDIA,:) = ZGFL_CP(1)*PGFL(KIDIA:KFDIA,:,IPT(1))
      DO JGFL=2,INUMACT
        CALL ACCUMULATE(ZGFL_CP(JGFL),PGFL(1,1,IPT(JGFL)),ZCP)
      ENDDO
      ZCP(KIDIA:KFDIA,:) = RCPD+ZCP(KIDIA:KFDIA,:)
    ENDIF
  ENDIF
ELSE
  IF(LLR) THEN
    ZR(KIDIA:KFDIA,:)  = RD
  ENDIF 
  IF(LLCP) THEN
    ZCP(KIDIA:KFDIA,:) = RCPD
  ENDIF 
  IF(LLQ .AND. YGFL%YQ%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=RV-RD
      CALL ACCUMULATE(ZX,PQ,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCPV-RCPD
      CALL ACCUMULATE(ZX,PQ,ZCP)
    ENDIF 
  ENDIF
  IF(LLQL .AND. YGFL%YL%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=-RD
      CALL ACCUMULATE(ZX,PQL,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCW-RCPD
      CALL ACCUMULATE(ZX,PQL,ZCP)
    ENDIF 
  ENDIF
  IF(LLQI .AND. YGFL%YI%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=-RD
      CALL ACCUMULATE(ZX,PQI,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCS-RCPD
      CALL ACCUMULATE(ZX,PQI,ZCP)
    ENDIF 
  ENDIF
  IF(LLQR .AND. YGFL%YR%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=-RD
      CALL ACCUMULATE(ZX,PQR,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCW-RCPD
      CALL ACCUMULATE(ZX,PQR,ZCP)
    ENDIF 
  ENDIF
  IF(LLQS .AND. YGFL%YS%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=-RD
      CALL ACCUMULATE(ZX,PQS,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCS-RCPD
      CALL ACCUMULATE(ZX,PQS,ZCP)
    ENDIF 
  ENDIF
  IF(LLQG .AND. YGFL%YG%LTHERMACT) THEN
    IF(LLR) THEN
      ZX=-RD
      CALL ACCUMULATE(ZX,PQG,ZR)
    ENDIF 
    IF(LLCP) THEN
      ZX=RCS-RCPD
      CALL ACCUMULATE(ZX,PQG,ZCP)
    ENDIF 
  ENDIF
ENDIF

! * fill PR,PCP,PKAP:
IF (LLR) PR(KIDIA:KFDIA,:)=ZR(KIDIA:KFDIA,:)
IF (LLCP) PCP(KIDIA:KFDIA,:)=ZCP(KIDIA:KFDIA,:)
IF (LLKAP) PKAP(KIDIA:KFDIA,:) = ZR(KIDIA:KFDIA,:)/ZCP(KIDIA:KFDIA,:)

!     ------------------------------------------------------------------




CONTAINS

SUBROUTINE ACCUMULATE(PX,PGFL,POUT)
USE PARKIND1 , ONLY : JPRB

REAL(KIND=JPRB), INTENT(IN) :: PX
REAL(KIND=JPRB), INTENT(IN) :: PGFL(KLON,KFLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: POUT(KLON,KFLEV)

IF (KIDIA==1 .AND. KFDIA==KLON) THEN
! Implicit loop collapsing
  POUT(:,:) = POUT(:,:)+PX*PGFL(:,:)
ELSE
! Nested loops : avoid uninitialized PGFL on the edge
  POUT(KIDIA:KFDIA,:) = POUT(KIDIA:KFDIA,:)+PX*PGFL(KIDIA:KFDIA,:)
ENDIF

END SUBROUTINE ACCUMULATE

END SUBROUTINE GPRCP
