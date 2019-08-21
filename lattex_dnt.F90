SUBROUTINE LATTEX_DNT(PSTACK, KPSTSZ, KPSTPT, KLON, KIDIA,KFDIA,LDSETTLS,KXLAG,PESGP,PESGM,PXT0,PXT9,PMOY1X,&
 & PXSI,PXNLT9,PXT1,PXL0,PXL9,PXLF9,PCXNLT9,&
 & PSIDDHXT1,PSIDDHXT9,PSIDDHXL0,LDNESC)

!------------------------------------------------------------------------------
! LATTEX_DNT - Semi-Lagrangian scheme.
!              Computation of the t and t-dt useful quantities
!              at grid-points. Equations for tri-dimensional
!              variables for 2TL scheme.

! Purpose
! -------

! Interface
! ---------
!   CALL LATTEX_DNT(..)

! Explicit arguments :
! --------------------

! * INPUT:
!        KIDIA       - first element of work.
!        KFDIA     - depth of work.
!        LDSETTLS  - .T./.F.: Stable/Conventional extrapolations for SL2TL.
!        KXLAG     - type of SL discretisation 
!        PESGP     - (1 + uncentering factor).
!        PESGM     - (1 - uncentering factor).
!        PXT0      - prognostic variable time t (predictor or SI),
!                    preliminary t+dt (corrector)
!        PXT9      - prognostic variable time t (corrector),
!                    not used for predictor or SI
!        PMOY1X    - full nonlinear model at time t [(Delta t/2) "cursive" A]

! * INPUT/OUTPUT:
!        PXSI      - semi-implicit linear model at time t
!                    [- (Delta t/2) "cursive" B]
!        PXNLT9    - buffer used during predictor resp. SI time step
!        PXT1      - t+dt term and other final point terms 
!        PXL0      - second SL quantity to be interpolated (linearly) at O if KXLAG>3
!        PXL9      - SL quantity to be interpolated at O 
!                    (if NSPLTHOI=1 with diffusive interpolation)
!        PXLF9     - SL quantity to be interpolated at O with high order
!                    interpolation (only meaningfull with NSPLTHOI /= 0)
!        PCXNLT9   - buffer used during corrector
!        PSIDDHX.. - buffers for DDH (linear terms).

! * OPTIONAL INPUT:
!        LDNESC    - cf. LNESC in YOMDYNA.

! Externals
! ---------
!   none
!   Called by LATTEX.

! Method
! ------

! Reference
! ---------
!   Arpege documentation about semi-Lagrangian scheme.

! Author
! ------
!      Mar-2002 J. VIVODA (SHMI/CHMI/LACE) - rationalization of LATTEX

! Modifications
! -------------
!   12-Oct-2002 J. Masek   PC bugfix
!   01-Oct-2003 M. Hamrud  CY28 Cleaning
!   09-Jun-2004 J. Masek   NH cleaning (LPC_NOTR, LDFULLIMP)
!   01-Jul-2004 K. Yessad  Make clearer the tests for PC scheme.
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Aug 2008): simplify XIDT treatment with PC + cleanings
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   F. Vana  15-Oct-2009: option NSPLTHOI
!   F. Vana  22-Feb-2011: option K[X]LAG=4
!   K. Yessad (Oct 2013): allow NESC without ICI-PC scheme.
!   K. Yessad (July 2014): Move some variables, rename some variables.
! End Modifications
!------------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMDYNA  , ONLY : LNESC, LPC_FULL, LPC_CHEAP
USE YOMCT3   , ONLY : NSTEP
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYN   , ONLY : YRDYN
USE YOMLDDH  , ONLY : YRLDDH

!     ------------------------------------------------------------------

IMPLICIT NONE


REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KPSTPT
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
LOGICAL           ,INTENT(IN)    :: LDSETTLS
INTEGER(KIND=JPIM),INTENT(IN)    :: KXLAG
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGM
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXT0(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXT9(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMOY1X(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXSI(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXNLT9(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXT1(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXL0(KLON,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXL9(KLON,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXLF9(KLON,YRDIMV%NFLSA:YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCXNLT9(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXT1(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXT9(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSIDDHXL0(KLON,YRDIMV%NFLSA:YRDIMV%NFLEN)
LOGICAL,OPTIONAL  ,INTENT(IN)    :: LDNESC

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV,JLON

!     * ZXNLT0 (resp. ZXNLT1) the non linear term at t (resp. t+dt).
REAL(KIND=JPRB) :: ZXIDT0,ZXIDT9,ZXIGP

LOGICAL :: LLCT,LLNESC
INTEGER(KIND=JPIM) :: IPSTPT_ZXNLT0

INTEGER(KIND=JPIM) :: IPSTPT_ZXNLT1

INTEGER(KIND=JPIM) :: IPSTPT




!     ------------------------------------------------------------------

#include "abor1.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*      1. AUXILIARY VARIABLES.
!       -----------------------


IPSTPT = KPSTPT

IPSTPT_ZXNLT1 = IPSTPT

IPSTPT = IPSTPT + 1

#define ZXNLT1(i) PSTACK (i,IPSTPT_ZXNLT1)

IPSTPT_ZXNLT0 = IPSTPT

IPSTPT = IPSTPT + 1

#define ZXNLT0(i) PSTACK (i,IPSTPT_ZXNLT0)

IF (IPSTPT > KPSTSZ) CALL ABOR1 ('IPSTPT > KPSTSZ')
IF (PRESENT(LDNESC)) THEN
  LLNESC=LDNESC
ELSE
  LLNESC=LNESC
ENDIF

LLCT = (LPC_FULL .AND. YRDYN%NCURRENT_ITER > 0)

ZXIDT0=1.0_JPRB+YRDYN%XIDT
ZXIDT9=1.0_JPRB+YRDYN%XIDT
ZXIGP=1.0_JPRB+YRDYN%XIDT

!     ------------------------------------------------------------------

!*      2. MAIN CALCULATIONS.
!       ---------------------

IF( .NOT.LLCT )THEN

  !############################################
  ! 2.1 Predictor for LPC_FULL,
  !     or case NSITER=0.
  !############################################

  IF (LPC_CHEAP.AND.(.NOT.LLNESC)) THEN
    ! LPC_CHEAP=T is currently coded for LNESC only
    !  (for LNESC=F interpolated quantities are not the same ones
    !  at the predictor and corrector steps, and the LPC_CHEAP code
    !  currently assumes that they are identical). 
    CALL ABOR1('LATTEX_DNT: If LPC_CHEAP=T, LNESC should be T')
  ENDIF

  DO JLEV=1,YRDIMV%NFLEVG

    ! nonlinear residual time t
    DO JLON=KIDIA,KFDIA
      ZXNLT0(JLON)=PMOY1X(JLON,JLEV)+ZXIDT0*PXSI(JLON,JLEV)
    ENDDO

    ! Fill PXL9,PXLF9,PXL0,PXT1.
    IF ((KXLAG == 2).AND.(YRDYN%NSPLTHOI /= 0)) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV)
          PXLF9(JLON,JLEV)=PXLF9(JLON,JLEV)+PESGM*PMOY1X(JLON,JLEV)
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV)
          PXLF9(JLON,JLEV)=PXLF9(JLON,JLEV) &
           & +PESGM*PMOY1X(JLON,JLEV)+(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV)
          PXLF9(JLON,JLEV)=PXLF9(JLON,JLEV)+PESGM*PMOY1X(JLON,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JLON)-0.5_JPRB*PXNLT9(JLON,JLEV))
        ENDDO
      ENDIF
    ELSEIF ((KXLAG == 2).AND.(YRDYN%NSPLTHOI == 0)) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV) &
           & +PESGM*PMOY1X(JLON,JLEV)
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV) &
           & +PESGM*PMOY1X(JLON,JLEV)+(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JLON=KIDIA,KFDIA
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV) &
           & +PESGM*PMOY1X(JLON,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JLON)-0.5_JPRB*PXNLT9(JLON,JLEV))
        ENDDO
      ENDIF
    ELSEIF (KXLAG >= 3) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JLON=KIDIA,KFDIA
          PXL0(JLON,JLEV)=PXL0(JLON,JLEV)+PESGM*PMOY1X(JLON,JLEV)
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JLON=KIDIA,KFDIA
          PXL0(JLON,JLEV)=PXL0(JLON,JLEV)+PESGM*PMOY1X(JLON,JLEV) &
           & +(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT0(JLON)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JLON=KIDIA,KFDIA
          PXL0(JLON,JLEV)=PXL0(JLON,JLEV)+PESGM*PMOY1X(JLON,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JLON)-PXNLT9(JLON,JLEV))
          PXT1(JLON,JLEV)=PXT1(JLON,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JLON)-0.5_JPRB*PXNLT9(JLON,JLEV))
        ENDDO
      ENDIF
      DO JLON=KIDIA,KFDIA
        PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT0(JLON,JLEV)
      ENDDO
    ENDIF

    ! save quantities for corrector step
    IF( LPC_FULL )THEN
      ! save nonlinear model at time t
      DO JLON = KIDIA, KFDIA
      PCXNLT9(JLON,JLEV) = PMOY1X(JLON,JLEV)
      ENDDO
    ENDIF

    IF( .NOT.LLNESC )THEN
      ! save of nonlinear residual at time t
      ! to be used as nonlinear residual at time t-dt next time step
      DO JLON=KIDIA,KFDIA
        PXNLT9(JLON,JLEV)=PMOY1X(JLON,JLEV)+ZXIDT9*PXSI(JLON,JLEV)
      ENDDO
    ENDIF

  ENDDO

ELSE

  !############################################
  ! 2.2 Corrector for LPC_FULL
  !############################################

  DO JLEV=1,YRDIMV%NFLEVG

    ! nonlinear residual time t+dt (preliminary state from predictor)

    DO JLON=KIDIA,KFDIA
      ZXNLT1(JLON)=PMOY1X(JLON,JLEV)+ZXIDT9*PXSI(JLON,JLEV)
    ENDDO

    IF (KXLAG == 2) THEN
      IF (.NOT. LPC_CHEAP) THEN
        IF (YRDYN%NSPLTHOI /= 0) THEN
          DO JLON=KIDIA,KFDIA
            PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT9(JLON,JLEV)
            PXLF9(JLON,JLEV)=PXLF9(JLON,JLEV) &
             & +PESGM*PCXNLT9(JLON,JLEV)
          ENDDO
        ELSE
          DO JLON=KIDIA,KFDIA
            PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT9(JLON,JLEV) &
             & +PESGM*PCXNLT9(JLON,JLEV)
          ENDDO
        ENDIF
      ENDIF
      DO JLON=KIDIA,KFDIA
        PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT1(JLON)
      ENDDO
    ELSEIF (KXLAG >= 3) THEN
      IF (.NOT. LPC_CHEAP) THEN
        DO JLON=KIDIA,KFDIA
          PXL0(JLON,JLEV)=PXL0(JLON,JLEV)+PESGM*PCXNLT9(JLON,JLEV)
          PXL9(JLON,JLEV)=PXL9(JLON,JLEV)+PXT9(JLON,JLEV)
        ENDDO
      ENDIF
      DO JLON=KIDIA,KFDIA
        PXT1(JLON,JLEV)=PXT1(JLON,JLEV)+PESGP*ZXNLT1(JLON)
      ENDDO
    ENDIF

  ENDDO

ENDIF

!########################################################
! 2.3 Addition of preliminary quantity for LAGPHY physics
!########################################################

DO JLEV=1,YRDIMV%NFLEVG
  IF(YRDYN%XIDT > 0.0_JPRB)THEN
    DO JLON=KIDIA,KFDIA
      PXT1(JLON,JLEV)=PXT1(JLON,JLEV)-ZXIGP*PXSI(JLON,JLEV)
      PXSI(JLON,JLEV)=ZXIGP*PXSI(JLON,JLEV)
    ENDDO
  ELSE
    DO JLON=KIDIA,KFDIA
      PXT1(JLON,JLEV)=PXT1(JLON,JLEV)-PESGP*PXSI(JLON,JLEV)
      PXSI(JLON,JLEV)=PESGP*PXSI(JLON,JLEV)
    ENDDO
  ENDIF
ENDDO

!########################################################
! 2.4  DDH computations for SI correction 
!########################################################

IF (YRLDDH%LRSIDDH) THEN
  IF ( .NOT.LLCT ) THEN
    IF (KXLAG >= 3) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JLEV = 1, YRDIMV%NFLEVG
        DO JLON = KIDIA, KFDIA
        PSIDDHXT1(JLON,JLEV) = PSIDDHXT1(JLON,JLEV) &
         & +PESGP*ZXIDT0*PXSI(JLON,JLEV)
        ENDDO
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JLEV = 1, YRDIMV%NFLEVG
        DO JLON = KIDIA, KFDIA
        PSIDDHXL0(JLON,JLEV) = PSIDDHXL0(JLON,JLEV) &
                 & +(ZXIDT0*PXSI(JLON,JLEV)-PSIDDHXT9(JLON,JLEV))
        ENDDO
        ENDDO
        DO JLEV = 1, YRDIMV%NFLEVG
        DO JLON = KIDIA, KFDIA
        PSIDDHXT1(JLON,JLEV) = PSIDDHXT1(JLON,JLEV) &
                 &+PESGP*ZXIDT0*PXSI(JLON,JLEV)
        ENDDO
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JLEV = 1, YRDIMV%NFLEVG
        DO JLON = KIDIA, KFDIA
        PSIDDHXL0(JLON,JLEV) = PSIDDHXL0(JLON,JLEV) &
                 & +0.5_JPRB*PESGM*(ZXIDT0*PXSI(JLON,JLEV) &
                 & -PSIDDHXT9(JLON,JLEV))
        ENDDO
        ENDDO
        DO JLEV = 1, YRDIMV%NFLEVG
        DO JLON = KIDIA, KFDIA
        PSIDDHXT1(JLON,JLEV) = PSIDDHXT1(JLON,JLEV) &
                 & +PESGP*(1.5_JPRB*ZXIDT0*PXSI(JLON,JLEV) &
                 & -0.5_JPRB*PSIDDHXT9(JLON,JLEV))
        ENDDO
        ENDDO
      ENDIF
    ENDIF
    IF ( .NOT.LLNESC ) THEN
      ! save of semi-implicit linear term  at time t
      ! to be used at time t-dt next time step
      DO JLEV = 1, YRDIMV%NFLEVG
      DO JLON = KIDIA, KFDIA
      PSIDDHXT9(JLON,JLEV) = ZXIDT9*PXSI(JLON,JLEV)
      ENDDO
      ENDDO
    ENDIF
  ELSE
    IF (KXLAG >= 3) THEN
      DO JLEV = 1, YRDIMV%NFLEVG
      DO JLON = KIDIA, KFDIA
      PSIDDHXT1(JLON,JLEV) = PSIDDHXT1(JLON,JLEV) &
             & +PESGP*ZXIDT9*PXSI(JLON,JLEV)
      ENDDO
      ENDDO
    ENDIF
  ENDIF
ENDIF

!     ------------------------------------------------------------------



END SUBROUTINE LATTEX_DNT

