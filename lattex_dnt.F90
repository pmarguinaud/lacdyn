SUBROUTINE LATTEX_DNT(KLON, KIDIA,KFDIA,LDSETTLS,KXLAG,PESGP,PESGM,PXT0,PXT9,PMOY1X,&
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

INTEGER(KIND=JPIM) :: JLEV,JROF

!     * ZXNLT0 (resp. ZXNLT1) the non linear term at t (resp. t+dt).
REAL(KIND=JPRB) :: ZXNLT1(KLON)
REAL(KIND=JPRB) :: ZXNLT0(KLON)
REAL(KIND=JPRB) :: ZXIDT0,ZXIDT9,ZXIGP

LOGICAL :: LLCT,LLNESC



!     ------------------------------------------------------------------

#include "abor1.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*      1. AUXILIARY VARIABLES.
!       -----------------------

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
    DO JROF=KIDIA,KFDIA
      ZXNLT0(JROF)=PMOY1X(JROF,JLEV)+ZXIDT0*PXSI(JROF,JLEV)
    ENDDO

    ! Fill PXL9,PXLF9,PXL0,PXT1.
    IF ((KXLAG == 2).AND.(YRDYN%NSPLTHOI /= 0)) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV)
          PXLF9(JROF,JLEV)=PXLF9(JROF,JLEV)+PESGM*PMOY1X(JROF,JLEV)
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV)
          PXLF9(JROF,JLEV)=PXLF9(JROF,JLEV) &
           & +PESGM*PMOY1X(JROF,JLEV)+(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV)
          PXLF9(JROF,JLEV)=PXLF9(JROF,JLEV)+PESGM*PMOY1X(JROF,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JROF)-0.5_JPRB*PXNLT9(JROF,JLEV))
        ENDDO
      ENDIF
    ELSEIF ((KXLAG == 2).AND.(YRDYN%NSPLTHOI == 0)) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV) &
           & +PESGM*PMOY1X(JROF,JLEV)
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV) &
           & +PESGM*PMOY1X(JROF,JLEV)+(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JROF=KIDIA,KFDIA
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV) &
           & +PESGM*PMOY1X(JROF,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JROF)-0.5_JPRB*PXNLT9(JROF,JLEV))
        ENDDO
      ENDIF
    ELSEIF (KXLAG >= 3) THEN
      IF (NSTEP <= 0 .OR. LLNESC) THEN
        DO JROF=KIDIA,KFDIA
          PXL0(JROF,JLEV)=PXL0(JROF,JLEV)+PESGM*PMOY1X(JROF,JLEV)
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSEIF (LDSETTLS) THEN
        DO JROF=KIDIA,KFDIA
          PXL0(JROF,JLEV)=PXL0(JROF,JLEV)+PESGM*PMOY1X(JROF,JLEV) &
           & +(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT0(JROF)
        ENDDO
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        DO JROF=KIDIA,KFDIA
          PXL0(JROF,JLEV)=PXL0(JROF,JLEV)+PESGM*PMOY1X(JROF,JLEV) &
           & +0.5_JPRB*PESGM*(ZXNLT0(JROF)-PXNLT9(JROF,JLEV))
          PXT1(JROF,JLEV)=PXT1(JROF,JLEV) &
           & +PESGP*(1.5_JPRB*ZXNLT0(JROF)-0.5_JPRB*PXNLT9(JROF,JLEV))
        ENDDO
      ENDIF
      DO JROF=KIDIA,KFDIA
        PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT0(JROF,JLEV)
      ENDDO
    ENDIF

    ! save quantities for corrector step
    IF( LPC_FULL )THEN
      ! save nonlinear model at time t
      PCXNLT9(KIDIA:KFDIA,JLEV)=PMOY1X(KIDIA:KFDIA,JLEV)
    ENDIF

    IF( .NOT.LLNESC )THEN
      ! save of nonlinear residual at time t
      ! to be used as nonlinear residual at time t-dt next time step
      DO JROF=KIDIA,KFDIA
        PXNLT9(JROF,JLEV)=PMOY1X(JROF,JLEV)+ZXIDT9*PXSI(JROF,JLEV)
      ENDDO
    ENDIF

  ENDDO

ELSE

  !############################################
  ! 2.2 Corrector for LPC_FULL
  !############################################

  DO JLEV=1,YRDIMV%NFLEVG

    ! nonlinear residual time t+dt (preliminary state from predictor)

    DO JROF=KIDIA,KFDIA
      ZXNLT1(JROF)=PMOY1X(JROF,JLEV)+ZXIDT9*PXSI(JROF,JLEV)
    ENDDO

    IF (KXLAG == 2) THEN
      IF (.NOT. LPC_CHEAP) THEN
        IF (YRDYN%NSPLTHOI /= 0) THEN
          DO JROF=KIDIA,KFDIA
            PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT9(JROF,JLEV)
            PXLF9(JROF,JLEV)=PXLF9(JROF,JLEV) &
             & +PESGM*PCXNLT9(JROF,JLEV)
          ENDDO
        ELSE
          DO JROF=KIDIA,KFDIA
            PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT9(JROF,JLEV) &
             & +PESGM*PCXNLT9(JROF,JLEV)
          ENDDO
        ENDIF
      ENDIF
      DO JROF=KIDIA,KFDIA
        PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT1(JROF)
      ENDDO
    ELSEIF (KXLAG >= 3) THEN
      IF (.NOT. LPC_CHEAP) THEN
        DO JROF=KIDIA,KFDIA
          PXL0(JROF,JLEV)=PXL0(JROF,JLEV)+PESGM*PCXNLT9(JROF,JLEV)
          PXL9(JROF,JLEV)=PXL9(JROF,JLEV)+PXT9(JROF,JLEV)
        ENDDO
      ENDIF
      DO JROF=KIDIA,KFDIA
        PXT1(JROF,JLEV)=PXT1(JROF,JLEV)+PESGP*ZXNLT1(JROF)
      ENDDO
    ENDIF

  ENDDO

ENDIF

!########################################################
! 2.3 Addition of preliminary quantity for LAGPHY physics
!########################################################

DO JLEV=1,YRDIMV%NFLEVG
  IF(YRDYN%XIDT > 0.0_JPRB)THEN
    DO JROF=KIDIA,KFDIA
      PXT1(JROF,JLEV)=PXT1(JROF,JLEV)-ZXIGP*PXSI(JROF,JLEV)
      PXSI(JROF,JLEV)=ZXIGP*PXSI(JROF,JLEV)
    ENDDO
  ELSE
    DO JROF=KIDIA,KFDIA
      PXT1(JROF,JLEV)=PXT1(JROF,JLEV)-PESGP*PXSI(JROF,JLEV)
      PXSI(JROF,JLEV)=PESGP*PXSI(JROF,JLEV)
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
        PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & +PESGP*ZXIDT0*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG)
      ELSEIF (LDSETTLS) THEN
        PSIDDHXL0(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXL0(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & +(ZXIDT0*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG)-PSIDDHXT9(KIDIA:KFDIA,1:YRDIMV%NFLEVG))
        PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         &+PESGP*ZXIDT0*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG)
      ELSE
        ! * remaining case: ldsettls=false, lnesc=false.
        PSIDDHXL0(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXL0(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & +0.5_JPRB*PESGM*(ZXIDT0*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & -PSIDDHXT9(KIDIA:KFDIA,1:YRDIMV%NFLEVG))
        PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & +PESGP*(1.5_JPRB*ZXIDT0*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
         & -0.5_JPRB*PSIDDHXT9(KIDIA:KFDIA,1:YRDIMV%NFLEVG))
      ENDIF
    ENDIF
    IF ( .NOT.LLNESC ) THEN
      ! save of semi-implicit linear term  at time t
      ! to be used at time t-dt next time step
      PSIDDHXT9(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=ZXIDT9*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG)
    ENDIF
  ELSE
    IF (KXLAG >= 3) THEN
      PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG)=PSIDDHXT1(KIDIA:KFDIA,1:YRDIMV%NFLEVG) &
       & +PESGP*ZXIDT9*PXSI(KIDIA:KFDIA,1:YRDIMV%NFLEVG)
    ENDIF
  ENDIF
ENDIF

!     ------------------------------------------------------------------



END SUBROUTINE LATTEX_DNT

