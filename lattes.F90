SUBROUTINE LATTES(YDGMV,KST,KPROF,PDTS2,PBDT,PESGP,PESGM,&
 & PVCRS0,PRDLR0,YDOROG,POROGL,POROGM,&
 & PSDIV0,PSDIV9,PSDVBC,PRES0,PGMVS,&
 & PGMV,PGMVT1S,PB1,PB2)

!**** *LATTES*   Semi-Lagrangian scheme.
!                Computation of the t and t-dt useful quantities
!                 at grid-points. Equations for bi-dimensional
!                 variables (continuity equation).

!     Purpose.
!     --------
!        * This subroutine computes the equation quantities to be
!          interpolated at each grid-point of the colocation grid.
!          The terms considered here are the explicit terms and
!          the explicit part of the semi-implicit scheme (terms
!          previously computed in LASSIE or LANHSI).
!          Equations considered here are equations for bi-dimensional
!          variables: continuity equation.
!        * Remark 1: when an alternate averaging is used for linear terms
!          in the 2TL SL scheme, the first timestep is treated differently
!          (first order uncentering), no uncentering is applied to the
!          total term ("cursive A") and the term saved in P[X]NLT9
!          is [ (Delta t/2) ("cursive A" - (1 + xidt) beta "cursive B") ]
!          instead of [ (Delta t/2) ("cursive A" - beta "cursive B") ].
!        * Remark 2: for lsettls=true, uncentering is applied to
!          the 'stable' extrapolation if vesl > 0 to avoid instability
!          in the momentum equation.
!        * Remark 3: for PC schemes:
!          - this routine is called for nsiter=0.
!          - this routine is called for the predictor of lpc_full.
!          - this routine is called for the corrector of lpc_full.

!**   Interface.
!     ----------
!        *CALL* *LATTES(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST         - first element of work.
!          KPROF       - depth of work.
!          PDTS2       - 0.5*"pdt", where "pdt" =
!                        time step for the first time-integration step of
!                        a leap-frog scheme or all time-integration steps of
!                        a two-time level scheme; 2*time step for the following
!                        time-integration steps of a leap-frog scheme.
!          PBDT        - zbt if semi-implicit scheme with unreduced
!                        divergence, zbt*(c**2/GM**2) if semi-implicit
!                        scheme with reduced divergence, where zbt
!                        is equal to PDTS2*BETADT (BETADT is in YOMDYN) or zero
!                        according to the value of configuration.
!          PESGP       - (1 + uncentering factor).
!          PESGM       - (1 - uncentering factor).
!          PVCRS0      - array containing quantities for LVERCOR=T (for ex. rs/a) at t.
!          PRDLR0      - array containing "r/a", "a/r", "grad(r/a)" at t.
!          YDOROG      - structure for orography
!          POROGL      - zonal component of "rssavnabla(surf orography)"
!          POROGM      - meridian component of "rssavnabla(surf orography)"
!          PSDIV0      - SI term at time t for continuity equation (Nu*D).
!          PSDIV9      - SI term at time t-dt for continuity equation (Nu*D).
!          PSDVBC      - vertical integral of divergence computed in "gpcty",
!                        including the "lrubc" and "delta m=1" contributions
!                        of (etadot d prehyd/d eta).
!          PRES0       - hydrostatic pressure at half levels at t.
!          PGMVS       - GMVS variables at t-dt and t.

!        INPUT/OUTPUT:
!          PGMV        - GMV variables at t-dt and t.
!          PGMVT1S     - GMVS variables at t+dt.
!          PB1         - "SLBUF1" buffer for interpolations.
!          PB2         - "SLBUF2" buffer.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!           none
!           Called by LACDYN.

!     Reference.
!     ----------
!             Arpege documentation about semi-Lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old parts 3.3 et 3.4 of LACDYN. 
!      Original : AUGUST 1995.

! Modifications
! -------------
!   J.Vivoda (03-2002)
!    - rationalization, PC schemes for NH dynamics (LPC_XXXX keys)
!   12-Oct-2002 J. Masek   PC bugfix
!   01-Oct-2003 M. Hamrud  CY28 Cleaning
!   09-Jun-2004 J. Masek   NH cleaning (LPC_NOTR, LFULLIMP)
!   01-Jul-2004 K. Yessad  Make clearer the tests for PC scheme.
!   23-Oct-2007 K. Yessad  Cleanings + (hidden) bugs corrections.
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad (Aug 2008): simplify XIDT treatment with PC + cleanings
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (Oct 2013): allow NESC without ICI-PC scheme.
!   K. Yessad (July 2014): Move some variables, rename some variables.
!   O. Marsden: June 2015 CY42 YRGMV, YRGFL, YRSURF, YRGMV5, and YRGFL5 are now passed by argument
! End Modifications
!------------------------------------------------------------------------------

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LTWOTL
USE YOMCT3   , ONLY : NSTEP
USE YOMCVER  , ONLY : LVERTFE, LVFE_INTB
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMVERT  , ONLY : YRVAB, YRVETA, YRVFE
USE YOMOROG  , ONLY : TOROG
USE YOMDYNA  , ONLY : LVERCOR, LRWSDLR, LPC_FULL, LPC_CHEAP, LNESC, LSETTLS, LSLINLC1
USE YOMDYN   , ONLY : YRDYN
USE YOMSTA   , ONLY : YRSTA
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE PAR_RDLR , ONLY : JP_ASRF, JP_DIMR0
USE INTDYN_MOD,ONLY : YYTVC0

!------------------------------------------------------------------------------

IMPLICIT NONE

TYPE(TGMV) , INTENT(INOUT) :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDTS2 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBDT(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PESGM 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
TYPE(TOROG)       ,INTENT(IN)    :: YDOROG
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGL(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGM(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDIV0(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDIV9(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDVBC(YRDIM%NPROMA,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRES0(YRDIM%NPROMA,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMVS(YRDIM%NPROMA,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(YRDIM%NPROMA,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1S(YRDIM%NPROMA,YDGMV%YT1%NDIMS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(YRDIM%NPROMA,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB2(YRDIM%NPROMA,YRPTRSLB2%NFLDSLB2)
!------------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZMOY1SP(YRDIM%NPROMA,YRDIMV%NFLEVG) 
REAL(KIND=JPRB) :: ZMOYSPNL(YRDIM%NPROMA),ZMOYSPNL_SI(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT0(YRDIM%NPROMA),ZSPNLT0_SI(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT1(YRDIM%NPROMA),ZSPNLT1_SI(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPNLT_FE(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSPTB(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPT1(YRDIM%NPROMA,YRDIMV%NFLEVG+1)
REAL(KIND=JPRB) :: ZVWEI(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZVWEIM(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZIN(YRDIM%NPROMA,YRDIMV%NFLEVG),ZOU(YRDIM%NPROMA,YRDIMV%NFLEVG+1)
REAL(KIND=JPRB) :: ZIN2(YRDIM%NPROMA,0:YRDIMV%NFLEVG+1)

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZCMSLP, ZXIGP
REAL(KIND=JPRB) :: ZXIDT0, ZXIDT9

LOGICAL :: LLCT, LLCTC


!------------------------------------------------------------------------------

#include "abor1.intfb.h"
!include "verint.intfb.h"

!------------------------------------------------------------------------------




!------------------------------------------------------------------------------

!############################################
! 1. AUXILIARITIES
!############################################

LLCT = LPC_FULL .AND. YRDYN%NCURRENT_ITER > 0 ! corrector for LPC_FULL
LLCTC = LPC_CHEAP .AND. YRDYN%NCURRENT_ITER > 0

ZXIDT0=1.0_JPRB+YRDYN%XIDT
ZXIDT9=1.0_JPRB+YRDYN%XIDT
ZXIGP=1.0_JPRB+YRDYN%XIDT

!------------------------------------------------------------------------------

!############################################
! 2. NONLINEAR MODEL
!############################################

ZCMSLP=YRDYN%RCMSLP0/(RD*YRSTA%RTSUR)
DO JROF=KST,KPROF
  ZSPTB(JROF)=PSDVBC(JROF,YRDIMV%NFLEVG)/PRES0(JROF,YRDIMV%NFLEVG)
ENDDO
IF (LVERCOR) THEN
  ! * the vertical integral of divergence
  !   has to be multiplied by "a**2/rs[surf]**2"
  DO JROF=KST,KPROF
    ZSPTB(JROF)=ZSPTB(JROF)*PVCRS0(JROF,YRDIMV%NFLEVG,YYTVC0%M_VCASRSH)*PVCRS0(JROF,YRDIMV%NFLEVG,YYTVC0%M_VCASRSH)
  ENDDO
ENDIF

IF (LVERCOR) THEN
! * [vertical weight]_l =
!   {[delta B]_l (rs**2/a**2)_l} / {sum_k=1_to_k=L [delta B]_k (rs**2/a**2)_k}
  ZVWEIM(KST:KPROF)=0.0_JPRB
  DO JLEV=1,YRDIMV%NFLEVG
    DO JROF=KST,KPROF
      ZVWEI(JROF,JLEV)=YRVAB%VDELB(JLEV)*PVCRS0(JROF,JLEV,YYTVC0%M_VCRSSAF)*PVCRS0(JROF,JLEV,YYTVC0%M_VCRSSAF)
      ZVWEIM(JROF)=ZVWEIM(JROF)+ZVWEI(JROF,JLEV)
    ENDDO
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JROF=KST,KPROF
      ZVWEI(JROF,JLEV)=ZVWEI(JROF,JLEV)/ZVWEIM(JROF)
    ENDDO
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JROF=KST,KPROF
      PB2(JROF,YRPTRSLB2%MSLB2VCVWEI+JLEV-1)=PB2(JROF,YRPTRSLB2%MSLB2VCVWEI+JLEV-1)+ZVWEI(JROF,JLEV)
    ENDDO
  ENDDO
ELSE
! * [vertical weight]_l = [delta B]_l
  DO JLEV=1,YRDIMV%NFLEVG
    ZVWEI(KST:KPROF,JLEV)=YRVAB%VDELB(JLEV)
  ENDDO
ENDIF

! linear model at time t

DO JROF=KST,KPROF
  PB2(JROF,YRPTRSLB2%MSLB2SPSI)=PBDT(JROF)*PSDIV0(JROF)
  ZSPTB(JROF)=-PDTS2*ZSPTB(JROF)
ENDDO

! nonlinear model at time t

DO JLEV=1,YRDIMV%NFLEVG
  DO JROF=KST,KPROF
    ZMOY1SP(JROF,JLEV)=PDTS2*&
     & (PGMV(JROF,JLEV,YDGMV%YT0%MU)*(PGMVS(JROF,YDGMV%YT0%MSPL)+ZCMSLP*POROGL(JROF))&
     & +PGMV(JROF,JLEV,YDGMV%YT0%MV)*(PGMVS(JROF,YDGMV%YT0%MSPM)+ZCMSLP*POROGM(JROF)))
  ENDDO
  IF (LVERCOR) THEN
    ! * multiply the semi-reduced derivatives by "a/rs".
    DO JROF=KST,KPROF
      ZMOY1SP(JROF,JLEV)=ZMOY1SP(JROF,JLEV)*PVCRS0(JROF,JLEV,YYTVC0%M_VCASRSF)
    ENDDO
  ENDIF
  IF (LRWSDLR) THEN
    ! * multiply the semi-reduced derivatives by "a/r".
    DO JROF=KST,KPROF
      ZMOY1SP(JROF,JLEV)=ZMOY1SP(JROF,JLEV)*PRDLR0(JROF,JLEV,JP_ASRF)
    ENDDO
  ENDIF
  DO JROF=KST,KPROF
    ZMOY1SP(JROF,JLEV)=ZMOY1SP(JROF,JLEV)+ZSPTB(JROF)
  ENDDO
ENDDO

!*       *   Continuity equation.

IF (LTWOTL) THEN

  IF(.NOT.LLCT)THEN

    !############################################
    ! Predictor for LPC_FULL
    ! or case nsiter=0.
    !############################################

    IF (LPC_CHEAP.AND.(.NOT.LNESC)) THEN
      ! LPC_CHEAP=T is currently coded for LNESC only
      !  (for LNESC=F interpolated quantities are not the same ones
      !  at the predictor and corrector steps, and the LPC_CHEAP code
      !  currently assumes that they are identical).
      CALL ABOR1('LATTES: If LPC_CHEAP=T, LNESC should be T')
    ENDIF

    !---------------------------------------------------------------
    ! 3D buffers 
    !---------------------------------------------------------------
    DO JLEV=1,YRDIMV%NFLEVG

      ! nonlinear residual time t
      IF (LSLINLC1) THEN
        DO JROF=KST,KPROF
          ZSPNLT0(JROF)=ZMOY1SP(JROF,JLEV)
          ZSPNLT0_SI(JROF)=ZXIDT0*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ELSE
        DO JROF=KST,KPROF
          ZSPNLT0(JROF)=ZMOY1SP(JROF,JLEV)+ZXIDT0*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ENDIF
            
      ! Fill PB1(.,MSLB1C9),PGMVT1S(.,YT1%MSP),ZSPNLT_FE:
      IF (YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3) THEN

        IF ( NSTEP <= 0 .OR. LNESC ) THEN

          DO JROF=KST,KPROF
            PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
             & +PESGM*ZMOY1SP(JROF,JLEV)
          ENDDO
          IF (LVERTFE) THEN
            DO JROF=KST,KPROF
              ZSPNLT_FE(JROF,JLEV)=ZSPNLT0(JROF) &
               & *ZVWEI(JROF,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
            IF (LSLINLC1) THEN
              DO JROF=KST,KPROF
                PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0_SI(JROF)
              ENDDO
            ENDIF
          ELSE
            DO JROF=KST,KPROF
              PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0(JROF)
            ENDDO
          ENDIF
 
        ELSEIF (LSETTLS) THEN

          IF (LSLINLC1) THEN
            DO JROF=KST,KPROF
              PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JROF,JLEV) &
               & +(ZSPNLT0(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL))
              PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
               & +(ZSPNLT0_SI(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL_SI))
            ENDDO
          ELSE
            DO JROF=KST,KPROF
              PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JROF,JLEV) &
               & +(ZSPNLT0(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL))
            ENDDO
          ENDIF
          IF( LVERTFE )THEN
            DO JROF=KST,KPROF
              ZSPNLT_FE(JROF,JLEV)=&
               & ZSPNLT0(JROF)*PESGP*ZVWEI(JROF,JLEV)*YRVETA%VFE_RDETAH(JLEV)
            ENDDO
            IF (LSLINLC1) THEN
              DO JROF=KST,KPROF
                PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0_SI(JROF)
              ENDDO
            ENDIF
          ELSE
            DO JROF=KST,KPROF
              PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JROF,JLEV)*ZSPNLT0(JROF)
            ENDDO
          ENDIF

        ELSE

          ! remaining case: ldsettls=false, lnesc=false.
          IF (LSLINLC1) THEN
            DO JROF=KST,KPROF
              PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JROF,JLEV) &
               & +0.5_JPRB*PESGM*(ZSPNLT0(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL))
              PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
               & +0.5_JPRB*PESGM*(ZSPNLT0_SI(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL_SI))
            ENDDO 
          ELSE
            DO JROF=KST,KPROF
              PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JROF,JLEV) &
               & +0.5_JPRB*PESGM*(ZSPNLT0(JROF)-PGMV(JROF,JLEV,YDGMV%YT9%MSPNL))
            ENDDO 
          ENDIF
          IF (LVERTFE) THEN
            DO JROF=KST,KPROF
              ZSPNLT_FE(JROF,JLEV)= &
               & (1.5_JPRB*ZSPNLT0(JROF)-0.5_JPRB*PGMV(JROF,JLEV,YDGMV%YT9%MSPNL)) &
               & *ZVWEI(JROF,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
            IF (LSLINLC1) THEN
              DO JROF=KST,KPROF
                PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JROF,JLEV)*(1.5_JPRB*ZSPNLT0_SI(JROF) &
                 & -0.5_JPRB*PGMV(JROF,JLEV,YDGMV%YT9%MSPNL_SI))
              ENDDO
            ENDIF
          ELSE
            DO JROF=KST,KPROF
              PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JROF,JLEV) &
               & *(1.5_JPRB*ZSPNLT0(JROF)-0.5_JPRB*PGMV(JROF,JLEV,YDGMV%YT9%MSPNL))
            ENDDO
          ENDIF

        ENDIF

      ENDIF

      ! Save quantities for corrector step
      IF( LPC_FULL )THEN
        ! save nonlinear model at time t
        PGMV(KST:KPROF,JLEV,YDGMV%YT9%MCSPNL)=ZMOY1SP(KST:KPROF,JLEV)
      ENDIF

      IF( .NOT.LNESC )THEN
        ! save of nonlinear residual at time t
        ! to be used as nonlinear residual at time t-dt next time step
        IF (LSLINLC1) THEN
          PGMV(KST:KPROF,JLEV,YDGMV%YT9%MSPNL)=ZMOY1SP(KST:KPROF,JLEV)
          PGMV(KST:KPROF,JLEV,YDGMV%YT9%MSPNL_SI)=ZXIDT9*PB2(KST:KPROF,YRPTRSLB2%MSLB2SPSI)
        ELSE
          DO JROF=KST,KPROF
            PGMV(JROF,JLEV,YDGMV%YT9%MSPNL)=ZMOY1SP(JROF,JLEV) &
             & +ZXIDT9*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
          ENDDO
        ENDIF
      ENDIF

    ENDDO

    !------------------------------------------------------------------------
    ! 3D additional actions
    !------------------------------------------------------------------------
    IF (LVERTFE) THEN
      IF(YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3)THEN
        IF( LVFE_INTB )THEN
          DO JROF=KST,KPROF
            ZIN2(JROF,0)=0.0_JPRB
            ZIN2(JROF,1:YRDIMV%NFLEVG)=ZSPNLT_FE(JROF,1:YRDIMV%NFLEVG)
            ZIN2(JROF,YRDIMV%NFLEVG+1)=0.0_JPRB
          ENDDO
!         CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
        ELSE
!         CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
        ENDIF
        DO JROF=KST,KPROF
          PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)+ZSPT1(JROF,YRDIMV%NFLEVG+1)
        ENDDO
      ENDIF
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF(YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3)THEN
      DO JROF=KST,KPROF
        PB1(JROF,YRPTRSLB1%MSLB1SP9)=PB1(JROF,YRPTRSLB1%MSLB1SP9)+PGMVS(JROF,YDGMV%YT0%MSP) &
         & +ZCMSLP*YDOROG%OROG(JROF)
        PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JROF)
      ENDDO
    ENDIF

  ELSE

    !############################################
    ! Corrector for LPC_FULL
    !############################################
    !---------------------------------------------------------------
    ! 3D buffers 
    !---------------------------------------------------------------
    DO JLEV=1,YRDIMV%NFLEVG

      ! nonlinear residual time t

      IF (LSLINLC1) THEN
        ZSPNLT1(KST:KPROF)=ZMOY1SP(KST:KPROF,JLEV)
        ZSPNLT1_SI(KST:KPROF)=ZXIDT9*PB2(KST:KPROF,YRPTRSLB2%MSLB2SPSI)
      ELSE
        DO JROF=KST,KPROF
          ZSPNLT1(JROF)=ZMOY1SP(JROF,JLEV)+ZXIDT9*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ENDIF

      IF (.NOT.LLCTC) THEN
        DO JROF=KST,KPROF
          PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
           & +PESGM*PGMV(JROF,JLEV,YDGMV%YT9%MCSPNL)
        ENDDO
      ENDIF
      IF (LVERTFE) THEN
        DO JROF=KST,KPROF
          ZSPNLT_FE(JROF,JLEV)=ZSPNLT1(JROF) &
           & *ZVWEI(JROF,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
        ENDDO
        IF (LSLINLC1) THEN
          DO JROF=KST,KPROF
            PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
             & +ZVWEI(JROF,JLEV)*ZSPNLT1_SI(JROF)*PESGP
          ENDDO
        ENDIF
      ELSE
        DO JROF=KST,KPROF
          PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
           & +ZVWEI(JROF,JLEV)*ZSPNLT1(JROF)*PESGP
        ENDDO
      ENDIF

    ENDDO

    !------------------------------------------------------------------------
    ! 3D additional actions
    !------------------------------------------------------------------------
    IF (LVERTFE) THEN
      IF( LVFE_INTB )THEN
        DO JROF=KST,KPROF
          ZIN2(JROF,0)=0.0_JPRB
          ZIN2(JROF,1:YRDIMV%NFLEVG)=ZSPNLT_FE(JROF,1:YRDIMV%NFLEVG)
          ZIN2(JROF,YRDIMV%NFLEVG+1)=0.0_JPRB
        ENDDO
!       CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
      ELSE
!       CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
      ENDIF
      DO JROF=KST,KPROF
        PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)+ZSPT1(JROF,YRDIMV%NFLEVG+1)
      ENDDO
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF (.NOT.LLCTC) THEN
      DO JROF=KST,KPROF
        PB1(JROF,YRPTRSLB1%MSLB1SP9)=PB1(JROF,YRPTRSLB1%MSLB1SP9)+PGMVS(JROF,YDGMV%YT9%MSP) &
         & +ZCMSLP*YDOROG%OROG(JROF)
      ENDDO
    ENDIF
    DO JROF=KST,KPROF
      PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JROF)
    ENDDO

  ENDIF

  ! add preliminary quantity for LAPGHY physics

  IF( YRDYN%XIDT > 0.0_JPRB )THEN
    DO JROF=KST,KPROF
      PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)-ZXIGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
      PB2(JROF,YRPTRSLB2%MSLB2SPSI)=ZXIGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
    ENDDO
  ELSE
    DO JROF=KST,KPROF
      PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)-PESGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
      PB2(JROF,YRPTRSLB2%MSLB2SPSI)=PESGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
    ENDDO
  ENDIF

ELSE

  IF(YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3)THEN

    IF(LVERTFE) THEN
      DO JLEV=1,YRDIMV%NFLEVG
        IF (LSLINLC1) THEN
          DO JROF=KST,KPROF
            ZMOYSPNL(JROF)=ZMOY1SP(JROF,JLEV)
            ZMOYSPNL_SI(JROF)=PB2(JROF,YRPTRSLB2%MSLB2SPSI)
            PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
             & +PESGM*ZMOYSPNL(JROF)-PESGM*PBDT(JROF)*PSDIV9(JROF)
            PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
             & +PESGM*ZMOYSPNL_SI(JROF)
            ZSPNLT_FE(JROF,JLEV)=PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL(JROF)
            PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
             & +PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL_SI(JROF)
          ENDDO
        ELSE
          DO JROF=KST,KPROF
            ZMOYSPNL(JROF)=ZMOY1SP(JROF,JLEV)+PB2(JROF,YRPTRSLB2%MSLB2SPSI)
            PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
             & +PESGM*ZMOYSPNL(JROF)-PESGM*PBDT(JROF)*PSDIV9(JROF)
            ZSPNLT_FE(JROF,JLEV)=PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL(JROF)
          ENDDO
        ENDIF
      ENDDO
      IF( LVFE_INTB )THEN
        DO JROF=KST,KPROF
          ZIN2(JROF,0)=0.0_JPRB
          ZIN2(JROF,1:YRDIMV%NFLEVG)=ZSPNLT_FE(JROF,1:YRDIMV%NFLEVG)
          ZIN2(JROF,YRDIMV%NFLEVG+1)=0.0_JPRB
        ENDDO
!       CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
      ELSE
!       CALL VERINT(YRDIM%NPROMA,KST,KPROF,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
      ENDIF
      DO JROF=KST,KPROF
        PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)+ZSPT1(JROF,YRDIMV%NFLEVG+1)
      ENDDO
    ELSE
      DO JLEV=1,YRDIMV%NFLEVG
        DO JROF=KST,KPROF
          ZMOYSPNL(JROF)=ZMOY1SP(JROF,JLEV)+PB2(JROF,YRPTRSLB2%MSLB2SPSI)
          PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JROF,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
           & +PESGM*ZMOYSPNL(JROF)-PESGM*PBDT(JROF)*PSDIV9(JROF)
          PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP) &
           & +PESGP*ZVWEI(JROF,JLEV)*ZMOYSPNL(JROF)
        ENDDO
      ENDDO
    ENDIF

    DO JROF=KST,KPROF
      PB1(JROF,YRPTRSLB1%MSLB1SP9)=PB1(JROF,YRPTRSLB1%MSLB1SP9)+PGMVS(JROF,YDGMV%YT9%MSP) &
       & +ZCMSLP*YDOROG%OROG(JROF)
      PGMVT1S(JROF,YDGMV%YT1%MSP)=PGMVT1S(JROF,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JROF) &
       & -PESGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
      PB2(JROF,YRPTRSLB2%MSLB2SPSI)=PESGP*PB2(JROF,YRPTRSLB2%MSLB2SPSI)
    ENDDO

  ENDIF

ENDIF

!------------------------------------------------------------------------------



END SUBROUTINE LATTES

