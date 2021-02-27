SUBROUTINE LATTES(YRVFE, YRVETA, YRVAB, YRSTA, YRDYN, YRDIMV, YRDIM, &
 & KLON, YDGMV,KIDIA,KFDIA,PDTS2,PBDT,PESGP,PESGM,&
 & PVCRS0,PRDLR0,YDOROG,POROGL,POROGM,&
 & PSDIV0,PSDIV9,PSDVBC,PRES0,PGMVS,&
 & PGMV,PGMVT1S,PB1,PB2,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

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
!          KIDIA         - first element of work.
!          KFDIA       - depth of work.
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

!------------------------------------------------------------------------------

IMPLICIT NONE


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
!------------------------------------------------------------------------------
 
temp (REAL(KIND=JPRB), ZMOY1SP, (KLON,YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZMOYSPNL_SI, (KLON))
temp (REAL(KIND=JPRB), ZMOYSPNL, (KLON))

temp (REAL(KIND=JPRB), ZSPNLT0_SI, (KLON))
temp (REAL(KIND=JPRB), ZSPNLT0, (KLON))

temp (REAL(KIND=JPRB), ZSPNLT1_SI, (KLON))
temp (REAL(KIND=JPRB), ZSPNLT1, (KLON))

temp (REAL(KIND=JPRB), ZSPNLT_FE, (KLON,YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZSPTB, (KLON))

temp (REAL(KIND=JPRB), ZSPT1, (KLON,YRDIMV%NFLEVG+1))

temp (REAL(KIND=JPRB), ZVWEI, (KLON,YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZVWEIM, (KLON))

temp (REAL(KIND=JPRB), ZOU, (KLON,YRDIMV%NFLEVG+1))
temp (REAL(KIND=JPRB), ZIN, (KLON,YRDIMV%NFLEVG))


temp (REAL(KIND=JPRB), ZIN2, (KLON,0:YRDIMV%NFLEVG+1))
INTEGER(KIND=JPIM) :: JLEV, JLON

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

init_stack ()

alloc (ZMOY1SP)
alloc (ZMOYSPNL)
alloc (ZMOYSPNL_SI)
alloc (ZSPNLT0)
alloc (ZSPNLT0_SI)
alloc (ZSPNLT1)
alloc (ZSPNLT1_SI)
alloc (ZSPNLT_FE)
alloc (ZSPTB)
alloc (ZSPT1)
alloc (ZVWEI)
alloc (ZVWEIM)
alloc (ZIN)
alloc (ZOU)
alloc (ZIN2)



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
DO JLON=KIDIA,KFDIA
  ZSPTB(JLON)=PSDVBC(JLON,YRDIMV%NFLEVG)/PRES0(JLON,YRDIMV%NFLEVG)
ENDDO
IF (LVERCOR) THEN
  ! * the vertical integral of divergence
  !   has to be multiplied by "a**2/rs[surf]**2"
  DO JLON=KIDIA,KFDIA
    ZSPTB(JLON)=ZSPTB(JLON)*PVCRS0(JLON,YRDIMV%NFLEVG,YYTVC0%M_VCASRSH)*PVCRS0(JLON,YRDIMV%NFLEVG,YYTVC0%M_VCASRSH)
  ENDDO
ENDIF

IF (LVERCOR) THEN
! * [vertical weight]_l =
!   {[delta B]_l (rs**2/a**2)_l} / {sum_k=1_to_k=L [delta B]_k (rs**2/a**2)_k}
  DO JLON = KIDIA, KFDIA
  ZVWEIM(JLON) = 0.0_JPRB
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON=KIDIA,KFDIA
      ZVWEI(JLON,JLEV)=YRVAB%VDELB(JLEV)*PVCRS0(JLON,JLEV,YYTVC0%M_VCRSSAF)*PVCRS0(JLON,JLEV,YYTVC0%M_VCRSSAF)
      ZVWEIM(JLON)=ZVWEIM(JLON)+ZVWEI(JLON,JLEV)
    ENDDO
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON=KIDIA,KFDIA
      ZVWEI(JLON,JLEV)=ZVWEI(JLON,JLEV)/ZVWEIM(JLON)
    ENDDO
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON=KIDIA,KFDIA
      PB2(JLON,YRPTRSLB2%MSLB2VCVWEI+JLEV-1)=PB2(JLON,YRPTRSLB2%MSLB2VCVWEI+JLEV-1)+ZVWEI(JLON,JLEV)
    ENDDO
  ENDDO
ELSE
! * [vertical weight]_l = [delta B]_l
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON = KIDIA, KFDIA
  ZVWEI(JLON,JLEV) = YRVAB%VDELB(JLEV)
  ENDDO
  ENDDO
ENDIF

! linear model at time t

DO JLON=KIDIA,KFDIA
  PB2(JLON,YRPTRSLB2%MSLB2SPSI)=PBDT(JLON)*PSDIV0(JLON)
  ZSPTB(JLON)=-PDTS2*ZSPTB(JLON)
ENDDO

! nonlinear model at time t

DO JLEV=1,YRDIMV%NFLEVG
  DO JLON=KIDIA,KFDIA
    ZMOY1SP(JLON,JLEV)=PDTS2*&
     & (PGMV(JLON,JLEV,YDGMV%YT0%MU)*(PGMVS(JLON,YDGMV%YT0%MSPL)+ZCMSLP*POROGL(JLON))&
     & +PGMV(JLON,JLEV,YDGMV%YT0%MV)*(PGMVS(JLON,YDGMV%YT0%MSPM)+ZCMSLP*POROGM(JLON)))
  ENDDO
  IF (LVERCOR) THEN
    ! * multiply the semi-reduced derivatives by "a/rs".
    DO JLON=KIDIA,KFDIA
      ZMOY1SP(JLON,JLEV)=ZMOY1SP(JLON,JLEV)*PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF)
    ENDDO
  ENDIF
  IF (LRWSDLR) THEN
    ! * multiply the semi-reduced derivatives by "a/r".
    DO JLON=KIDIA,KFDIA
      ZMOY1SP(JLON,JLEV)=ZMOY1SP(JLON,JLEV)*PRDLR0(JLON,JLEV,JP_ASRF)
    ENDDO
  ENDIF
  DO JLON=KIDIA,KFDIA
    ZMOY1SP(JLON,JLEV)=ZMOY1SP(JLON,JLEV)+ZSPTB(JLON)
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
        DO JLON=KIDIA,KFDIA
          ZSPNLT0(JLON)=ZMOY1SP(JLON,JLEV)
          ZSPNLT0_SI(JLON)=ZXIDT0*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ELSE
        DO JLON=KIDIA,KFDIA
          ZSPNLT0(JLON)=ZMOY1SP(JLON,JLEV)+ZXIDT0*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ENDIF
            
      ! Fill PB1(.,MSLB1C9),PGMVT1S(.,YT1%MSP),ZSPNLT_FE:
      IF (YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3) THEN

        IF ( NSTEP <= 0 .OR. LNESC ) THEN

          DO JLON=KIDIA,KFDIA
            PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
             & +PESGM*ZMOY1SP(JLON,JLEV)
          ENDDO
          IF (LVERTFE) THEN
            DO JLON=KIDIA,KFDIA
              ZSPNLT_FE(JLON,JLEV)=ZSPNLT0(JLON) &
               & *ZVWEI(JLON,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
            IF (LSLINLC1) THEN
              DO JLON=KIDIA,KFDIA
                PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JLON,JLEV)*ZSPNLT0_SI(JLON)
              ENDDO
            ENDIF
          ELSE
            DO JLON=KIDIA,KFDIA
              PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JLON,JLEV)*ZSPNLT0(JLON)
            ENDDO
          ENDIF
 
        ELSEIF (LSETTLS) THEN

          IF (LSLINLC1) THEN
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JLON,JLEV) &
               & +(ZSPNLT0(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL))
              PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
               & +(ZSPNLT0_SI(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL_SI))
            ENDDO
          ELSE
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JLON,JLEV) &
               & +(ZSPNLT0(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL))
            ENDDO
          ENDIF
          IF( LVERTFE )THEN
            DO JLON=KIDIA,KFDIA
              ZSPNLT_FE(JLON,JLEV)=&
               & ZSPNLT0(JLON)*PESGP*ZVWEI(JLON,JLEV)*YRVETA%VFE_RDETAH(JLEV)
            ENDDO
            IF (LSLINLC1) THEN
              DO JLON=KIDIA,KFDIA
                PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JLON,JLEV)*ZSPNLT0_SI(JLON)
              ENDDO
            ENDIF
          ELSE
            DO JLON=KIDIA,KFDIA
              PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JLON,JLEV)*ZSPNLT0(JLON)
            ENDDO
          ENDIF

        ELSE

          ! remaining case: ldsettls=false, lnesc=false.
          IF (LSLINLC1) THEN
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JLON,JLEV) &
               & +0.5_JPRB*PESGM*(ZSPNLT0(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL))
              PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
               & +0.5_JPRB*PESGM*(ZSPNLT0_SI(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL_SI))
            ENDDO 
          ELSE
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
               & +PESGM*ZMOY1SP(JLON,JLEV) &
               & +0.5_JPRB*PESGM*(ZSPNLT0(JLON)-PGMV(JLON,JLEV,YDGMV%YT9%MSPNL))
            ENDDO 
          ENDIF
          IF (LVERTFE) THEN
            DO JLON=KIDIA,KFDIA
              ZSPNLT_FE(JLON,JLEV)= &
               & (1.5_JPRB*ZSPNLT0(JLON)-0.5_JPRB*PGMV(JLON,JLEV,YDGMV%YT9%MSPNL)) &
               & *ZVWEI(JLON,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
            ENDDO
            IF (LSLINLC1) THEN
              DO JLON=KIDIA,KFDIA
                PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
                 & +PESGP*ZVWEI(JLON,JLEV)*(1.5_JPRB*ZSPNLT0_SI(JLON) &
                 & -0.5_JPRB*PGMV(JLON,JLEV,YDGMV%YT9%MSPNL_SI))
              ENDDO
            ENDIF
          ELSE
            DO JLON=KIDIA,KFDIA
              PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
               & +PESGP*ZVWEI(JLON,JLEV) &
               & *(1.5_JPRB*ZSPNLT0(JLON)-0.5_JPRB*PGMV(JLON,JLEV,YDGMV%YT9%MSPNL))
            ENDDO
          ENDIF

        ENDIF

      ENDIF

      ! Save quantities for corrector step
      IF( LPC_FULL )THEN
        ! save nonlinear model at time t
        DO JLON = KIDIA, KFDIA
        PGMV(JLON,JLEV,YDGMV%YT9%MCSPNL) = ZMOY1SP(JLON,JLEV)
        ENDDO
      ENDIF

      IF( .NOT.LNESC )THEN
        ! save of nonlinear residual at time t
        ! to be used as nonlinear residual at time t-dt next time step
        IF (LSLINLC1) THEN
          DO JLON = KIDIA, KFDIA
          PGMV(JLON,JLEV,YDGMV%YT9%MSPNL) = ZMOY1SP(JLON,JLEV)
          ENDDO
          DO JLON = KIDIA, KFDIA
          PGMV(JLON,JLEV,YDGMV%YT9%MSPNL_SI) = ZXIDT9*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
          ENDDO
        ELSE
          DO JLON=KIDIA,KFDIA
            PGMV(JLON,JLEV,YDGMV%YT9%MSPNL)=ZMOY1SP(JLON,JLEV) &
             & +ZXIDT9*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
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
          DO JLON=KIDIA,KFDIA
            ZIN2(JLON,0)=0.0_JPRB
            DO JLEV = 1, YRDIMV%NFLEVG
            ZIN2(JLON,JLEV) = ZSPNLT_FE(JLON,JLEV)
            ENDDO
            ZIN2(JLON,YRDIMV%NFLEVG+1)=0.0_JPRB
          ENDDO
!         CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
        ELSE
!         CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
        ENDIF
        DO JLON=KIDIA,KFDIA
          PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)+ZSPT1(JLON,YRDIMV%NFLEVG+1)
        ENDDO
      ENDIF
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF(YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3)THEN
      DO JLON=KIDIA,KFDIA
        PB1(JLON,YRPTRSLB1%MSLB1SP9)=PB1(JLON,YRPTRSLB1%MSLB1SP9)+PGMVS(JLON,YDGMV%YT0%MSP) &
         & +ZCMSLP*YDOROG%OROG(JLON)
        PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JLON)
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
        DO JLON = KIDIA, KFDIA
        ZSPNLT1(JLON) = ZMOY1SP(JLON,JLEV)
        ENDDO
        DO JLON = KIDIA, KFDIA
        ZSPNLT1_SI(JLON) = ZXIDT9*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ELSE
        DO JLON=KIDIA,KFDIA
          ZSPNLT1(JLON)=ZMOY1SP(JLON,JLEV)+ZXIDT9*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
        ENDDO
      ENDIF

      IF (.NOT.LLCTC) THEN
        DO JLON=KIDIA,KFDIA
          PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA) &
           & +PESGM*PGMV(JLON,JLEV,YDGMV%YT9%MCSPNL)
        ENDDO
      ENDIF
      IF (LVERTFE) THEN
        DO JLON=KIDIA,KFDIA
          ZSPNLT_FE(JLON,JLEV)=ZSPNLT1(JLON) &
           & *ZVWEI(JLON,JLEV)*YRVETA%VFE_RDETAH(JLEV)*PESGP
        ENDDO
        IF (LSLINLC1) THEN
          DO JLON=KIDIA,KFDIA
            PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
             & +ZVWEI(JLON,JLEV)*ZSPNLT1_SI(JLON)*PESGP
          ENDDO
        ENDIF
      ELSE
        DO JLON=KIDIA,KFDIA
          PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
           & +ZVWEI(JLON,JLEV)*ZSPNLT1(JLON)*PESGP
        ENDDO
      ENDIF

    ENDDO

    !------------------------------------------------------------------------
    ! 3D additional actions
    !------------------------------------------------------------------------
    IF (LVERTFE) THEN
      IF( LVFE_INTB )THEN
        DO JLON=KIDIA,KFDIA
          ZIN2(JLON,0)=0.0_JPRB
          DO JLEV = 1, YRDIMV%NFLEVG
          ZIN2(JLON,JLEV) = ZSPNLT_FE(JLON,JLEV)
          ENDDO
          ZIN2(JLON,YRDIMV%NFLEVG+1)=0.0_JPRB
        ENDDO
!       CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
      ELSE
!       CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
      ENDIF
      DO JLON=KIDIA,KFDIA
        PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)+ZSPT1(JLON,YRDIMV%NFLEVG+1)
      ENDDO
    ENDIF

    !------------------------------------------------------------------------
    ! 2D buffers
    !------------------------------------------------------------------------
    IF (.NOT.LLCTC) THEN
      DO JLON=KIDIA,KFDIA
        PB1(JLON,YRPTRSLB1%MSLB1SP9)=PB1(JLON,YRPTRSLB1%MSLB1SP9)+PGMVS(JLON,YDGMV%YT9%MSP) &
         & +ZCMSLP*YDOROG%OROG(JLON)
      ENDDO
    ENDIF
    DO JLON=KIDIA,KFDIA
      PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JLON)
    ENDDO

  ENDIF

  ! add preliminary quantity for LAPGHY physics

  IF( YRDYN%XIDT > 0.0_JPRB )THEN
    DO JLON=KIDIA,KFDIA
      PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)-ZXIGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
      PB2(JLON,YRPTRSLB2%MSLB2SPSI)=ZXIGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
    ENDDO
  ELSE
    DO JLON=KIDIA,KFDIA
      PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)-PESGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
      PB2(JLON,YRPTRSLB2%MSLB2SPSI)=PESGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
    ENDDO
  ENDIF

ELSE

  IF(YRDYN%NVLAG == 2 .OR. YRDYN%NVLAG == 3)THEN

    IF(LVERTFE) THEN
      DO JLEV=1,YRDIMV%NFLEVG
        IF (LSLINLC1) THEN
          DO JLON=KIDIA,KFDIA
            ZMOYSPNL(JLON)=ZMOY1SP(JLON,JLEV)
            ZMOYSPNL_SI(JLON)=PB2(JLON,YRPTRSLB2%MSLB2SPSI)
            PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
             & +PESGM*ZMOYSPNL(JLON)-PESGM*PBDT(JLON)*PSDIV9(JLON)
            PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9_SI+JLEV-YRDIMV%NFLSA) &
             & +PESGM*ZMOYSPNL_SI(JLON)
            ZSPNLT_FE(JLON,JLEV)=PESGP*ZVWEI(JLON,JLEV)*ZMOYSPNL(JLON)
            PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
             & +PESGP*ZVWEI(JLON,JLEV)*ZMOYSPNL_SI(JLON)
          ENDDO
        ELSE
          DO JLON=KIDIA,KFDIA
            ZMOYSPNL(JLON)=ZMOY1SP(JLON,JLEV)+PB2(JLON,YRPTRSLB2%MSLB2SPSI)
            PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
             & +PESGM*ZMOYSPNL(JLON)-PESGM*PBDT(JLON)*PSDIV9(JLON)
            ZSPNLT_FE(JLON,JLEV)=PESGP*ZVWEI(JLON,JLEV)*ZMOYSPNL(JLON)
          ENDDO
        ENDIF
      ENDDO
      IF( LVFE_INTB )THEN
        DO JLON=KIDIA,KFDIA
          ZIN2(JLON,0)=0.0_JPRB
          DO JLEV = 1, YRDIMV%NFLEVG
          ZIN2(JLON,JLEV) = ZSPNLT_FE(JLON,JLEV)
          ENDDO
          ZIN2(JLON,YRDIMV%NFLEVG+1)=0.0_JPRB
        ENDDO
!       CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG+2,YRDIMV%NFLEVG+1,YRVFE%RINTBF11,ZIN2,ZSPT1,0)
      ELSE
!       CALL VERINT(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG,YRDIMV%NFLEVG+1,YRVFE%RINTE,ZSPNLT_FE,ZSPT1,0)
      ENDIF
      DO JLON=KIDIA,KFDIA
        PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)+ZSPT1(JLON,YRDIMV%NFLEVG+1)
      ENDDO
    ELSE
      DO JLEV=1,YRDIMV%NFLEVG
        DO JLON=KIDIA,KFDIA
          ZMOYSPNL(JLON)=ZMOY1SP(JLON,JLEV)+PB2(JLON,YRPTRSLB2%MSLB2SPSI)
          PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1C9+JLEV-YRDIMV%NFLSA)&
           & +PESGM*ZMOYSPNL(JLON)-PESGM*PBDT(JLON)*PSDIV9(JLON)
          PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP) &
           & +PESGP*ZVWEI(JLON,JLEV)*ZMOYSPNL(JLON)
        ENDDO
      ENDDO
    ENDIF

    DO JLON=KIDIA,KFDIA
      PB1(JLON,YRPTRSLB1%MSLB1SP9)=PB1(JLON,YRPTRSLB1%MSLB1SP9)+PGMVS(JLON,YDGMV%YT9%MSP) &
       & +ZCMSLP*YDOROG%OROG(JLON)
      PGMVT1S(JLON,YDGMV%YT1%MSP)=PGMVT1S(JLON,YDGMV%YT1%MSP)-ZCMSLP*YDOROG%OROG(JLON) &
       & -PESGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
      PB2(JLON,YRPTRSLB2%MSLB2SPSI)=PESGP*PB2(JLON,YRPTRSLB2%MSLB2SPSI)
    ENDDO

  ENDIF

ENDIF

!------------------------------------------------------------------------------



END SUBROUTINE LATTES


