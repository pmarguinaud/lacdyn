SUBROUTINE LATTEX(KLON, YDGMV, &
 ! --- INPUT --------------------------------------------------
 & KIDIA,KFDIA,PDTS2,PBT,PBDT,PESGP,PESGM,&
 & YDGSGEOM,YDOROG,POROGL,POROGM,&
 & PVCRS0,PVCRSSA9F,PRDLR0,PRDLR9,PVCW0F,PWDLW0F,&
 & PGAGT0L,PGAGT0M,PTOD0,&
 & PGAGT9L,PGAGT9M,PTOD9,&
 & PRDELP,PEVEL,PATND,&
 & PNHXT0,PGWT0,PNHXT9,PGWT9,PGFL,&
 ! --- INPUT/OUTPUT -------------------------------------------
 & PGMV,PGMVT1,PGMVTNDSI,PB1, &
 ! --- OUTPUT -------------------------------------------------
 & PB2,KSTPT,KSTSZ,PSTACK)

!**** *LATTEX*   Semi-Lagrangian scheme.
!                Computation of the t and t-dt useful quantities
!                 at grid-points. Equations for tri-dimensional
!                 variables.
!                Abbreviation "vwv" stands for "vertical wind variable".

!     Purpose.
!     --------
!        * This subroutine computes the equation quantities to be
!          interpolated at each grid-point of the colocation grid
!          (Gauss grid at all levels).
!          The terms considered here are the explicit terms and
!          the explicit part of the semi-implicit scheme (terms
!          previously computed in LASSIE or LANHSI).
!          Equations considered here are equations for tri-dimensional
!          variables: momentum, temperature, NH variables, GFL.
!        * Remark 1: when an alternate averaging is used for linear terms
!          in the 2TL SL scheme, the first timestep is treated differently
!          (first order uncentering), no uncentering is applied to the
!          total term ("cursive A") and the term saved in PGMV(1,1,YT9%M[X]NL)
!          is [ (Delta t/2) ("cursive A" - (1 + xidt) beta "cursive B") ]
!          instead of [ (Delta t/2) ("cursive A" - beta "cursive B") ].
!        * Remark 2: for lsettls=true, uncentering is applied to
!          the 'stable' extrapolation if vesl > 0 to avoid instability
!          in the momentum equation.
!        * Remark 3: for lnh_pdvd.and.lgwadv=true in the NH model, variable
!          "gw" is advected instead of vertical divergence; it is advected at
!          half levels if lvfe_gw=f, full levels if lvfe_gw=t.
!          That means that PDPD0,PDPD9,PATND(.,.,YYTTND%M_TNDGW),PGWT0,PGWT9 contain:
!          - half level values 0 to nflevg-1 if LVFE_GW=F.
!          - full level values 1 to nflevg if LVFE_GW=T.
!        * Remark 4: for PC schemes:
!          - this routine is called for nsiter=0.
!          - this routine is called for the predictor of lpc_full.
!          - this routine is called for the corrector of lpc_full.

!**   Interface.
!     ----------
!        *CALL* *LATTEX(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KIDIA         - first element of work.
!          KFDIA       - depth of work.
!          PDTS2       - 0.5*time step for the first time-integration step of
!                        a leap-frog scheme or all time-integration steps of
!                        a two-time level scheme; time step for the following
!                        time-integration steps of a leap-frog scheme.
!          PBT         - PDTS2*BETADT (BETADT is in YOMDYN).
!          PBDT        - PBT if semi-implicit scheme with unreduced
!                        divergence, PBT*(c**2/GM**2) if semi-implicit
!                        scheme with reduced divergence.
!          PESGP       - (1 + uncentering factor).
!          PESGM       - (1 - uncentering factor).
!          YDGSGEOM    - structure for geographical sphere horizontal geometry
!          YDOROG      - structure for orography
!          POROGL      - zonal component of "rssavnabla(surf orography)"
!          POROGM      - meridian component of "rssavnabla(surf orography)"
!          PVCRS0      - array containing quantities for LVERCOR=T (for ex. rs/a) at t.
!          PVCRSSA9F   - "rs/a" at full levels at t-dt.
!          PRDLR0      - array containing "r/a", "a/r", "grad(r/a)" at t.
!          PRDLR9      - array containing "r/a", "a/r" at t-dt.
!          PVCW0F      - pseudo-vertical velocity "W = Drs/Dt" at full lev at t.
!          PWDLW0F     - vertical velocity "w = Dr/Dt" at full levels at t.
!          PGAGT0L     - semi-implicit term at time t for U-wind equation.
!          PGAGT0M     - semi-implicit term at time t for V-wind equation.
!          PTOD0       - semi-implicit term at time t for temperature equation.
!          PSPDS0      - semi-implicit term at time t for press dep equation.
!          PLPD0       - semi-implicit term at time t for vert div equation.
!          PDPD0       - semi-implicit term at time t for "gw" equation.
!          PGAGT9L     - semi-implicit term at time t-dt for U-wind equation.
!          PGAGT9M     - semi-implicit term at time t-dt for V-wind equation.
!          PTOD9       - semi-implicit term at time t-dt for temperature eqn.
!          PSPDS9      - semi-implicit term at time t-dt for press dep equation.
!          PLPD9       - semi-implicit term at time t-dt for vert div equation.
!          PDPD9       - semi-implicit term at time t-dt for "gw" equation.
!          PRDELP      - "1/(pressure depth of layers)" at t.
!          PEVEL       - "etadot (d prehyd/d eta)" at half levels at t.
!          PATND       - adiabatic Lagrangian tendencies.
!          PNHXT0      - "X" at full levels at t, diagnosed in CPG_GP.
!          PGWT0       - "gw" at t (LGWADV=T only; layers: see in CPG_GP).
!          PNHXT9      - "X" at full levels at t-dt, diagnosed in CPG_GP.
!          PGWT9       - "gw" at t-dt (LGWADV=T only; layers: see in CPG_GP).
!          PGFL        - unified_treatment grid-point fields

!        INPUT/OUTPUT:
!          PGMV        - GMV variables at t-dt and t.
!          PGMVT1      - GMV variables at t+dt.
!          PGMVTNDSI   - GMV: tendency due to linear terms (for DDH).
!          PB1         - "SLBUF1" buffer for interpolations.

!        OUTPUT:
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
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old part 3.2 of LACDYN. 
!      Original : AUGUST 1995.

! Modifications
! -------------
!   J.Vivoda (03-2002) - rationalization, PC schemes for NH dynamics (LPC_XXXX keys)
!                      - introduction of LATTEX_DNT and LATTEX_TNT routines
!   Modified 02-09-30 P. Smolikova : variable d4 in NH
!   Modified 08-2002 C. Smith : use "w" as prognostic variable in the
!    semi-lag advection of vertical divergence in the NH model.
!   Modified 03-01-31 P. Smolikova : variable d4 in 2tl PC NH
!   A. Untch 01-08-10: transforms to b-spline space for cubic spline 
!                      interpolation in the vertical
!   Modified 2003-03-16 M.Hamrud/M.Hortal - Revised data flow (GFL)
!   01-Oct-2003 M. Hamrud  CY28 Cleaning
!   M.Hortal 01-Dec-2003 Introduce the possibility of extra-fields coming
!    from the dynamics
!   09-Jun-2004 J. Masek   NH cleaning (LPC_NOTR, LFULLIMP)
!   01-Jul-2004 K. Yessad  Make clearer the tests for PC scheme + optimization.
!   A.Untch June 2005: Rayleigh friction
!   12-Jan-2005 K. Yessad  Move calc. of (ZXM,ZX0,PNHXT9) from LACDYN to LATTEX
!   23-Feb-2005 J. Vivoda  GWADV scheme for 2TL PC NESC scheme
!   11-Jul-2005 R. Brozkova cleaning d4 subvariants
!   14-Oct-2005 Y. Bouteloup SL computation of CVGQ for French physics
!   K. Yessad 07-02-2007: Splitting alti/surf for (gw) in NH+LGWADV.
!   K. Yessad 07-03-2007: Remove useless (gw)_surf interpolations in NH+LGWADV.
!   K. Yessad 22-08-2007: Simplify data flux for NVDVAR=4
!   K. Yessad 22-08-2007: Bug correction (NH, NVDVAR=4, LSETTLS, corrector).
!   K. Yessad 09-10-2007: NH: fix calculation of ZMOY1T for VFE.
!   N. Wedi and K. Yessad (Nov 2007): ND4SYS=2 to improve NH model stability.
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   P. Bechtold+A. Untch 26-10-2008: add LEGWWMS switch for non-orogr. GWD
!   F. Vana  15-Oct-2009: option NSPLTHOI
!   K. Yessad (Nov 2009): cleanings, DT/Dt now pre-computed in CPG_GP.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana  22-Feb-2011: diff of phys. tendencies and LTDIABLIN attribute
!   K. Yessad (Dec 2011): various contributions.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   K. Yessad (Oct 2013): allow NESC without ICI-PC scheme.
!   K. Yessad (July 2014): Rename some variables, move some variables.
!   O. Marsden: June 2015 CY42 YRGMV, YRGFL, YRSURF, YRGMV5, and YRGFL5 are now passed by argument
! End Modifications
!------------------------------------------------------------------------------

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LNHDYN, LTWOTL
USE YOMCT3   , ONLY : NSTEP
USE YOMCVER  , ONLY : LVERTFE
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYN   , ONLY : YRDYN
USE YOMDYNA  , ONLY : LGWADV, NPDVAR, NVDVAR, ND4SYS, LNH_PDVD, LNH_GEOGW, &
 & LVERCOR, LRWSDLW, LRWSDLR, LPC_FULL, LPC_CHEAP, LNESC, LSETTLS
USE YOMVERT  , ONLY : YRVAB
USE YOMGSGEOM, ONLY : TGSGEOM
USE YOMOROG  , ONLY : TOROG
USE YOMSTA   , ONLY : YRSTA
USE YOM_YGFL , ONLY : YGFL
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2
USE PAR_RDLR , ONLY : JP_ASRF, JP_RSAF, JP_DIMR0, JP_DIMR9
USE INTDYN_MOD,ONLY : YYTVC0, YYTTND
USE YOMMDDH  , ONLY : YRMDDH
USE YOMLDDH  , ONLY : YRLDDH

!     ------------------------------------------------------------------

IMPLICIT NONE


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
!     ------------------------------------------------------------------
REAL(KIND=JPRB)               :: ZWT0(KLON)
REAL(KIND=JPRB)               :: ZMBF(YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMDELB(YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1U(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1V(KLON,YRDIMV%NFLEVG)
REAL(KIND=JPRB)               :: ZMOY1T(KLON,YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: IPX, IPXSP, IPQ
INTEGER(KIND=JPIM) :: JLEV, JGFL, JLON

LOGICAL :: LLSETTLSW, LLCT, LLCTC
LOGICAL :: LLTDIABLIN

!     -------------------------------------------------------

REAL(KIND=JPRB) :: ZCMSLP


!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "lattex_dnt.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*      1.  PRELIMINARY INITIALISATIONS.
!       --------------------------------

!       1.2  Scalar initialisations:

LLCT =LPC_FULL .AND. YRDYN%NCURRENT_ITER > 0  ! corrector step
LLCTC=LPC_CHEAP .AND. YRDYN%NCURRENT_ITER > 0

ZCMSLP=YRDYN%RCMSLP0/(RD*YRSTA%RTSUR)

!       1.3  reset to zero ddh arrays and pointers

IF (YRLDDH%LRSIDDH) THEN
  DO JLEV = 1, YRDIMV%NFLEVG
  DO JLON = KIDIA, KFDIA
  PGMVTNDSI(JLON,JLEV,YRMDDH%MSIDDH_U0) = 0.0_JPRB
  ENDDO
  ENDDO
  DO JLEV = 1, YRDIMV%NFLEVG
  DO JLON = KIDIA, KFDIA
  PGMVTNDSI(JLON,JLEV,YRMDDH%MSIDDH_V0) = 0.0_JPRB
  ENDDO
  ENDDO
  DO JLEV = 1, YRDIMV%NFLEVG
  DO JLON = KIDIA, KFDIA
  PGMVTNDSI(JLON,JLEV,YRMDDH%MSIDDH_T0) = 0.0_JPRB
  ENDDO
  ENDDO
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON = KIDIA, KFDIA
    PB1(JLON,YRPTRSLB1%MSLB1U9_SI+JLEV-YRDIMV%NFLSA) = 0.0_JPRB
    ENDDO
    DO JLON = KIDIA, KFDIA
    PB1(JLON,YRPTRSLB1%MSLB1V9_SI+JLEV-YRDIMV%NFLSA) = 0.0_JPRB
    ENDDO
    DO JLON = KIDIA, KFDIA
    PB1(JLON,YRPTRSLB1%MSLB1T9_SI+JLEV-YRDIMV%NFLSA) = 0.0_JPRB
    ENDDO
  ENDDO
  IF (LNHDYN) THEN
    DO JLEV = 1, YRDIMV%NFLEVG
    DO JLON = KIDIA, KFDIA
    PGMVTNDSI(JLON,JLEV,YRMDDH%MSIDDH_PD0) = 0.0_JPRB
    ENDDO
    ENDDO
    DO JLEV = 1, YRDIMV%NFLEVG
    DO JLON = KIDIA, KFDIA
    PGMVTNDSI(JLON,JLEV,YRMDDH%MSIDDH_VD0) = 0.0_JPRB
    ENDDO
    ENDDO
    DO JLEV=1,YRDIMV%NFLEVG
      DO JLON = KIDIA, KFDIA
      PB1(JLON,YRPTRSLB1%MSLB1PD9_SI+JLEV-YRDIMV%NFLSA) = 0.0_JPRB
      ENDDO
      DO JLON = KIDIA, KFDIA
      PB1(JLON,YRPTRSLB1%MSLB1VD9_SI+JLEV-YRDIMV%NFLSA) = 0.0_JPRB
      ENDDO
    ENDDO
  ENDIF
ENDIF

!     ------------------------------------------------------------------

!*      2.  TREATMENT OF GMV VARIABLES.
!       -------------------------------

!*       2.1   Momentum equation.

! * LSETTLS is replaced by LLSETTLSW=.FALSE. for wind-eqn if VESL>0 because
!   stable extrapolation deteriorates scores without improving stability.


IF (PESGP > PESGM) THEN
  LLSETTLSW=.FALSE.
ELSE
  LLSETTLSW=LSETTLS
ENDIF

DO JLEV=1,YRDIMV%NFLEVG

  DO JLON=KIDIA,KFDIA
    PB2(JLON,YRPTRSLB2%MSLB2USI+JLEV-1)=PBT*PGAGT0L(JLON,JLEV)
    PB2(JLON,YRPTRSLB2%MSLB2VSI+JLEV-1)=PBT*PGAGT0M(JLON,JLEV)
  ENDDO

  IF ((YRPTRSLB2%MSLB2USI+JLEV-1 == 1) .OR. &
    & (YRPTRSLB2%MSLB2VSI+JLEV-1 == 1)) THEN
  ENDIF


  ! * Add pressure gradient term + Rayleigh friction in the wind equation.
  DO JLON=KIDIA,KFDIA
    ZMOY1U(JLON,JLEV)=PDTS2*PATND(JLON,JLEV,YYTTND%M_TNDU_NOC)
    ZMOY1V(JLON,JLEV)=PDTS2*PATND(JLON,JLEV,YYTTND%M_TNDV_NOC)
  ENDDO

  ! * Add "- 2 Omega vec V" explicit contribution when required.
  IF (.NOT.YRDYN%LADVF) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV) &
       & +PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MV)
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV) &
       & -PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MU)
    ENDDO
  ENDIF
  ! * Add "- 2 Omega vec W k" explicit contribution when required.
  IF (LVERCOR.AND.(.NOT.YRDYN%LADVFW)) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV)-PDTS2* &
       & YDGSGEOM%GNORDM(JLON)*YDGSGEOM%RCORIC(JLON)*PVCW0F(JLON,JLEV)
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV)+PDTS2* &
       & YDGSGEOM%GNORDL(JLON)*YDGSGEOM%RCORIC(JLON)*PVCW0F(JLON,JLEV)
    ENDDO
  ELSEIF (LRWSDLW.AND.(.NOT.YRDYN%LADVFW)) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV)-PDTS2* &
       & YDGSGEOM%GNORDM(JLON)*YDGSGEOM%RCORIC(JLON)*PWDLW0F(JLON,JLEV)
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV)+PDTS2* &
       & YDGSGEOM%GNORDL(JLON)*YDGSGEOM%RCORIC(JLON)*PWDLW0F(JLON,JLEV)
    ENDDO
  ENDIF
  ! * Add "- 2 Omega vec (1-a/rs) V" residual explicit contrib when required.
  IF (LVERCOR.AND.YRDYN%LADVF.AND.(.NOT.YRDYN%LADVFW)) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV)+PDTS2* &
       & YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MV)*(1.0_JPRB-PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF))
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV)-PDTS2* &
       & YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MU)*(1.0_JPRB-PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF))
    ENDDO
  ENDIF
  ! * Add "- 2 Omega vec (1-a/r) V" residual explicit contrib when required.
  IF (LRWSDLW.AND.LRWSDLR.AND.YRDYN%LADVF.AND.(.NOT.YRDYN%LADVFW)) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV) &
       & +PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MV) &
       & *(1.0_JPRB-PRDLR0(JLON,JLEV,JP_ASRF))
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV) &
       & -PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MU) &
       & *(1.0_JPRB-PRDLR0(JLON,JLEV,JP_ASRF))
    ENDDO
  ENDIF
  ! * Add "- 2 Omega vec (a/rs-1) V" residual explicit contrib when required.
  IF (LVERCOR.AND.(.NOT.YRDYN%LADVF).AND.YRDYN%LADVFW) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV)+PDTS2* &
       & YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MV)*(PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF)-1.0_JPRB)
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV)-PDTS2* &
       & YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MU)*(PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF)-1.0_JPRB)
    ENDDO
  ENDIF
  ! * Add "- 2 Omega vec (a/r-1) V" residual explicit contrib when required.
  IF (LRWSDLW.AND.LRWSDLR.AND.(.NOT.YRDYN%LADVF).AND.YRDYN%LADVFW) THEN
    DO JLON=KIDIA,KFDIA
      ZMOY1U(JLON,JLEV)=ZMOY1U(JLON,JLEV) &
       & +PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MV) &
       & *(PRDLR0(JLON,JLEV,JP_ASRF)-1.0_JPRB)
      ZMOY1V(JLON,JLEV)=ZMOY1V(JLON,JLEV) &
       & -PDTS2*YDGSGEOM%RCORI(JLON)*PGMV(JLON,JLEV,YDGMV%YT0%MU) &
       & *(PRDLR0(JLON,JLEV,JP_ASRF)-1.0_JPRB)
    ENDDO
  ENDIF

ENDDO

IF (LTWOTL) THEN
  
  CALL LATTEX_DNT(KLON,KIDIA,KFDIA,LLSETTLSW,YRDYN%NWLAG,PESGP,PESGM,&
   & PGMV(1,1,YDGMV%YT0%MU),PGMV(1,1,YDGMV%YT9%MU),ZMOY1U,&
   & PB2(1,YRPTRSLB2%MSLB2USI),PGMV(1,1,YDGMV%YT9%MUNL),PGMVT1(1,1,YDGMV%YT1%MU),&
   & PB1(1,YRPTRSLB1%MSLB1U0),PB1(1,YRPTRSLB1%MSLB1U9),PB1(1,YRPTRSLB1%MSLB1UF9),PGMV(1,1,YDGMV%YT9%MCUNL),&
   & PGMVTNDSI(1,1,YRMDDH%MSIDDH_U0),PGMVTNDSI(1,1,YRMDDH%MSIDDH_U9),PB1(1,YRPTRSLB1%MSLB1U9_SI),&
   & ISTPT,KSTSZ,PSTACK)
  
  CALL LATTEX_DNT(KLON,KIDIA,KFDIA,LLSETTLSW,YRDYN%NWLAG,PESGP,PESGM,&
   & PGMV(1,1,YDGMV%YT0%MV),PGMV(1,1,YDGMV%YT9%MV),ZMOY1V, &
   & PB2(1,YRPTRSLB2%MSLB2VSI),PGMV(1,1,YDGMV%YT9%MVNL),PGMVT1(1,1,YDGMV%YT1%MV),&
   & PB1(1,YRPTRSLB1%MSLB1V0),PB1(1,YRPTRSLB1%MSLB1V9),PB1(1,YRPTRSLB1%MSLB1VF9),PGMV(1,1,YDGMV%YT9%MCVNL),&
   & PGMVTNDSI(1,1,YRMDDH%MSIDDH_V0),PGMVTNDSI(1,1,YRMDDH%MSIDDH_V9),PB1(1,YRPTRSLB1%MSLB1V9_SI),ISTPT,KSTSZ,PSTACK)
  
  IF(LVERCOR.AND.YRDYN%LADVFW.AND.(.NOT.LLCTC)) THEN
    ! need to interpolate "rs/a" (instant t) at O.
    DO JLEV=1,YRDIMV%NFLEVG
      DO JLON=KIDIA,KFDIA
        PB1(JLON,YRPTRSLB1%MSLB1RSSA9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1RSSA9+JLEV-YRDIMV%NFLSA) &
         & +PVCRS0(JLON,JLEV,YYTVC0%M_VCRSSAF)
      ENDDO
    ENDDO
  ENDIF
  IF (LRWSDLW.AND.LRWSDLR.AND.YRDYN%LADVFW.AND.(.NOT.LLCTC)) THEN
    ! need to interpolate "r/a" (instant t) at O.
    DO JLEV=1,YRDIMV%NFLEVG
      DO JLON=KIDIA,KFDIA
        PB1(JLON,YRPTRSLB1%MSLB1RSSA9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1RSSA9+JLEV-YRDIMV%NFLSA) &
         & +PRDLR0(JLON,JLEV,JP_RSAF)
      ENDDO
    ENDDO
  ENDIF
  
ENDIF
  
IF(YRDYN%LADVF.AND.(.NOT.YRDYN%LADVFW)) THEN
  DO JLEV=1,YRDIMV%NFLEVG
    DO JLON=KIDIA,KFDIA
      PGMVT1(JLON,JLEV,YDGMV%YT1%MU)=PGMVT1(JLON,JLEV,YDGMV%YT1%MU)-YDGSGEOM%GOMVRL(JLON)
      PGMVT1(JLON,JLEV,YDGMV%YT1%MV)=PGMVT1(JLON,JLEV,YDGMV%YT1%MV)-YDGSGEOM%GOMVRM(JLON)
    ENDDO
  ENDDO
ENDIF

!*       2.2   Temperature equation.

DO JLEV=1,YRDIMV%NFLEVG

  DO JLON=KIDIA,KFDIA
    PB2(JLON,YRPTRSLB2%MSLB2TSI+JLEV-1)=PBDT(JLON)*PTOD0(JLON,JLEV)
  ENDDO

  ! * compute ZMOY1T
  IF(LVERTFE) THEN
    DO JLON=KIDIA,KFDIA
      ZWT0(JLON)=PEVEL(JLON,JLEV)*PRDELP(JLON,JLEV)
    ENDDO
  ELSE
    DO JLON=KIDIA,KFDIA
      ZWT0(JLON)=0.5_JPRB*(PEVEL(JLON,JLEV)&
       & +PEVEL(JLON,JLEV-1))*PRDELP(JLON,JLEV)
    ENDDO
  ENDIF
  DO JLON=KIDIA,KFDIA
    ZMOY1T(JLON,JLEV)=&
     & PDTS2*ZCMSLP*YRDYN%RCORDIF(JLEV)*PGMV(JLON,JLEV,YDGMV%YT0%MU)*POROGL(JLON)&
     & +PDTS2*ZCMSLP*YRDYN%RCORDIF(JLEV)*PGMV(JLON,JLEV,YDGMV%YT0%MV)*POROGM(JLON)
  ENDDO
  IF(LVERCOR) THEN
    ! take account of the fact that (POROGL,POROGM) contain (rs/a)*nabla(orog)
    ! and that we need to use nabla(orog).
    DO JLON=KIDIA,KFDIA
      ZMOY1T(JLON,JLEV)=ZMOY1T(JLON,JLEV)*PVCRS0(JLON,JLEV,YYTVC0%M_VCASRSF)
    ENDDO
  ENDIF
  IF(LRWSDLR) THEN
    ! take account of the fact that (POROGL,POROGM) contain (r/a)*nabla(orog)
    ! and that we need to use nabla(orog).
    DO JLON=KIDIA,KFDIA
      ZMOY1T(JLON,JLEV)=ZMOY1T(JLON,JLEV)*PRDLR0(JLON,JLEV,JP_ASRF)
    ENDDO
  ENDIF
  DO JLON=KIDIA,KFDIA
    ZMOY1T(JLON,JLEV)=ZMOY1T(JLON,JLEV)&
     & +PDTS2*PATND(JLON,JLEV,YYTTND%M_TNDT)&
     & +PDTS2*ZCMSLP*(YRDYN%RCORDIH(JLEV)-YRDYN%RCORDIH(JLEV-1))*ZWT0(JLON)*YDOROG%OROG(JLON)
  ENDDO

ENDDO

IF (LTWOTL) THEN
  
  CALL LATTEX_DNT(KLON,KIDIA,KFDIA,LSETTLS,YRDYN%NTLAG,PESGP,PESGM, &
   & PGMV(1,1,YDGMV%YT0%MT),PGMV(1,1,YDGMV%YT9%MT),ZMOY1T,&
   & PB2(1,YRPTRSLB2%MSLB2TSI),PGMV(1,1,YDGMV%YT9%MTNL),PGMVT1(1,1,YDGMV%YT1%MT), &
   & PB1(1,YRPTRSLB1%MSLB1T0),PB1(1,YRPTRSLB1%MSLB1T9),PB1(1,YRPTRSLB1%MSLB1TF9),PGMV(1,1,YDGMV%YT9%MCTNL),&
   & PGMVTNDSI(1,1,YRMDDH%MSIDDH_T0),PGMVTNDSI(1,1,YRMDDH%MSIDDH_T9),PB1(1,YRPTRSLB1%MSLB1T9_SI),ISTPT,KSTSZ,PSTACK)
  
ENDIF

DO JLEV=1,YRDIMV%NFLEVG
  DO JLON=KIDIA,KFDIA
    PGMVT1(JLON,JLEV,YDGMV%YT1%MT)=PGMVT1(JLON,JLEV,YDGMV%YT1%MT) &
     & -YRDYN%RCORDIF(JLEV)*ZCMSLP*YDOROG%OROG(JLON)
  ENDDO
  IF (.NOT.LLCTC) THEN
    DO JLON=KIDIA,KFDIA
      PB1(JLON,YRPTRSLB1%MSLB1T9+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1T9+JLEV-YRDIMV%NFLSA) &
       & +YRDYN%RCORDIF(JLEV)*ZCMSLP*YDOROG%OROG(JLON)
    ENDDO
  ENDIF
ENDDO

!     ------------------------------------------------------------------

!*      3.  TREATMENT OF GFL VARIABLES.
!       --------------------------------

IF (LTWOTL) THEN
  
  IF(LLCT.AND.(.NOT.LLCTC))THEN
    DO JGFL=1,YGFL%NUMFLDS
      IF(YGFL%YCOMP(JGFL)%LADV) THEN
        IPX=(YGFL%YCOMP(JGFL)%MP_SL1-1)*(YRDIMV%NFLEN-YRDIMV%NFLSA+1)
        LLTDIABLIN=YGFL%YCOMP(JGFL)%LTDIABLIN
        IF (YRDYN%LSPLTHOIGFL.AND.(YRDYN%NSPLTHOI==0).AND.(.NOT.LLTDIABLIN)) THEN
          DO JLEV=1,YRDIMV%NFLEVG
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)&
               & +PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPX+JLEV-YRDIMV%NFLSA)+PGFL(JLON,JLEV,YGFL%YCOMP(JGFL)%MP9)
            ENDDO
          ENDDO
        ELSE
          DO JLEV=1,YRDIMV%NFLEVG
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)&
               & +PGFL(JLON,JLEV,YGFL%YCOMP(JGFL)%MP9)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ELSEIF (.NOT.LLCT) THEN
    DO JGFL=1,YGFL%NUMFLDS
      IF(YGFL%YCOMP(JGFL)%LADV) THEN
        IPX=(YGFL%YCOMP(JGFL)%MP_SL1-1)*(YRDIMV%NFLEN-YRDIMV%NFLSA+1)
        LLTDIABLIN=YGFL%YCOMP(JGFL)%LTDIABLIN
        IF (YRDYN%LSPLTHOIGFL.AND.(YRDYN%NSPLTHOI==0).AND.(.NOT.LLTDIABLIN)) THEN
          DO JLEV=1,YRDIMV%NFLEVG
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)&
               & +PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPX+JLEV-YRDIMV%NFLSA)+PGFL(JLON,JLEV,YGFL%YCOMP(JGFL)%MP)
            ENDDO
          ENDDO
        ELSE
          DO JLEV=1,YRDIMV%NFLEVG
            DO JLON=KIDIA,KFDIA
              PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)&
               & +PGFL(JLON,JLEV,YGFL%YCOMP(JGFL)%MP)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDIF


!  Qv in YCVGQ for computation of SL Moisture Convergence for French physics

  IF (YGFL%YCVGQ%LGP .AND. (.NOT.LLCTC)) THEN  
    IPX=(YGFL%YCVGQ%MP_SL1-1)*(YRDIMV%NFLEN-YRDIMV%NFLSA+1)
    IPQ=(YGFL%YQ%MP_SL1-1)*(YRDIMV%NFLEN-YRDIMV%NFLSA+1)
    LLTDIABLIN=YGFL%YCVGQ%LTDIABLIN
    IF ((YRDYN%NSPLTHOI /= 0).OR.LLTDIABLIN) THEN
      DO JLEV=1,YRDIMV%NFLEVG
        DO JLON=KIDIA,KFDIA
          PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPQ+JLEV-YRDIMV%NFLSA)
        ENDDO
      ENDDO
    ENDIF
    DO JLEV=1,YRDIMV%NFLEVG
      DO JLON=KIDIA,KFDIA
        PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPQ+JLEV-YRDIMV%NFLSA)
      ENDDO
    ENDDO
    LLTDIABLIN=YGFL%YCVGQ%LTDIABLIN.AND.YGFL%YQ%LTDIABLIN
    IF ((YRDYN%NSPLTHOI == 0).AND.LLTDIABLIN) THEN
      DO JLEV=1,YRDIMV%NFLEVG
        DO JLON=KIDIA,KFDIA
          PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA) &
           & +PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPQ+JLEV-YRDIMV%NFLSA)
        ENDDO
      ENDDO
    ENDIF
  ENDIF   
  
ELSE
  
  DO JGFL=1,YGFL%NUMFLDS
    IF(YGFL%YCOMP(JGFL)%LADV) THEN
      IPX=(YGFL%YCOMP(JGFL)%MP_SL1-1)*(YRDIMV%NFLEN-YRDIMV%NFLSA+1)
      DO JLEV=1,YRDIMV%NFLEVG
        DO JLON=KIDIA,KFDIA
          PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA) &
           & +PGFL(JLON,JLEV,YGFL%YCOMP(JGFL)%MP9)
        ENDDO
      ENDDO
      LLTDIABLIN=YGFL%YCOMP(JGFL)%LTDIABLIN
      IF (YRDYN%LSPLTHOIGFL.AND.(YRDYN%NSPLTHOI==0).AND.(.NOT.LLTDIABLIN)) THEN
        DO JLEV=1,YRDIMV%NFLEVG
          DO JLON=KIDIA,KFDIA
            PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA)=PB1(JLON,YRPTRSLB1%MSLB1GFL9+IPX+JLEV-YRDIMV%NFLSA) &
             & +PB1(JLON,YRPTRSLB1%MSLB1GFLF9+IPX+JLEV-YRDIMV%NFLSA)
          ENDDO
        ENDDO
      ENDIF
    ENDIF
  ENDDO

ENDIF

!     ------------------------------------------------------------------



END SUBROUTINE LATTEX

