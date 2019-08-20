SUBROUTINE LACDYN(YDGMV, &
 ! --- INPUT ------------------------------------------------------------------
 & KST,KPROF,PBETADT,PDT,PSLHDA,PSLHDD0, &
 & YDGSGEOM,YDOROG,POROGL,POROGM,&
 & PVCRSSA9F,PNHXT9,PVCRS0,PVCW0F,PWDLW0F,PRDLR0,PRDLR9,PNHXT0,&
 & PRES0,PRDELP0,PCTY0,PUVH0,&
 & PATND,PDBBC,PRDPHI,PGWT0,PGWT9,PGFL,&
 ! --- INPUT/OUTPUT -----------------------------------------------------------
 & KSETTLOFF,PGMV,PGMVS,PB1,PB2,PGMVT1,PGMVT1S,PGWS,PGMVTNDSI,PWRL95)

!**** *LACDYN*   Semi-Lagrangian scheme.
!                Computation of the t and t-dt useful quantities
!                 at grid-points.

!     Purpose.
!     --------

!          Dynamic non-linear computations in grid-point space
!          for hydrostatic and NH primitive equations and SL scheme.

!          Remark (in particular when the thin layer hypothesis is relaxed):
!          input horizontal derivatives are the semi-reduced ones
!          (i.e. rssavnabla X = (rs/a) vnabla X = M vnabla' X, where "M" is
!          the mapping factor and "vnabla'" is the reduced gradient operator
!          available in spectral space); input divergence and
!          vorticity are the semi-reduced ones.

!          Additional remarks:
!          - notation "prehyds" is for hydrostatic surface pressure.
!          - for input and output upper air variables, values are at full levels
!            if no other information is provided.
!          - NH variables: "P variable" is the pressure departure variable
!            (pressure departure equation), i.e. ln(pre/prehyd) if npdvar=2;
!            abbreviation "vwv" stands for "vertical wind variable".
!          - for PC schemes:
!            * this routine is called for nsiter=0.
!            * this routine is called for the predictor of lpc_full.
!            * this routine is called for the corrector of lpc_full.

!          This subroutine fills the semi-Lagrangian buffers to be
!          interpolated.

!**   Interface.
!     ----------
!        *CALL* *LACDYN(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST         - first element of work.
!          KPROF       - depth of work.
!          PBETADT     - BETADT or 0 according to configuration.
!          PDT         - For a leap-frog scheme (three time level scheme):
!                         'dt' at the first time-step, '2 dt' otherwise.
!                        For a 2TL SL scheme: timestep 'dt'.
!          PSLHDA      - Scaling factor of the deformation in f(d) function
!                        (including the model resolution correction)
!          PSLHDD0     - Treshold for deformation tensor enhancement
!          YDGSGEOM    - structure for geographical sphere horizontal geometry
!          YDOROG      - structure for orography
!          POROGL      - zonal component of the orography semi-reduced gradient
!          POROGM      - merid component of the orography semi-reduced gradient
!          PVCRSSA9F   - "rs/a" at full levels at t-dt.
!          PNHXT9      - "X" at full levels at t-dt, diagnosed in CPG_GP.
!          PVCRS0      - array containing quantities for LVERCOR=T (for ex. rs/a) at t.
!          PVCW0F      - pseudo-vertical velocity "W = Drs/Dt" at full lev at t.
!          PWDLW0F     - vertical velocity "w = Dr/Dt" at full levels at t.
!          PRDLR0      - array containing "r/a", "a/r", "grad(r/a)" at t.
!          PRDLR9      - array containing "r/a", "a/r" at t-dt.
!          PNHXT0      - "X" at full levels at t, diagnosed in CPG_GP.
!          PRES0       - hydrostatic pressure "prehyd" at half levels at t.
!          PRDELP0     - 1/(pressure depth of layers) at t.
!          PCTY0       - contains vertical velocities, vertical integral of divergence at t.
!          PUVH0       - horizontal wind at time t at half levels.
!          PATND       - adiabatic Lagrangian tendencies.
!          PDBBC       - [D (r**2/a**2) (Gw)_surf / Dt]_adiab if lrwsdlr.and.lrwsdlr2=T.
!                        [D (Gw)_surf / Dt]_adiab otherwise.
!          PRDPHI      - pre/(Rd T prehyd [Delta log(prehyd)]) at t.
!          PGWT0       - [Gw] at t (LGWADV=T only; levels: see CPG_GP).
!          PGWT9       - [Gw] at t-dt (LGWADV=T only; levels: see CPG_GP).
!          PGFL        - unified_treatment grid-point fields

!        INPUT/OUTPUT:
!          KSETTLOFF   - counter for SETTLSTF=T (# of points new scheme activated at each lev)
!          PGMV        - GMV variables at t-dt and t.
!          PGMVS       - GMVS variables at t-dt and t.
!          PB1         - "SLBUF1" buffer for interpolations.
!          PB2         - "SLBUF2" buffer.
!          PGMVT1      - GMV variables at t+dt.
!          PGMVT1S     - GMVS variables at t+dt.
!          PGWS        - (r**2/a**2)_surf [Gw]_surf at t if lrwsdlr.and.lrwsdlr2=T
!                        [Gw]_surf at t otherwise (LRDBBC only).
!          PGMVTNDSI   - GMV: tendency due to linear terms (for DDH).
!          PWRL95      - store current timestep PGMV(,,YMEDOT%YT9) values before re-setting

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        Called by CPG_DYN.

!     Reference.
!     ----------
!             Arpege documentation about semi-lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after routines
!        CPLGDY1 and LAGSIMP coded by Maurice IMBARD.
!      Original : FEBRUARY 1992.

! Modifications
! -------------
!   J.Vivoda 03-2002 PC schemes for NH dynamics (LPC_XXXX keys)
!   P.Smolikova 02-09-30 : variable d4 in NH
!   Modified 08-2002 C. Smith : use "w" as prognostic variable in the
!    semi-lag advection of vertical divergence in the NH model.
!   P.Smolikova 03-01-31 : variable d4 in 2tl PC NH
!   Modified 2003-03-16 M.Hamrud/M.Hortal - Revised data flow (GFL)
!   F. Vana 03-08-25 : new vertical profile of diffusion.
!   P.Smolikova 08-2003 : option LRDBBC.
!   01-Oct-2003 M. Hamrud  CY28 Cleaning
!   M.Hortal      01-Dec-2003 Extra-fields coming from the dynamics
!   09-Jun-2004 J. Masek   NH cleaning (LPC_NOTR, LFULLIMP, LVSLWBC)
!   01-Jul-2004 K. Yessad  Make clearer the tests for PC scheme.
!   Y.Seity 11-2004 : New arguments in call LASSIE and LANHSI for GPRCP
!   04-11-04 F. Vana: LATTE_DEF --> LATTE_KAPPA
!   12-Jan-2005 K. Yessad: GNHPDVD replaced by GP./GNH. routines in CPG_GP.
!   20-Feb-2005 J. Vivoda  GWADV scheme for PC_FULL (d3,d4)
!   15-Jan-2005 M.Hamrud   Revised GPRCP
!   11-Jul-2005 R. Brozkova cleaning d4 subvariants
!   08-Jun-2006 F. Vana two new arguments to LATTE_KAPPA
!   K. Yessad 07-02-2007: Splitting alti/surf for (Gw) in NH+LGWADV.
!   K. Yessad 07-02-2007: Update some comments.
!   K. Yessad 07-03-2007: Remove useless (Gw)_surf interpolations in NH+LGWADV.
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!   09-Sep-2008 J. Masek  Dataflow for flow deformation along pressure levels
!   K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!   K. Yessad (Nov 2009): cleanings, DT/Dt now pre-computed in CPG_GP.
!   K. Yessad (Nov 2009): prune lpc_old.
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   K. Yessad (Dec 2011): various contributions.
!   M. Diamantakis (Feb 2014): code for LSETTLSVF=T
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   S. Malardel (Nov 2013): LATTE_STDDIS for COMAD corrections
!   K. Yessad (July 2014): Move some variables.
!   O. Marsden: June 2015 CY42 YRGMV, YRGFL, YRSURF, YRGMV5, and YRGFL5 are now passed by argument
! End Modifications
!------------------------------------------------------------------

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYNA  , ONLY : LGWADV, LRDBBC, LSLHD, LNH_PDVD, LNH_GEOGW,&
                    & LCOMAD, LCOMADH, LCOMADV, LPC_FULL, LPC_CHEAP
USE YOMDYN   , ONLY : YRDYN
USE YOM_YGFL , ONLY : YGFL
USE PAR_RDLR , ONLY : JP_DIMR0, JP_DIMR9
USE YOMMDDH  , ONLY : YRMDDH
USE YOMGSGEOM, ONLY : TGSGEOM
USE YOMOROG  , ONLY : TOROG
USE INTDYN_MOD,ONLY : YYTVC0, YYTTND, YYTHW0, YYTCTY0
USE PTRSLB1  , ONLY : YRPTRSLB1
USE PTRSLB2  , ONLY : YRPTRSLB2

!   ---------------------------------------------------------------

IMPLICIT NONE

TYPE(TGMV) , INTENT(INOUT) :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSETTLOFF(YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBETADT
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLHDA(YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLHDD0(YRDIM%NPROMA)
TYPE(TGSGEOM)     ,INTENT(IN)    :: YDGSGEOM
TYPE(TOROG)       ,INTENT(IN)    :: YDOROG
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGL(YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: POROGM(YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRSSA9F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT9(YRDIM%NPROMA,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCRS0(YRDIM%NPROMVC,0:YRDIMV%NFLEVG,YYTVC0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCW0F(YRDIM%NPROMVC,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWDLW0F(YRDIM%NPROMDLW,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR0(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR0)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDLR9(YRDIM%NPROMDLR,0:YRDIMV%NFLEVG,JP_DIMR9)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNHXT0(YRDIM%NPROMA,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRES0(YRDIM%NPROMA,0:YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP0(YRDIM%NPROMA,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCTY0(YRDIM%NPROMA,0:YRDIMV%NFLEVG,YYTCTY0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUVH0(YRDIM%NPROMA,0:YRDIMV%NFLEVG,YYTHW0%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PATND(YRDIM%NPROMA,YRDIMV%NFLEVG,YYTTND%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDBBC(YRDIM%NPROMNH)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDPHI(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT0(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGWT9(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(YRDIM%NPROMA,YRDIMV%NFLEVG,YGFL%NDIM) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMV(YRDIM%NPROMA,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVS(YRDIM%NPROMA,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB1(YRDIM%NPROMA,YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PB2(YRDIM%NPROMA,YRPTRSLB2%NFLDSLB2)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1(YRDIM%NPROMA,YRDIMV%NFLEVG,YDGMV%YT1%NDIM)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVT1S(YRDIM%NPROMA,YDGMV%YT1%NDIMS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGWS(YRDIM%NPROMNH)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGMVTNDSI(YRDIM%NPROMA,YRDIMV%NFLEVG,YRMDDH%NDIMSIGMV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWRL95(YRDIM%NPROMA,YRDIMV%NFLEVG)
!     ------------------------------------------------------------------
! * computed in LASURE:
REAL(KIND=JPRB) :: ZBT, ZDTS2, ZESGM, ZESGP
REAL(KIND=JPRB) :: ZREDIV(YRDIM%NPROMA)
! * computed in LASSIE or LANHSI:
REAL(KIND=JPRB) :: ZBDT(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZGAGT0L(YRDIM%NPROMA,YRDIMV%NFLEVG),ZGAGT0M(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZGAGT9L(YRDIM%NPROMA,YRDIMV%NFLEVG),ZGAGT9M(YRDIM%NPROMA,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZLPD0(YRDIM%NPROMNH,YRDIMV%NFLEVG),ZLPD9(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZDPD0(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZDPD9(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZSDIV0(YRDIM%NPROMA),ZSDIV9(YRDIM%NPROMA)
REAL(KIND=JPRB) :: ZSPDS0 (YRDIM%NPROMNH,YRDIMV%NFLEVG),ZSPDS9(YRDIM%NPROMNH,YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZTOD0(YRDIM%NPROMA,YRDIMV%NFLEVG),ZTOD9(YRDIM%NPROMA,YRDIMV%NFLEVG)

LOGICAL :: LL2TLFF1, LLGWADV


!     ------------------------------------------------------------------

#include "lassie.intfb.h"
#include "lasure.intfb.h"
#include "lattes.intfb.h"
#include "lattex.intfb.h"
#include "lavabo.intfb.h"
#include "lavent.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*       0.    ALLOCATIONS:
!              ------------

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS:
!              ----------------------------

CALL LASURE(KST,KPROF,PBETADT,PDT,YDGSGEOM,&
 & ZDTS2,ZBT,LL2TLFF1,ZBDT,ZREDIV,ZESGP,ZESGM)  

!     ------------------------------------------------------------------

!*       2.    COMPUTATION OF THE LINEAR TERMS FOR SEMI-IMPLICIT SCHEME.
!              ---------------------------------------------------------

ZGAGT0M = 0

CALL LASSIE(YDGMV,KST,KPROF,YDGSGEOM%RCORI,PGMV,PGMVS,PGFL,&
& ZSDIV0,ZSDIV9,ZTOD0,ZTOD9,ZGAGT0L,ZGAGT0M,ZGAGT9L,ZGAGT9M)  

!     ------------------------------------------------------------------

!*       3.    COMPUTATION OF THE WIND COMPONENTS NECESSARY FOR SL TRAJECTORY.
!              ---------------------------------------------------------------

IF ((YRDYN%NCURRENT_ITER == 0).OR. (&
 & YRDYN%NCURRENT_ITER > 0 .AND. LPC_FULL .AND.(.NOT.LPC_CHEAP) )) THEN

  CALL LAVENT(YDGMV,KST,KPROF,KSETTLOFF,LL2TLFF1,PVCRS0,PRDLR0,ZDTS2,PRDELP0,&
            & PCTY0(1,0,YYTCTY0%M_EVEL),PATND,PGMV,PB1,PB2,PWRL95)

ENDIF

!     ------------------------------------------------------------------

!*       4.    COMPUTATION OF THE 3D-EQUATIONS RIGHT-HAND SIDE TERMS.
!              ------------------------------------------------------

!        4.1:  general case:

CALL LATTEX(YDGMV,KST,KPROF,ZDTS2,ZBT,ZBDT,ZESGP,ZESGM,&
 & YDGSGEOM,YDOROG,POROGL,POROGM,&
 & PVCRS0,PVCRSSA9F,PRDLR0,PRDLR9,PVCW0F,PWDLW0F,&
 & ZGAGT0L,ZGAGT0M,ZTOD0,ZSPDS0,ZLPD0,ZDPD0,&
 & ZGAGT9L,ZGAGT9M,ZTOD9,ZSPDS9,ZLPD9,ZDPD9,&
 & PRDELP0,PCTY0(1,0,YYTCTY0%M_EVEL),PATND,&
 & PNHXT0,PGWT0,PNHXT9,PGWT9,PGFL,&
 & PGMV,PGMVT1,PGMVTNDSI,PB1,PB2)

!        4.2:  Additional quantities required in the NH model
!              for option LRDBBC=T:

!     ------------------------------------------------------------------

!*       5.    COMPUTATION OF THE 2D-EQUATIONS RIGHT-HAND SIDE TERMS.
!              ------------------------------------------------------

CALL LATTES(YDGMV,KST,KPROF,ZDTS2,ZBDT,ZESGP,ZESGM,&
 & PVCRS0,PRDLR0,YDOROG,POROGL,POROGM,&
 & ZSDIV0,ZSDIV9,PCTY0(1,0,YYTCTY0%M_PSDVBC),PRES0,PGMVS,&
 & PGMV,PGMVT1S,PB1,PB2)

!     ------------------------------------------------------------------

!*       6.    COMPUTATION OF "KAPPA" AND STORE IT IN  "SLBUF2".
!              -----------------------------------------------------------

!     ------------------------------------------------------------------

!*       7.    COMPUTATION OF "STDDIS" for each direction
!              AND STORE THEM IN  "SLBUF2".
!              -----------------------------------------------------------

!     ------------------------------------------------------------------

!*       8.    UPPER AND LOWER LATERAL BOUNDARIES CONDITIONS.
!              ----------------------------------------------

IF ((YRDYN%NCURRENT_ITER == 0).OR. (&
 & YRDYN%NCURRENT_ITER > 0 .AND. LPC_FULL .AND.(.NOT.LPC_CHEAP) )) THEN
  CALL LAVABO(KST,KPROF,LL2TLFF1,PB1)
ENDIF

END SUBROUTINE LACDYN
