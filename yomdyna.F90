MODULE YOMDYNA

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

! ----------------------------------------------------------------------

!=========== VARIABLES FOR DYNAMICS: FIRST PART ===============================
! We put there not geometry-dependent variables for dynamics.
! Values are identical for all models run under the OOPS layer.
!==============================================================================

! ------ NH model ------------------------------------------------------

! NPDVAR : switch for type of variable used for pressure departure
!         2: q_hat = ln(p/pi)
!         3: Phi - B*Phi_surf
!         4: Phi 

! NVDVAR : switch for type of variable used for pseudo vertical divergence
!         3: d3 = -g (p/(m.Rd.T)) d_parc w/d_parc eta
!         4: d4 = d3 + X = d3 + (p/m.R.T) nabla_phi d_parc V/d_parc eta
!         5: gw-B*gw_s
!         6: gw

! ND4SYS : switch for the way of treatment of term NHX in the NH d4 equation.
!          ND4SYS=1: all contributions of D(NHX)/Dt are treated at the level
!                    of CPG_DYN+CALL_SL, and X is updated via GPXX in CPG_GP.
!          ND4SYS=2 (SL only): only the advective terms are treated at the
!                    level of CPG_DYN+CALL_SL; additional contributions
!                    are done in CPGLAG, and X(t+dt) is updated in CPGLAG.

! LNH_PDVD: NH model with NH variables based on pressure departure and
!           vertical divergence (i.e currently NPDVAR=2, NVDVAR=3 or 4).

! LNH_GEOGW: NH model with NH variables based on geopotential and vertical
!            velocity (i.e. currently NPDVAR=3 or 4, NVDVAR=5 or 6).

! LNHX   : variable NHX (grid-point and spectral) is needed at t.
! LNHXDER: horizontal derivatives of variable NHX are needed at t.

! LGWADV: .T. => "vwv" prognostic variable in NH model is "g*w"
!                (but the variable which is transformed into spectral
!                space is "d_nvdvar").
!         .F. => "vwv" prognostic variable in NH model is "d_nvdvar".

! NGWADVSI: when LGWADV, alternate treatments for linear terms.
!           NGWADVSI=1: linear terms set to 0 in LATTEX, LATTES for all eqns;
!            linear terms evaluated at O and linear terms evaluated at F
!            added to the RHS of equations in GPENDTR.
!           NGWADVSI=2 (relevant for LSETTLS but not for LNESC):
!            linear terms set to 0 in LATTEX, LATTES for all eqns;
!            linear terms evaluated at O added to the RHS of equations in
!            LAPINEB (after the call to GNHGW2SVD); linear terms evaluated
!            at F added to the RHS of equations in GPENDTR.

! LRDBBC: .T. if S.-L. diagnostic BBC active for NH (if LSLAG=.T. only)
!         .F. if eulerian development of NH  BBC in LSLAG.

! LGWOPT1: Case LNH_GEOGW=T only. Way to compute "dver" from "gw" in the RHS
!          of temperature equation.
!          .F. => vertical derivative applied to "gw".
!          .T. => vertical derivative applied to pseudo NH departure of "gw".

INTEGER(KIND=JPIM) :: NPDVAR
INTEGER(KIND=JPIM) :: NVDVAR
INTEGER(KIND=JPIM) :: ND4SYS
LOGICAL :: LNH_PDVD
LOGICAL :: LNH_GEOGW
LOGICAL :: LNHX
LOGICAL :: LNHXDER
LOGICAL :: LGWADV
INTEGER(KIND=JPIM) :: NGWADVSI
LOGICAL :: LRDBBC
LOGICAL :: LGWOPT1

! ------ SLHD diffusion ------------------------------------------------

!          SLHD (= horizontal diffusion using damping properties of some
!          semi-Lagrangian interpolatores) keys
! LSLHD     : internal model switch for semi-Lagrangian diffusion computation
! LSLHD_W   : switch for SLHD of horizontal flow.
! LSLHD_T   : switch for SLHD of temperature.
! LSLHD_SPD : switch for SLHD of (NH) pressure departure vs geopotential var.
! LSLHD_SVD : switch for SLHD of (NH) vertical divergence vs 'gw' var.
! LSLHD_GFL : switch for SLHD of GFL var (at least one GFL var has SLHD).
! LSLHD_OLD : use old SLHD interpolator (cubic Lagrange mixed with linear)
! LSLHD_STATIC : do not diagnose kappa from horizontal flow deformation,
!                use static value instead:
!                  SLHDKMIN for undiffused fields
!                  SLHDKMAX for diffused fields
! LSLHDQUAD : internal model switch indicating need to precompute quadratic weights
! SLHDKMIN  : minimum value for the kappa function
! SLHDKMAX  : maximum value for the kappa function
! SLHDEPSH  : dimensionless strength of horizontal Laplacian smoothing
! SLHDEPSV  : dimensionless strength of vertical Laplacian smoothing
! LSLHDVER  : switch activating vertical dependency of SLHDEPSV to B coefficient

LOGICAL :: LSLHD
LOGICAL :: LSLHD_W
LOGICAL :: LSLHD_T
LOGICAL :: LSLHD_SPD
LOGICAL :: LSLHD_SVD
LOGICAL :: LSLHD_GFL
LOGICAL :: LSLHD_OLD
LOGICAL :: LSLHD_STATIC
LOGICAL :: LSLHDQUAD
LOGICAL :: LSLHDVER
REAL(KIND=JPRB) :: SLHDKMIN
REAL(KIND=JPRB) :: SLHDKMAX
REAL(KIND=JPRB) :: SLHDEPSH
REAL(KIND=JPRB) :: SLHDEPSV

! ------ Other diffusive processes ------------------------------------------

! LRFRIC     : .T. = Rayleigh friction in horizontal (zonal) wind.
! LRFRICISOTR: isotropic Rayleigh friction (acts on zonal and meridian horizontal wind).
! LGRADSP    : special switch for de-aliasing the pressure gradient term

LOGICAL :: LRFRIC
LOGICAL :: LRFRICISOTR
LOGICAL :: LGRADSP

! ------ Deep-layer equations: White and Bromley formulation ---------

! LVERCOR   : .T. deep layer formulation switched on.

LOGICAL :: LVERCOR

! ------ Deep-layer equations: Wood and Staniforth formulation ---------

! The Wood and Staniforth formulation of deep-layer equations is different
!  from the White and Bromley one (variable LVERCOR) and is well designed
!  for the versions of the NH model present in ARPEGE/IFS.

! LRWSDLW   : "w" inertia terms taken into account.
! LRWSDLR   : metric terms taken into account (elsewhere than in the
!             conversions between "dver" and "w").
! LRWSDLR2  : metric terms taken into account also in the conversions
!             between "dver" and "w" (requires LRWSDLR=T).
! LRWSDLG   : vertical variations of "g" taken into account (requires LRWSDLR=T)
! LCURVW    : takes account of inertial curvature term -(w/r) V in the RHS of wind equation,
!             if "w" terms are activated in the Coriolis term.
! NITPRHS   : number of iterations in GNH_CONV_PRHS (LRWSDLR=T) to find "r/a".
!             not relevant if LRWSDLR=F
! RC_PD1    : coefficient in factor of log(presim/prehyd).
!             This quantity is used in the PDVD NH model (NPDVAR=2),
!             for PD variable, when LRWSDLR=T.
!             In this case, we apply a correction in order to recover
!             a variable not too far from log(pre/prehyd).

LOGICAL :: LRWSDLW
LOGICAL :: LRWSDLR
LOGICAL :: LRWSDLR2
LOGICAL :: LRWSDLG
LOGICAL :: LCURVW
INTEGER(KIND=JPIM) :: NITPRHS
REAL(KIND=JPRB) :: RC_PD1

! ------ 3D turbulence ------------------------------------------------------

! L3DTURB   : main key to activate 3D turbulence

LOGICAL :: L3DTURB

! ------ Dynamics diagnostics -----------------------------------------------

! LSLDIA    : switch on semi-lagrangian dynamics diagnostics
! LRPRSLTRJ : more detailed printings in LARMES/ELARMES (SL displacement).

LOGICAL :: LSLDIA
LOGICAL :: LRPRSLTRJ

! ------ Semi-Lagrangian scheme ---------------------------------------------

! LRALTVDISP : alternate way to compute vertical displacement (semi-Lagrangian advection),
!              in order to avoid trajectories going out of the atmosphere.
! LVSPLIP    : .T. if vertical spline cubic SL interpolations.
! LCOMAD     : swith for COMAD (will be T if any variable needs COMAD interpolation).
! LCOMADH/LCOMADV : COMAD in the horizontal/vertical directions of interpolation
!                   same choice common for all COMAD interpolation
! LCOMAD_W   : COMAD for horizontal flow.
! LCOMAD_T   : COMAD for temperature.
! LCOMAD_SPD : COMAD for (NH) pressure departure vs geopotential var.
! LCOMAD_SVD : COMAD for (NH) vertical divergence vs 'gw' var.
! LCOMAD_SP  : COMAD for surface pressure.
! LCOMAD_GFL : COMAD for GFL (must be T if one of GFL attribut LCOMAD=T).
! LNESCT     : non-extrapolating horizontal displacement in SL2TL.
! LNESCV     : non-extrapolating vertical displacement in SL2TL.
! LNESC      : non-extrapolating RHS in SL2TL.
! LSETTLST   : stable extrapolating horizontal displacement in SL2TL.
! LSETTLSV   : stable extrapolating vertical displacement in SL2TL.
! LSETTLS    : stable extrapolating RHS in SL2TL.
! LELTRA     : alternate ``elegant" calculation of horizontal and vertical displacement in SL2TL.
! LSLINLC1   : separate linear terms from non-linear terms in continuity equation (case LRNHC1=T).
! LSLINLC2   : separate linear terms from non-linear terms in continuity equation (case (LGWADV,LSETTLS)=(T,T)).
! LSLINL     : separate linear terms from non-linear terms in other equations (case (LGWADV,LSETTLS)=(T,T)).

LOGICAL :: LRALTVDISP
LOGICAL :: LVSPLIP
LOGICAL :: LCOMAD
LOGICAL :: LCOMADH
LOGICAL :: LCOMADV
LOGICAL :: LCOMAD_W
LOGICAL :: LCOMAD_T
LOGICAL :: LCOMAD_SPD
LOGICAL :: LCOMAD_SVD
LOGICAL :: LCOMAD_SP
LOGICAL :: LCOMAD_GFL
LOGICAL :: LNESCT
LOGICAL :: LNESCV
LOGICAL :: LNESC
LOGICAL :: LSETTLST
LOGICAL :: LSETTLSV
LOGICAL :: LSETTLS
LOGICAL :: LELTRA
LOGICAL :: LSLINLC1
LOGICAL :: LSLINLC2
LOGICAL :: LSLINL

! ------ Vertical discretisation --------------------------------------------

! LAPRXPK : way of computing full-levels pressures in primitive equation
!           hydrostatic model.
!           .T.: full levels are computed by PK=(PK+1/2 + PK-1/2)*0.5
!           .F.: full levels are computed by a more complicated formula
!                consistent with "alpha" in geopotential formula.
! NDLNPR  : NDLNPR=0: conventional formulation of delta, i.e. ln(P(l)/P(l-1)).
!           NDLNPR=1: formulation of delta used in non hydrostatic model,
!                     i.e. (P(l)-P(l-1))/SQRT(P(l)*P(l-1)).
! RHYDR0  : value given to "alpha(1) = depth of log(Pi) between top and full level nr 1"
!           in case where general formula to compute "alpha" gives an infinite value
!           (used only if LVERTFE=F, NDLNPR=0).
!           This quantity is never used in the following cases:
!            LVERTFE=T.
!            LVERTFE=F with NDLNPR=1.
! LRUBC   : .T. if radiational upper boundary condition

LOGICAL :: LAPRXPK
INTEGER(KIND=JPIM) :: NDLNPR
REAL(KIND=JPRB) :: RHYDR0
LOGICAL :: LRUBC

! ------ PC (ICI) schemes ---------------------------------------------------

! LPC_FULL  : full PC scheme switch (with reiterations of trajectories)
! LPC_CHEAP : 'cheap PC scheme': when LPC_FULL=T and semi-Lagrangian advection,
!             the PC update is not done on the calculation of the SL trajectory.

LOGICAL :: LPC_FULL
LOGICAL :: LPC_CHEAP

! ----------------------------------------------------------------------
END MODULE YOMDYNA
