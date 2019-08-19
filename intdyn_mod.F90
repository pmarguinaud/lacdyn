MODULE INTDYN_MOD

! Purpose :
! -------
!    To define and compute pointers and logical conditions used when
!    computing local quantities in the dynamics.
!    Allows to use some global structures, for example under CPG
!    (and also their TL and AD).

! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    K. YESSAD (CNRM/GMAP)
!    Original : January 2011

! Modifications :
! -------------
!  K. YESSAD (Feb 2014): some structures have been moved in INTDYNSL_MOD.
!  F. Vana 13-Feb-2014  SLHD weights for heat variables
!-----------------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

IMPLICIT NONE
SAVE

!=============================================================================

!      1.    TYPE DEFINITIONS
!            ----------------

!      1.01  Type TYXB: pointers for output of GPXYB.

! ky: M_RPREF for "1/(full level pressure)" could be later added to this structure
TYPE TXYB
INTEGER(KIND=JPIM) :: M_DELP        ! pressure depths at full levels
INTEGER(KIND=JPIM) :: M_RDELP       ! 1/(pressure depths) at full levels
INTEGER(KIND=JPIM) :: M_LNPR        ! "delta = depth of log(pressure)" at full levels
INTEGER(KIND=JPIM) :: M_ALPH        ! "alpha" at full levels
INTEGER(KIND=JPIM) :: M_RTGR        ! ratio "rtgr": grad(pressure)/pressure = "rtgr" grad(pressure_surf)
INTEGER(KIND=JPIM) :: M_RPRE        ! 1/(half level pressure)
INTEGER(KIND=JPIM) :: M_RPP         ! 1/(pressure(lbar)*pressure(lbar-1)), where lbar=half level
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TXYB

!      1.02  Type TXYBDER: pointers for output of GPGRXYB.

TYPE TXYBDER
INTEGER(KIND=JPIM) :: M_LNPRL       ! zonal derivative of grad(delta) at full levels
INTEGER(KIND=JPIM) :: M_LNPRM       ! meridian derivative of grad(delta) at full levels
INTEGER(KIND=JPIM) :: M_ALPHL       ! zonal derivative of grad(alpha) at full levels
INTEGER(KIND=JPIM) :: M_ALPHM       ! meridian derivative of grad(alpha) at full levels
INTEGER(KIND=JPIM) :: M_ALPHPLL     ! zonal derivative of grad(alpha + log prehyd) at full levels
INTEGER(KIND=JPIM) :: M_ALPHPLM     ! meridian derivative of grad(alpha + log prehyd) at full levels
INTEGER(KIND=JPIM) :: M_COEFD       ! coefficient to compute grad(delta)
INTEGER(KIND=JPIM) :: M_COEFA       ! coefficient to compute grad(alpha)
INTEGER(KIND=JPIM) :: M_COEFAPL     ! coefficient to compute grad(alpha + log prehyd)
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TXYBDER

!      1.03  Type TRCP: pointers for output of GPRCP.

TYPE TRCP
INTEGER(KIND=JPIM) :: M_CP          ! "Cp" at full levels
INTEGER(KIND=JPIM) :: M_R           ! "R" at full levels
INTEGER(KIND=JPIM) :: M_KAP         ! "Kap = R/Cp" at full levels
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TRCP

!      1.04  Type TVERCOR: pointers for output of GPVC.. routines (option LVERCOR=T).

TYPE TVERCOR
INTEGER(KIND=JPIM) :: M_VCRSSAF     ! "rs/a" at full levels
INTEGER(KIND=JPIM) :: M_VCRSSAH     ! "rs/a" at half levels
INTEGER(KIND=JPIM) :: M_VCASRSF     ! "a/rs" at full levels
INTEGER(KIND=JPIM) :: M_VCASRSH     ! "a/rs" at half levels
INTEGER(KIND=JPIM) :: M_VCRSSAFL    ! zonal derivative of "rs/a" at full levels
INTEGER(KIND=JPIM) :: M_VCRSSAFM    ! meridian derivative of "rs/a" at full levels
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TVERCOR

!      1.05  Type TNHPRE: pointers for output of GNHPRE, GNHPREH, GNHGRPRE.

! ??? expected to be coded later
!     attributes: at least NHPREF, NHPREH, RNHPPI, QCHAL, QCHAM

!      1.06  Type TWSDLR: pointers for output of GNHDLR and GNHGRDLR.

! ??? expected to be coded later, to replace current par_dlr

!      1.07  Type TGEO: pointers for output of GPGEO and GPGRGEO.

! ??? expected to be coded later
!     attributes: at least PHIH, PHIF, PHIHL, PHIHM, PHIFL, PHIFM

!      1.08  Type TCTY: pointers for output of GPCTY.

TYPE TCTY
INTEGER(KIND=JPIM) :: M_EVEL        ! etadot (d prehyd / d eta)
INTEGER(KIND=JPIM) :: M_VVEL        ! omega / prehyd
INTEGER(KIND=JPIM) :: M_PSDIV       ! vertical integral of divergence without the "lrubc" contrib
INTEGER(KIND=JPIM) :: M_PSDVBC      ! vertical integral of divergence with the "lrubc" contrib
INTEGER(KIND=JPIM) :: M_DIVDP       ! thin layer eqns: grad(vec(V) * (Delta prehyd))
                                    ! lvercor=T: "grad((rs/a)*vec(V)*(Delta prehyd))"
                                    ! lrwsdlr=T: "(a/r) grad((a/r)*vec(V)*(Delta presim))"
                                    !  "grad" is the semi-reduced gradient ((r/a)nabla).
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TCTY

!      1.09  Type THWIND: pointers for half-level horizontal wind.

TYPE THWIND
INTEGER(KIND=JPIM) :: M_UH          ! U-wind at half levels
INTEGER(KIND=JPIM) :: M_VH          ! V-wind at half levels
INTEGER(KIND=JPIM) :: M_WWI         ! weights to compute half levels winds
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE THWIND

!      1.10  Types TTND: pointer for Lagrangian adiabatic tendencies.

TYPE TTND
INTEGER(KIND=JPIM) :: M_TNDU        ! tendency for U-wind equation
INTEGER(KIND=JPIM) :: M_TNDV        ! tendency for V-wind equation
INTEGER(KIND=JPIM) :: M_TNDU_NOC    ! tendency for U-wind equation without Coriolis term
INTEGER(KIND=JPIM) :: M_TNDV_NOC    ! tendency for V-wind equation without Coriolis term
INTEGER(KIND=JPIM) :: M_TNDASRU     ! tendency for (a/r)*U-wind equation
INTEGER(KIND=JPIM) :: M_TNDASRV     ! tendency for (a/r)*V-wind equation
INTEGER(KIND=JPIM) :: M_TNDT        ! tendency for temperature
INTEGER(KIND=JPIM) :: M_TNDPD       ! tendency for pressure departure variable
INTEGER(KIND=JPIM) :: M_TNDVD       ! tendency for vertical divergence variable
INTEGER(KIND=JPIM) :: M_TNDGW       ! tendency for Gw
INTEGER(KIND=JPIM) :: NDIM          ! total number of fields allocated
END TYPE TTND

!      1.13  Type TGMVT: pointers for GMV trajectory under CPG5_GP.

TYPE TGMVT
INTEGER(KIND=JPIM) :: M_U      ! U-component of horizontal wind
INTEGER(KIND=JPIM) :: M_V      ! V-component of horizontal wind
INTEGER(KIND=JPIM) :: M_T      ! temperature
INTEGER(KIND=JPIM) :: M_DIV    ! horizontal divergence
INTEGER(KIND=JPIM) :: M_SPD    ! pressure departure variable
INTEGER(KIND=JPIM) :: M_SVD    ! vertical divergence variable
INTEGER(KIND=JPIM) :: NDIM     ! total number of fields allocated
END TYPE TGMVT

!      1.14  Type TGFLT: pointers for GFL trajectory under CPG5_GP.

TYPE TGFLT
INTEGER(KIND=JPIM) :: M_Q      ! specific humidity
INTEGER(KIND=JPIM) :: M_L      ! liquid water
INTEGER(KIND=JPIM) :: M_I      ! ice
INTEGER(KIND=JPIM) :: NDIM     ! total number of fields allocated
END TYPE TGFLT

!      1.15  other possible structures to be introduced later:
!            * TRT (RT,RTL,RTM)

!=============================================================================

!      2.    DECLARATIONS
!            ------------

!      2.01  Type TYXB.

TYPE(TXYB) :: YYTXYB0         ! at t
TYPE(TXYB) :: YYTXYB5         ! at t (trajectory)
TYPE(TXYB) :: YYTXYB9         ! at t-dt
TYPE(TXYB) :: YYTXYB95        ! at t-dt (trajectory)
TYPE(TXYB) :: YYTXYB0_PHY     ! output of MF_PHYS_PREP at t
TYPE(TXYB) :: YYTXYB9_PHY     ! output of MF_PHYS_PREP at t-dt
TYPE(TXYB) :: YYTXYBPP        ! for POS
! ky: YYTXYB: may be introduced later in GPXYB, GPGRXYB, GPCTY, GPGRP, GNHDLRB too
TYPE(TXYB) :: YYTXYB          ! for GP.. routines
TYPE(TXYB) :: YYTXYBT         ! for GP.. routines (trajectory)

!      2.02  Type TXYBDER.

TYPE(TXYBDER) :: YYTXYBDER0   ! at t
TYPE(TXYBDER) :: YYTXYBDER5   ! at t (trajectory)
TYPE(TXYBDER) :: YYTXYBDERPP  ! for POS
TYPE(TXYBDER) :: YYTXYBDER    ! for GP.. routines
TYPE(TXYBDER) :: YYTXYBDERT   ! for GP.. routines (trajectory)

!      2.03  Type TRCP:

TYPE(TRCP) :: YYTRCP0   ! at t
TYPE(TRCP) :: YYTRCP5   ! at t (trajectory)
TYPE(TRCP) :: YYTRCP9   ! at t-dt
TYPE(TRCP) :: YYTRCP95  ! at t-dt (trajectory)

!      2.04  Type TVERCOR.

TYPE(TVERCOR) :: YYTVC0       ! at t
TYPE(TVERCOR) :: YYTVC1       ! at t+dt
TYPE(TVERCOR) :: YYTVCPP      ! for POS
TYPE(TVERCOR) :: YYTVC        ! for GP.. routines

!      2.05  Type TNHPRE.

! ??? expected to be coded later

!      2.06  Type TWSDLR.

! ??? expected to be coded later

!      2.07  Type TGEO.

! ??? expected to be coded later

!      2.08  Type TCTY.

TYPE(TCTY) :: YYTCTY0        ! at t
TYPE(TCTY) :: YYTCTY5        ! at t (trajectory)
TYPE(TCTY) :: YYTCTYPP       ! for POS
TYPE(TCTY) :: YYTCTY         ! for GP.. routines

!      2.09  Type THWIND.

TYPE(THWIND) :: YYTHW0       ! at t
TYPE(THWIND) :: YYTHW9       ! at t-dt
TYPE(THWIND) :: YYTHW5       ! at t (trajectory)
TYPE(THWIND) :: YYTHW95      ! at t-dt (trajectory)
TYPE(THWIND) :: YYTHWPP      ! for POS
TYPE(THWIND) :: YYTHW        ! for GP.. routines

!      2.10  Type TTND.

TYPE(TTND) :: YYTTND

!      2.13  Type TGMVT.

TYPE(TGMVT) :: YYTGMVT95

!      2.14  Type TGFLT.

TYPE(TGFLT) :: YYTGFLT95

!=============================================================================

END
