MODULE YOM_YGFL

USE PARKIND1 , ONLY : JPIM, JPRB
USE CRMDIMS   ,ONLY : CRMBUFSIZE

IMPLICIT NONE

!-------------------------------------------------------------------------
! Contains the descriptors of GFL arrays
!-------------------------------------------------------------------------

! JPGFL : Max number of GFL fields
! JPNAMED_GFL : Number of currently pre-defined components of GFL
! JPGHG : Number of greenhouse gas fields
! JPCHEM : Number of chemical species
! JPAERO : Number of active aerosol fields
! JPAEROUT: Number of output aerosol fields
! JPUVP : Number of output from UV processor
! JPTRAC : Number of tracers for diagnostics
! JPERA40 : Number of ERA40 diagnostic fields
! JPCH4S  : Number of added fields related to methane
! JPNOGW  : Number of diagnostic fields for NORO GWD SCHEME
! JPSLDIA : Number of SL dynamics diagnostic fields
! JPCHEM_ASSIM : Maximum number of assimilated of chemical species
! JPCRM : Number of CRM columns (prognostic variables space)
!-------------------------------------------------------------------------

INTEGER(KIND=JPIM), PARAMETER :: JPCRM=CRMBUFSIZE
INTEGER(KIND=JPIM), PARAMETER :: JPGFL=2163+JPCRM
INTEGER(KIND=JPIM), PARAMETER :: JPNAMED_GFL=27
INTEGER(KIND=JPIM), PARAMETER :: JPGHG=3
INTEGER(KIND=JPIM), PARAMETER :: JPTRAC=10
INTEGER(KIND=JPIM), PARAMETER :: JPCHEM=66
INTEGER(KIND=JPIM), PARAMETER :: JPCHEM_ASSIM=5
INTEGER(KIND=JPIM), PARAMETER :: JPAERO=16
INTEGER(KIND=JPIM), PARAMETER :: JPFORC=999
INTEGER(KIND=JPIM), PARAMETER :: JPERA40=14
INTEGER(KIND=JPIM), PARAMETER :: JPSLDIA=7
INTEGER(KIND=JPIM), PARAMETER :: JPEZDIAG=50
INTEGER(KIND=JPIM), PARAMETER :: JPCH4S=1
INTEGER(KIND=JPIM), PARAMETER :: JPNOGW=2
INTEGER(KIND=JPIM), PARAMETER :: JPAEROUT=17
INTEGER(KIND=JPIM), PARAMETER :: JPUVP=2
INTEGER(KIND=JPIM), PARAMETER :: JPPHYS=8   
INTEGER(KIND=JPIM), PARAMETER :: GRIB_CODE_GFL_PHYS=81  ! AJGDB hopefully harmless

TYPE TYPE_GFL_COMP ! Individual field descriptor

SEQUENCE ! daand: necessary to avoid memory corruption with gfortran 4.3.3

CHARACTER(LEN=16)  :: CNAME     = ''        ! ARPEGE field name 
INTEGER(KIND=JPIM) :: IGRBCODE  = -999      ! GRIB code
LOGICAL            :: LADV      = .FALSE.   ! Field advected or not
LOGICAL            :: LADV5     = .FALSE.   ! Field advected without wind increments
LOGICAL            :: LTDIABLIN = .FALSE.   ! Diabatic tendency is interpolated by lin. int.
LOGICAL            :: LHORTURB  = .FALSE.   ! Horizontal part affected by 3D turbulence
INTEGER(KIND=JPIM) :: NREQIN    = 0         ! 1 if field requiered in input, 0 if not, -1 if initialised
                                            ! with a reference value REFVALI
LOGICAL            :: LREQOUT   = .FALSE.   ! T if field requiered in output
LOGICAL            :: LGPINGP   = .TRUE.    ! GP field input as GP
LOGICAL            :: LGP       = .FALSE.   ! Field exists and of grid-point type
LOGICAL            :: LSP       = .FALSE.   ! Field exists and of spectral type
LOGICAL            :: LCDERS    = .FALSE.   ! Derivatives required (spectral only)
LOGICAL            :: LACTIVE   = .FALSE.   ! Field in use
LOGICAL            :: LTHERMACT = .FALSE.   ! Field thermodynamically active
REAL(KIND=JPRB)    :: R         = 0.0_JPRB
REAL(KIND=JPRB)    :: RCP       = 0.0_JPRB
LOGICAL            :: LT9       = .FALSE.   ! Field in t-dt GFL
LOGICAL            :: LT1       = .FALSE.   ! Field in t+dt GFL
LOGICAL            :: LT5       = .FALSE.   ! Field in trajectory GFL
LOGICAL            :: LPHY      = .FALSE.   ! Field in physics GFL
LOGICAL            :: LPT       = .FALSE.   ! Field in PC phy. tend. GFL (GFLPT)
LOGICAL            :: LTRAJIO   = .FALSE.   ! Field written to and from trajectory structure
LOGICAL            :: LDIAG     = .FALSE.   ! Field is "diagnostic" at t; e.g. cloud fraction 
LOGICAL            :: LPC       = .FALSE.   ! Field in predictor/corrector time stepping (GFLPC)
REAL(KIND=JPRB)    :: REFVALI   = 0.0_JPRB  ! Reference value for init, used in case NREQIN==-1
! LAM specific attributes (Arome/Aladin)
LOGICAL            :: LADJUST0  = .FALSE.   ! True if field is thermodynamically adjusted at t
                                            ! (immediatly after inverse spectral transforms)
LOGICAL            :: LADJUST1  = .FALSE.   ! True if field is thermodynamically adjusted at t+dt
                                            ! (after SL interpolations and NL residuals)
INTEGER(KIND=JPIM) :: NCOUPLING = 0         ! 1 if field is coupled by Davies relaxation, 0 if not,
                                            ! -1 if coupled with reference value for coupling REFVALC
REAL(KIND=JPRB)    :: REFVALC   = 0.0_JPRB  ! Reference value for coupling, used in case NCOUPLING==-1
LOGICAL            :: LBIPER    = .FALSE.   ! True if field must be biperiodised inside the transforms
! End LAM specific attributes (Arome/Aladin)
CHARACTER(LEN=12)  :: CSLINT    = ''        ! S.L interpolaion "type"
INTEGER(KIND=JPIM) :: MP        = -99999999 ! Basic field "pointer"
INTEGER(KIND=JPIM) :: MPL       = -99999999 ! zonal derivative "pointer"
INTEGER(KIND=JPIM) :: MPM       = -99999999 ! Meridional derivative "pointer"
INTEGER(KIND=JPIM) :: MP9       = -99999999 ! Basic field "pointer" t-dt
INTEGER(KIND=JPIM) :: MP9_PH    = -99999999 ! Basic field "pointer" for Physics
INTEGER(KIND=JPIM) :: MP1       = -99999999 ! Basic field "pointer" t+dt
INTEGER(KIND=JPIM) :: MP5       = -99999999 ! Basic field "pointer" trajectory
INTEGER(KIND=JPIM) :: MP5L      = -99999999 ! zonal derivative "pointer" trajectory
INTEGER(KIND=JPIM) :: MP5M      = -99999999 ! Meridional derivative "pointer" trajectory
INTEGER(KIND=JPIM) :: MPSLP     = -99999999 ! Basic field "pointer" physics
INTEGER(KIND=JPIM) :: MPSP      = -99999999 ! Basic field "pointer" spectral space
INTEGER(KIND=JPIM) :: MP_SPL    = -99999999 ! Basic field "pointer" spline interpolation
INTEGER(KIND=JPIM) :: MP_SL1    = -99999999 ! Basic field "pointer" in SLBUF1
INTEGER(KIND=JPIM) :: MP_SLX    = -99999999 ! Basic field "pointer" in SLBUF1 for CPG_PT
INTEGER(KIND=JPIM) :: MPPT      = -99999999 ! Physics tendency "pointer"
INTEGER(KIND=JPIM) :: MPPC      = -99999999 ! Predictor/corrector auxiliary array "pointer"

! daand: INTFLEX attributes
LOGICAL            :: LWATER                ! TRUE for water species
LOGICAL            :: LPRECIP               ! TRUE for precipitating water species
REAL(KIND=JPRB)    :: RLZER                 ! Latent heat change at 0K

! gems nl ext
INTEGER(KIND=JPIM) :: NCOUPLO4              ! Coupled to CTM by OASIS4 intefrace
LOGICAL            :: LASSIM                ! use as Control Variable (either monitored or assimilated)
INTEGER(KIND=JPIM) :: IGRIBDV               ! GRIB code of deposition velocity 
INTEGER(KIND=JPIM) :: IGRIBTC               ! GRIB code of Total Column
INTEGER(KIND=JPIM) :: IGRIBSFC              ! GRIB code of Surface Flux 
LOGICAL            :: LDIFF                 ! Diffusion  on
LOGICAL            :: LCONV                 ! Convection on
REAL(KIND=JPRB)    :: RMOLMASS              ! Molar Mass 
REAL(KIND=JPRB)    :: REFOLD                ! Efolding decay time 
REAL(KIND=JPRB)    :: HENRYA                ! Henry constant a 
REAL(KIND=JPRB)    :: HENRYB                ! Henry constant b 
LOGICAL            :: LNEGFIX               ! Cut off negative values in sugridug an
LOGICAL            :: LCOMAD                ! COMAD interpolation for GFL component
LOGICAL            :: LMASSFIX              ! Correct mass error of sl advection in gpmodel (if LTRCMFIX)
REAL(KIND=JPRB)    :: BETAMFBC              ! beta parameters for BC mass fixer (if LMASSFIX)
TYPE(TYPE_GFL_COMP),POINTER :: PREVIOUS     ! Pointer to previously def. field

END TYPE TYPE_GFL_COMP

TYPE TYPE_GFL_NAML ! Individual field descriptor for namelist input

SEQUENCE ! daand: necessary to avoid memory corruption with gfortran 4.3.3

CHARACTER(LEN=16)  :: CNAME     ! ARPEGE field name 
INTEGER(KIND=JPIM) :: IGRBCODE  ! GRIB code
INTEGER(KIND=JPIM) :: NREQIN    ! 1 if field required in input, 0 if not, -1 if initialised
                                ! with a reference value REFVALI
REAL(KIND=JPRB) :: REFVALI      ! Reference value for initialisation, used in case NREQIN==-1
LOGICAL :: LREQOUT              ! T if field requiered in output
LOGICAL :: LGPINGP              ! GP field input as GP
LOGICAL :: LGP                  ! Field exists and of grid-point type
LOGICAL :: LSP                  ! Field exists and of spectral type
LOGICAL :: LCDERS               ! Derivatives required (spectral only)
LOGICAL :: LT9                  ! Field in t-dt GFL
LOGICAL :: LT1                  ! Field in t+dt GFL
LOGICAL :: LT5                  ! Field in trajectory GFL
LOGICAL :: LPHY                 ! Field with physics tendencies GFL
LOGICAL :: LPT                  ! Field in PC physics tendency GFLPT
LOGICAL :: LTRAJIO              ! Field written to and from trajectory structure
LOGICAL :: LDIAG                ! Field is "diagnostic" at t; e.g. cloud fraction 
LOGICAL :: LPC                  ! Field in predictor/corrector time stepping GFLPC
LOGICAL :: LADV                 ! Field advected or not
LOGICAL :: LADV5                ! Field advected without wind increments
LOGICAL :: LINTLIN              ! Linear interpolation for field
LOGICAL :: LTDIABLIN            ! Diabatic tendency is interpolated by linear int.
LOGICAL :: LHORTURB             ! Horizontal part affected by 3D turbulence
LOGICAL :: LQM                  ! quasi-monotonous interpolation for field
LOGICAL :: LQMH                 ! quasi-monotonous interpolation in horizontal for field
LOGICAL :: LQM3D                ! quasi-monotone interpolation applied directly in 3 dimensions
LOGICAL :: LSLHD                ! Semi-lagrangian horizontal diffusion used for field
LOGICAL :: LCOMAD               ! COMAD weights used for SL interpolation of field
LOGICAL :: LHV                  ! Hermite vertical interpolation used for field (only ozone sofar)
LOGICAL :: LVSPLIP              ! vertical spline interpolation used for field (only ozone sofar)
INTEGER(KIND=JPIM) :: NCOUPLING ! 1 if field is coupled by Davies relaxation, 0 if not,
                                ! -1 if coupled with reference value for coupling REFVALC
REAL(KIND=JPRB) :: REFVALC      ! Reference value for coupling, used in case 
                                ! NCOUPLING==-1
! gems nl ext
INTEGER(KIND=JPIM)  :: NCOUPLO4 ! Coupled to CTM by OASIS4 intefrace =1 input,=2 in&output,=-1 none
LOGICAL             :: LASSIM   ! use as Control Variable (either monitored or assimilated)
INTEGER(KIND=JPIM)  :: IGRIBDV  ! GRIB code of deposition velocity 
INTEGER(KIND=JPIM)  :: IGRIBTC  ! GRIB code of Total Column
INTEGER(KIND=JPIM)  :: IGRIBSFC ! GRIB code of Surface Flux 
LOGICAL             :: LDIFF    ! Diffusion  on
LOGICAL             :: LCONV    ! Convection on
LOGICAL             :: LNEGFIX  ! Cut off negative values in sugridug and callpar
LOGICAL             :: LMASSFIX ! Correct mass error of sl advection in gpmodel (if LTRCMFIX)
REAL(KIND=JPRB)     :: BETAMFBC ! beta parameters for BC mass fixer (if LMASSFIX)
REAL(KIND=JPRB)     :: RMOLMASS ! Molar Mass 
REAL(KIND=JPRB)     :: REFOLD   ! Efolding  decay time 
REAL(KIND=JPRB)     :: HENRYA   ! Henry constant a 
REAL(KIND=JPRB)     :: HENRYB   ! Henry constant b 

END TYPE TYPE_GFL_NAML

!-------------------------------------------------------------------------
! Derived types for describing the GFL structure.
!-------------------------------------------------------------------------
! Modifications:
! 03/07/09 C. Fischer - add Arome/Aladin attributes
! 03/10/01 C. Moussy  - add Arome/Aladin attributes coupling
! 03/10/31 M. Tudor   - add physics tendencies for predictor-corrector
! 05/10/10 J. Haseler - switch for I/O to trajectory structure
! 2004-Nov F. Vana    - update of CSLINT attribute
! 20-Feb-2005 Vivoda  - 3TL Eul PC scheme (GFLPC)
! 07/06/27 E. Holm    - TL/AD advection without wind increments LADV5
! 12/04/08 J. Flemming - GFL attribute extention for GEMS 
! 22-Feb-11 F. Vana   - LTDIABLIN and LHORTURB
! spring 2011 ECMWF   - LINTLIN
! Nov. 2013           - LCOMAD
! 2013-11, D. Degrauwe - INTFLEX attributes
! 06-Feb-2015 F. Vana & M. Kharoutdinov - super-parametrization fields.

TYPE TYPE_GFLD

SEQUENCE ! daand: necessary to avoid memory corruption with gfortran 4.3.3

! Overall descriptor,dimensioning etc.
INTEGER(KIND=JPIM) :: NUMFLDS     = 0  ! Number of GFL fields
INTEGER(KIND=JPIM) :: NDERS       = 0  ! Number of horizontal derivatives fields
INTEGER(KIND=JPIM) :: NUMSPFLDS   = 0  ! Number of spectrally represented GFL fields
INTEGER(KIND=JPIM) :: NUMGPFLDS   = 0  ! Number of grid-point GFL fields
INTEGER(KIND=JPIM) :: NUMFLDS9    = 0  ! Number of GFL fields in (t-dt) part
INTEGER(KIND=JPIM) :: NUMFLDS1    = 0  ! Number of GFL fields in (t+dt) array
INTEGER(KIND=JPIM) :: NUMSPFLDS1  = 0  ! Number of spectrally represented GFL fields (t+dt)
INTEGER(KIND=JPIM) :: NUMFLDS5    = 0  ! Number of GFL fields (trajectory)
INTEGER(KIND=JPIM) :: NUMFLDSPHY  = 0  ! Number of GFL fields (phys.)
INTEGER(KIND=JPIM) :: NUMFLDS_SPL = 0  ! Number of GFL fields (S.L. spline interpolation)
INTEGER(KIND=JPIM) :: NUMFLDS_SL1 = 0  ! Number of GFL fields in S.L. buffer 1
INTEGER(KIND=JPIM) :: NUMFLDSPC   = 0  ! Number of GFL fields (predictor/corrector)
INTEGER(KIND=JPIM) :: NDIM        = 0  ! Dimension of main array holding GFL fields(GFL)
INTEGER(KIND=JPIM) :: NUMFLDSPT   = 0  ! Number of GFL fields (phy. tend.)
INTEGER(KIND=JPIM) :: NDIM0       = 0  ! Dimension of t0 part of GFL
INTEGER(KIND=JPIM) :: NDIM9       = 0  ! Dimension of t-dt part of GFL
INTEGER(KIND=JPIM) :: NDIM1       = 0  ! Dimension of t+dt array (GFLT1)
INTEGER(KIND=JPIM) :: NDIM5       = 0  ! Dimension of traj. GFL array (GFL5)
INTEGER(KIND=JPIM) :: NDIMSLP     = 0  ! Diminsion of S.L. phys. GFL array (GFLSLP)
INTEGER(KIND=JPIM) :: NDIM_SPL    = 0  ! Dim. of arrays holding GFL fields (S.L.spline int.)
INTEGER(KIND=JPIM) :: NDIMPT      = 0  ! Dimension of phy. tend. GFL array (GFLPT)
INTEGER(KIND=JPIM) :: NDIMPC      = 0  ! Dimension of iterative scheme auxiliary array (GFLPC)

INTEGER(KIND=JPIM) :: NGFL_EXT
INTEGER(KIND=JPIM) :: NGFL_FORC
INTEGER(KIND=JPIM) :: NGFL_EZDIAG
INTEGER(KIND=JPIM) :: NGHG
INTEGER(KIND=JPIM) :: NTRAC
INTEGER(KIND=JPIM) :: NAERO
INTEGER(KIND=JPIM) :: NACTAERO
INTEGER(KIND=JPIM) :: NDDHAERO
INTEGER(KIND=JPIM) :: NERA40
INTEGER(KIND=JPIM) :: NNOGW
INTEGER(KIND=JPIM) :: NAEROUT
INTEGER(KIND=JPIM) :: NUVP
INTEGER(KIND=JPIM) :: NSLDIA
INTEGER(KIND=JPIM) :: NSLDIAGP
INTEGER(KIND=JPIM) :: NGFL_PHYS
INTEGER(KIND=JPIM) :: NCRM
LOGICAL :: LGHGRTTOV
LOGICAL :: LCO2SFC
LOGICAL :: LCH4SFC
LOGICAL :: LAEROSFC
LOGICAL :: LFIRE
LOGICAL :: LINJ           ! Use of injection height for bb emissions of aerosols
LOGICAL :: LINJ_CHEM      ! Use of injection height for bb emissions of GRG/GHG
LOGICAL :: LAERODIU
LOGICAL :: LTRCMFIX       ! Activates tracer mass fixer
LOGICAL :: LTRCMFIX_PS    ! Adjust pressure to conserve dry mass in mass fixer calculations
LOGICAL :: LAEROUT
LOGICAL :: LUVPOUT
LOGICAL :: LCHEM

INTEGER(KIND=JPIM) :: NGEMS   ! The total number of "GEMS" fields.
INTEGER(KIND=JPIM) :: NCHEM
INTEGER(KIND=JPIM) :: NCHEM_ASSIM
INTEGER(KIND=JPIM) :: NCHEM_FLX 
INTEGER(KIND=JPIM) :: NCHEM_DV
INTEGER(KIND=JPIM) :: NCHEM_TC
INTEGER(KIND=JPIM) :: NCHEM_SCV

!     ------------------------------------------------------------------
!      Mass fixers
!     ------------------------------------------------------------------
INTEGER(KIND=JPIM) :: NNEGAFIX     ! Num of fields to apply -ve fixer
INTEGER(KIND=JPIM) :: NOPTNEGFIX   ! 1: simple negative fixer (reset to 0)
                                   ! 2: reset to local minimum

LOGICAL :: LQM3DCONS      ! Bermejo & Staniforth quasi-monotone limiter with improved
                          ! conservation option. When true, applied to all GFL s.t. LQM3D=true
LOGICAL :: LADVNEGFIX              ! Activates negative fixer for advection
LOGICAL :: LTRCMFBC                ! Activate Bermejo & Conde mass fixer if true
LOGICAL :: LTRCMFPR                ! Activate Priestley mass fixer if true
LOGICAL :: LTRCMFMG                ! Activate Mac Gregor's mass fixer if true
LOGICAL :: LTRCMFP                 ! Activate proportional mass fixer  
LOGICAL :: LTRCMFA_DIF             ! Activate additive mass fixer based on t - t+1 diff 
LOGICAL :: LTRCMFA_LAP             ! Activate additive mass fixer based on horizontal laplace 
LOGICAL :: LTRCMFA_VER             ! Activate additive mass fixer based on vertical laplace

LOGICAL :: LEXTRADF                ! Extra diagnostics 


INTEGER(KIND=JPIM) :: NFLDSFIX     ! Number of fields to be fixed
INTEGER(KIND=JPIM) :: NOPTMFBC     ! Bermejo & Conde fixer version
INTEGER(KIND=JPIM) :: NOPTMFPR     ! Priestley fixer version
INTEGER(KIND=JPIM) :: NOPTVFE      ! Use Vertical FE in calculation of column mass total
INTEGER(KIND=JPIM) :: NMFDIAGLEV   ! Determines global diagnostic output level for fixer:
                                   ! 0 - nothing, 1 - norms printed, 2 - norms + monotonicity
INTEGER(KIND=JPIM) :: NMFIXFLDS(JPNAMED_GFL+JPGHG+JPCHEM+JPAERO+JPTRAC) 
                                   ! Index of fields to be corrected by mass fixers
INTEGER(KIND=JPIM) :: NNEGFLDS(JPNAMED_GFL+JPGHG+JPCHEM+JPAERO+JPTRAC)  
                                   ! Index of fields to be corrected by SL -ve fixer
REAL(KIND=JPRB)    :: ZMFIXEPS     ! Threshold for mass fixing scheme

TYPE(TYPE_GFL_COMP), POINTER  :: YCOMP(:)    ! General descriptor of all components

TYPE(TYPE_GFL_COMP)          :: YQ          
TYPE(TYPE_GFL_COMP)          :: YI          
TYPE(TYPE_GFL_COMP)          :: YL          
TYPE(TYPE_GFL_COMP)          :: YS          
TYPE(TYPE_GFL_COMP)          :: YR          
TYPE(TYPE_GFL_COMP)          :: YG          
TYPE(TYPE_GFL_COMP)          :: YCVGQ       

END TYPE TYPE_GFLD

! GFL general descriptor
TYPE(TYPE_GFLD) :: YGFL 

END MODULE YOM_YGFL
