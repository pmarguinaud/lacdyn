MODULE YOMPARAR

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TPARAR
!*
!     ------------------------------------------------------------------

!     VARIABLES pour utiliser la PHYSIQUE de meso_NH :
!     VARIABLES to use the MESO-NH physics:

CHARACTER(LEN=4)   :: CMICRO      ! Microphysics scheme (ICE3 or ICE4)
CHARACTER(LEN=4)   :: CSEDIM      ! Microphysics sedimentation scheme
                                  ! (SPLI or STAT)
INTEGER(KIND=JPIM) :: NSPLITR     ! Time splitting for Eulerian sedimentation
INTEGER(KIND=JPIM) :: NRR, NRRL, NRRI   !number of microphysical species
INTEGER(KIND=JPIM) :: NSV         !number of passiv variables in MesoNH,
                                  ! always 0 in AROME
INTEGER(KIND=JPIM) :: NSWB_MNH    !number of SW bands for surface
                                  ! (must be equal to NSW !!)
INTEGER(KIND=JPIM) :: NGPAR       !number of fields in the buffer containing
                                  ! the 2D pseudo-historical variables.
INTEGER(KIND=JPIM) :: MINPRR      !pointer on INPRR 
INTEGER(KIND=JPIM) :: MACPRR      !pointer on ACPRR 
INTEGER(KIND=JPIM) :: MINPRS      !pointer on INPRS
INTEGER(KIND=JPIM) :: MACPRS      !pointer on ACPRS
INTEGER(KIND=JPIM) :: MINPRG      !pointer on INPRG
INTEGER(KIND=JPIM) :: MACPRG      !pointer on ACPRG
INTEGER(KIND=JPIM) :: MALBDIR     !pointer on ALBDIR
INTEGER(KIND=JPIM) :: MALBSCA     !pointer on ALBSCA
INTEGER(KIND=JPIM) :: MRAIN       !pointer on surface rain
INTEGER(KIND=JPIM) :: MSNOW       !pointer on surface snow
INTEGER(KIND=JPIM) :: MGZ0        !pointer on GZ0 
INTEGER(KIND=JPIM) :: MGZ0H       !pointer on GZ0H 
INTEGER(KIND=JPIM) :: MVQS        !pointer on surface moisture
INTEGER(KIND=JPIM) :: MVTS        !pointer on surface temperature 
INTEGER(KIND=JPIM) :: MVEMIS      !pointer on surface emissivity
INTEGER(KIND=JPIM) :: MSWDIR      !pointer on SW direct surface flux
INTEGER(KIND=JPIM) :: MSWDIF      !pointer on SW surface diffuse flux
INTEGER(KIND=JPIM) :: MFRTHDS     !pointer on IR downward surface flux
INTEGER(KIND=JPIM) :: MLSM        !pointer on land-sea mask

REAL(KIND=JPRB), DIMENSION(:), ALLOCATABLE  :: XSW_BANDS  !SW spectral bands
! for ext. surface scheme
LOGICAL :: LOSUBG_COND ! switch to activate subgrid condensation
LOGICAL :: LOSEDIC     ! activate cloud sedimentation
LOGICAL :: LOWARM      ! see OWARM in mesoNH
LOGICAL :: LOSIGMAS    ! see OSIGMAS in mesoNH
LOGICAL :: LOLSMC      ! Land/sea mask for cloud droplet number conc. 
LOGICAL :: LOTOWNC     ! Town mask for cloud droplet number conc.

LOGICAL :: LOCND2      ! Separate solid and liquid phase
REAL(KIND=JPRB) :: RADGR,RADSN  ! Tuning of ice for radiation, TO BE REMOVED

REAL(KIND=JPRB) :: VSIGQSAT ! coeff applied to qsat variance contribution
                            ! for subgrid condensation

! switches for MF scheme (Pergaud et al)
CHARACTER (LEN=4)  :: CMF_UPDRAFT  ! Type of Mass Flux Scheme
                                     ! 'NONE' if no parameterization
CHARACTER (LEN=4)  :: CMF_CLOUD  ! type of cloud scheme associated with MF Scheme
                                     ! 'NONE', 'DIRE' or 'STAT'
LOGICAL            :: LMIXUV    ! True if mixing of momentum


! Tuning variables for MF scheme 

REAL(KIND=JPRB) :: XALP_PERT    ! coefficient for the perturbation of
                                ! theta_l and r_t at the first level of 
                                ! the updraft
REAL(KIND=JPRB) :: XABUO        ! coefficient of the buoyancy term in the w_up equation  
REAL(KIND=JPRB) :: XBENTR       ! coefficient of the entrainment term in the w_up equation
REAL(KIND=JPRB) :: XBDETR       ! coefficient of the detrainment term in the w_up equation
REAL(KIND=JPRB) :: XCMF         ! coefficient for the mass flux at the first level 
                                ! of the updraft (closure) 
REAL(KIND=JPRB) :: XENTR_MF     ! entrainment constant (m/Pa) = 0.2 (m)
REAL(KIND=JPRB) :: XCRAD_MF     ! cloud radius in cloudy part
REAL(KIND=JPRB) :: XENTR_DRY    ! coefficient for entrainment in dry part 
REAL(KIND=JPRB) :: XDETR_DRY    ! coefficient for detrainment in dry part
REAL(KIND=JPRB) :: XDETR_LUP    !  coefficient for detrainment in dry part
REAL(KIND=JPRB) :: XKCF_MF      ! coefficient for cloud fraction
REAL(KIND=JPRB) :: XKRC_MF      ! coefficient for convective rc
REAL(KIND=JPRB) :: XTAUSIGMF  
REAL(KIND=JPRB) :: XPRES_UV     ! coefficient for pressure term in wind mixing
REAL(KIND=JPRB) :: XFRAC_UP_MAX ! maximum Updraft fraction
REAL(KIND=JPRB) :: XALPHA_MF    ! coefficient for updraft fraction in STA2 cloud scheme
REAL(KIND=JPRB) :: XSIGMA_MF    ! coefficient for sigma in STA2 cloud scheme

! Tuning variables for RHCJ10 updraft :

REAL(KIND=JPRB) :: XA1 
REAL(KIND=JPRB) :: XB
REAL(KIND=JPRB) :: XC  
REAL(KIND=JPRB) :: XBETA1

!  Tuning parameter for Hourdin et al closure

REAL(KIND=JPRB) :: XR

! Thermodynamic constant to compute thetas from thetal

REAL(KIND=JPRB) :: XLAMBDA
LOGICAL :: LTHETAS  ! <= TRUE to use Thetas, FALSE to use Thetal

! * for the squall line case:
LOGICAL :: LSQUALL ! use for the squall line case
INTEGER(KIND=JPIM) :: NREFROI1 !starting point for cooling 
INTEGER(KIND=JPIM) :: NREFROI2 !end point for cooling
REAL(KIND=JPRB) :: VSQUALL ! mean velocity displacement of the squall line.

! * for the MESO-NH physics printings:
INTEGER(KIND=JPIM) :: NPTP ! index in NPROMA paquet where the print will be done
INTEGER(KIND=JPIM) :: NPRINTFR !frequency of physical prints in apl_arome
                               ! (default 1/h) 

!* for other diagnostics
!  wmax per vertical level 
LOGICAL :: LDIAGWMAX !activate print of WMAX in apl_arome
INTEGER(KIND=JPIM) :: NDIAGWMAX ! frequency of preceding prints (in time step)

!* for chemical scheme
! time step factor
INTEGER(KIND=JPIM) :: NDTCHEM
!* for MNH budget anlysis
LOGICAL :: LAROBU_ENABLE
!* for turbulence scheme
REAL(KIND=JPRB) :: XLINI ! minimum bl89 mixing length
!* Subgrid precipitation scheme
CHARACTER (LEN=4) :: CSUBG_AUCV_RC ! type of rc->rr autoconversion scheme
CHARACTER(LEN=80) :: CSUBG_RC_RR_ACCR ! type of rc rc autoconversion
CHARACTER(LEN=80) :: CSUBG_RR_EVAP ! type of evaporation scheme
CHARACTER(LEN=80) :: CSUBG_RPR_PDF ! PDF chosen for rain production
!* for autoconversion qi,qc
REAL(KIND=JPRB) :: RCRIAUTI, RCRIAUTC, RT0CRIAUTI
LOGICAL :: LCRIAUTI ! to activate use of namelists parameters
!* For total cumulative 3D prec flux for MOCAGE
LOGICAL :: LFPREC3D
REAL(KIND=JPRB) :: XCQVR ! reduction factor of water vapour used for radiation computation.
REAL(KIND=JPRB) :: GQVPLIM ! pressure value over which qv is damped towards 0 for radiation.
REAL(KIND=JPRB) :: GQVTOP ! qv value at the top of the atmopshere.
LOGICAL :: LQVTOP ! to activate modification of qv in input to radiation.

END TYPE TPARAR

TYPE(TPARAR) :: YRPARAR 


!     ------------------------------------------------------------------
END MODULE YOMPARAR
