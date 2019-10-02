MODULE YOMVERT

USE PARKIND1 , ONLY : JPRB

IMPLICIT NONE


!     ------------------------------------------------------------------

!*    * DEFINING THE VERTICAL COORDINATE: A and B

!     VP00  : REFERENCE PRESSURE FOR DEFINING VERTICAL COORDINATE
!     VALH  : (0:NFLEVG)
!     VBH   : (0:NFLEVG) : B of the vertical coordinate
!     VAH   : (0:NFLEVG) ;  =VALH*VP00
!     VC    : (NFLEVG)   ;  =VAH(J)*VBH(J-1)-VAH(J-1)*VBH(J)
!     VDELB : (NFLEVG)   ;  =VBH(J)-VBH(J-1)
!     VDELA : (NFLEVG)   ;  =VAH(J)-VAH(J-1)
!     VAF   : like VAH but at full levels.
!     VBF   : like VBH but at full levels.
!     TOPPRES: REFERENCE "EVANESCENT" PRESSURE
!              TOPPRES allows to solve some calculations of singularities
!              when the top pressure of the model is zero (for ex. in
!              GPPREF, GPXYB, SUNHBMAT).

!     WE HAVE THEN FOR THE HALF LEVEL PRESSURE : VAH + VBH*(SURFACE PRESSURE)

TYPE TVAB
  REAL(KIND=JPRB), POINTER :: VALH(:)
  REAL(KIND=JPRB), POINTER :: VBH(:)
  REAL(KIND=JPRB), POINTER :: VAH(:)
  REAL(KIND=JPRB), POINTER :: VC(:)
  REAL(KIND=JPRB), POINTER :: VAF(:)
  REAL(KIND=JPRB), POINTER :: VBF(:)
  REAL(KIND=JPRB), POINTER :: VDELA(:)
  REAL(KIND=JPRB), POINTER :: VDELB(:)
  REAL(KIND=JPRB)         :: VP00
  REAL(KIND=JPRB)         :: TOPPRES
END TYPE TVAB

!     ------------------------------------------------------------------

!*    * DEFINING THE VERTICAL COORDINATE: eta

!     VETAH : (0:NFLEVG) ; VERTICAL COORDINATE = VALH+VBH
!     VFE_ETAH : version of VETAH used for VFE operators.
!     VETAF : (0:NFLEVG+1) ; VERTICAL COORDINATE ON LAYERS.
!     VFE_ETAF : version of VETAF used for VFE operators.
!     VFE_RDETAH: VFE_RDETAH(jlev)=1/(VFE_ETAH(jlev)-VFE_ETAH(jlev-1))
!     VFE_ETAF_VDA: used if LVFE_VDA=T.

TYPE TVETA
  REAL(KIND=JPRB), POINTER :: VETAH(:)
  REAL(KIND=JPRB), POINTER :: VFE_ETAH(:)
  REAL(KIND=JPRB), POINTER :: VETAF(:)
  REAL(KIND=JPRB), POINTER :: VFE_ETAF(:)
  REAL(KIND=JPRB), POINTER :: VFE_RDETAH(:)
  REAL(KIND=JPRB), POINTER :: VFE_ETAF_VDA(:)
END TYPE TVETA

! -----------------------------------------------------------------------------

! * Matricial operators related to vertical discretisation in finite elements
!   (to compute integrals or derivatives):
! RINTE   : matricial operator for vertical integrations in the
!           finite element vertical discretisation.
! RDERI   : matricial operator for vertical first-order derivatives in the
!           finite element vertical discretisation.
!           Does not take account of top and bottom boundary conditions.
! RDERB   : version of RDERI taking account of top and bottom boundary conds.
! RDDERI  : matricial operator for vertical second-order derivatives in the
!           finite element vertical discretisation (NH model only).

! * Matricial operators related to vertical discretisation in finite elements
!   (to compute integrals or derivatives):
! RINTE    : matricial operator for vertical integrations in the
!            finite element vertical discretisation.
! RINTBF11 : alternate matricial operator "KINT" for vertical integrations,
!            operating on full levels, with given top and bottom BC.
!            input boundary conditions: (dX/deta)_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: KX_{0}=0
! RDERI    : matricial operator for vertical first-order derivatives in the
!            finite element vertical discretisation.
!            Does not take account of top and bottom boundary conditions.
! RDERB    : version of RDERI taking account of top and bottom boundary conds.
! RDDERI   : matricial operator for vertical second-order derivatives in the
!            finite element vertical discretisation (NH model only).
! RDERBH00 : matricial operator for vertical derivatives: on half levels.
!            input boundary conditions: X_0=0, X_L+1=X_L
!            output boundary conditions: none
! RDERBF00 : matricial operator for vertical derivatives: on full levels.
!            input boundary conditions: X_0=0, X_L+1=X_L
!            output boundary conditions: none
! RDERBF01 : matricial operator for vertical derivatives: on full levels.
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DX_{L+1}=0
! RDDERBF01: matricial operator for 2nd order vertical derivatives: on full levels.
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DDX_{L+1}=0

TYPE TVFE
  REAL(KIND=JPRB),POINTER :: RINTE(:,:)
  REAL(KIND=JPRB),POINTER :: RINTBF11 (:,:)
  REAL(KIND=JPRB),POINTER :: RDERI(:,:)
  REAL(KIND=JPRB),POINTER :: RDERB(:,:)
  REAL(KIND=JPRB),POINTER :: RDDERI(:,:)
  REAL(KIND=JPRB),POINTER :: RDERBH00 (:,:)
  REAL(KIND=JPRB),POINTER :: RDERBF00 (:,:)
  REAL(KIND=JPRB),POINTER :: RDERBF01 (:,:)
  REAL(KIND=JPRB),POINTER :: RDDERBF01(:,:)
END TYPE TVFE

TYPE(TVAB)   :: YRVAB  
TYPE(TVETA)  :: YRVETA 
TYPE(TVFE)   :: YRVFE  

!$acc declare create(YRVAB)
!$acc declare create(YRVETA)
!$acc declare create(YRVFE)

END MODULE YOMVERT
