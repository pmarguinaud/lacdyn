MODULE YOMCVER


#include "create.h"

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

! =============================================================================

! * Variables related to vertical discretisation in finite elements:

! LVERTFE : .T./.F. Finite element/conventional vertical discretisation.
! NVSCH   : type of basis if the finite element vertical discretisation is used.
!           1: linear functions.
!           3: Hermite cubic functions.

! LRNHC1  : .T.: in the NH model, ensure constraint C1 by using (FD,NDLNPR=1)
!           in the linear terms (SI scheme) even if other vertical
!           discretisations (ex VFE) are activated in the non-linear model.

! LVFE_LAPL     : VFE for vertical Laplacian term (NH model)
! LVFE_LAPL_BC  : VFE for boundary cond. in vertical Laplacian term (NH model)
!               : if inner domain is purely in VFE manner
! LVFE_X_TERM   : VFE X-term (NH model)
! LVFE_Z_TERM   : VFE Z-term (w on full levels in NH model)
! LVFE_GW       : VFE for vertical velocity (NH model); in this case
!                 vertical velocity is at full levels.
! LVFE_DELNHPRE : VFE to compute [Delta pre] at full levels.
! LVFE_GWMPA    : VFE for AROME physics vertical velocity
!                 (NH model with AROME physics)
! LVFE_DERIB    : If VFE for derivatives in non-advective terms, use RDERB.
! LVFE_DBCS     : If VFE for derivatives in non-advective terms, use RDERI
!                 and correct surface boundary condition.
! LVFE_DBCT     : If VFE for derivatives in non-advective terms, use RDERI
!                 and correct top boundary condition.
! LVFE_CENTRI   : Centripetal method for full levels eta_vfe calculation.
! RVFE_CENTRI   : Exponent in function computing eta_vfe if LVFE_CENTRI=T
! LVFE_APPROX   : Approximation (or interpolation) used to represent a function.
! LVFE_VDA      : T if variation diminishing spline approximaton of input
!                  vector in the case of LVFE_DERIB key or LVFE_INTB key.
! LVFE_INT_ECMWF: T if original ECMWF way to compute RINTE
!                 F if B-splines to compute RINTE (and RINTBF11 too)
! LVFE_INTB     : T if integrals with explicit BBC and TBC.
! NVFE_ORDER    : Order of spline (2 for linear, 4 for cubic ?)
! LVFE_LAPL_HALF: Vertical Laplacian uses derivative operators full->half->full

! ----------------------------------------------------------------------

LOGICAL :: LVERTFE
create (LVERTFE)
INTEGER(KIND=JPIM) :: NVSCH
create (NVSCH)
LOGICAL :: LRNHC1
create (LRNHC1)
LOGICAL :: LVFE_LAPL
create (LVFE_LAPL)
LOGICAL :: LVFE_LAPL_BC
create (LVFE_LAPL_BC)
LOGICAL :: LVFE_X_TERM
create (LVFE_X_TERM)
LOGICAL :: LVFE_Z_TERM
create (LVFE_Z_TERM)
LOGICAL :: LVFE_GW
create (LVFE_GW)
LOGICAL :: LVFE_DELNHPRE
create (LVFE_DELNHPRE)
LOGICAL :: LVFE_GWMPA
create (LVFE_GWMPA)
LOGICAL :: LVFE_DERIB
create (LVFE_DERIB)
LOGICAL :: LVFE_DBCS
create (LVFE_DBCS)
LOGICAL :: LVFE_DBCT
create (LVFE_DBCT)
LOGICAL :: LVFE_CENTRI
create (LVFE_CENTRI)
REAL(KIND=JPRB) :: RVFE_CENTRI
create (RVFE_CENTRI)
LOGICAL :: LVFE_APPROX
create (LVFE_APPROX)
LOGICAL :: LVFE_VDA
create (LVFE_VDA)
LOGICAL :: LVFE_INT_ECMWF
create (LVFE_INT_ECMWF)
LOGICAL :: LVFE_INTB
create (LVFE_INTB)
INTEGER(KIND=JPIM) :: NVFE_ORDER
create (NVFE_ORDER)
LOGICAL :: LVFE_LAPL_HALF

create (LVFE_LAPL_HALF)
! =============================================================================

NAMELIST/NAMCVER/LVERTFE,NVSCH,LRNHC1, &
      & LVFE_LAPL,LVFE_LAPL_BC,LVFE_X_TERM,LVFE_GW,LVFE_Z_TERM,LVFE_GWMPA, &
      & LVFE_DELNHPRE,LVFE_DBCS,LVFE_DBCT,LVFE_DERIB, &
      & LVFE_CENTRI,RVFE_CENTRI,LVFE_APPROX,LVFE_VDA, &
      & LVFE_INT_ECMWF,LVFE_INTB,NVFE_ORDER,LVFE_LAPL_HALF

! =============================================================================

END
