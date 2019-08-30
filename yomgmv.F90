MODULE YOMGMV

! Module containing t,t-dt,t+dt gridpoint arrays (apart from GFL) for dynamics
! and all "pointers" for accesssing elements in the arrays
! GMV and GMVS are permanently allocated, GMVT1 and GMVT1S temporary

!-------------------------------------------------------------------------

USE PARKIND1,  ONLY : JPIM, JPRB
USE TYPE_GMVS, ONLY : TYPE_T0,TYPE_T9,TYPE_T1,TYPE_PH9

IMPLICIT NONE
SAVE

TYPE :: TGMV

!-------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NDIMGMV ! Third dim. of GMV "(NPROMA,NFLEVG,NDIMGMV,NGPBLKS)"
INTEGER(KIND=JPIM) :: NDIMGMVS ! Second dim. GMVS "(NPROMA,NDIMGMVS,NGPBLKS)"

!-------------------------------------------------------------------------

TYPE(TYPE_T0)  :: YT0  ! Pointers to time t quantities
TYPE(TYPE_T9)  :: YT9  ! Pointers to time t-dt quantities
TYPE(TYPE_T1)  :: YT1  ! Pointers to time t+dt quantities
TYPE(TYPE_PH9) :: YPH9 ! Pointers to physics time t-dt quantities

!-------------------------------------------------------------------------

! Pointers (offsets) for individual fields in GMV grid-point arrays

INTEGER(KIND=JPIM) :: MGPGMV_U   ! U-wind
INTEGER(KIND=JPIM) :: MGPGMV_V   ! V-wind
INTEGER(KIND=JPIM) :: MGPGMV_T   ! Temperature
INTEGER(KIND=JPIM) :: MGPGMV_SPD ! Pressure departure variable
INTEGER(KIND=JPIM) :: MGPGMV_SVD ! Vertical div or velocity variable
INTEGER(KIND=JPIM) :: MGPGMV_NHX ! NHX term
INTEGER(KIND=JPIM) :: NGPGMV     ! Number of grid-point GMV fields

!-------------------------------------------------------------------------

END TYPE TGMV

END
