MODULE YOMGSGEOM

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE


!     ------------------------------------------------------------------

!*    Grid point horizontal geometry: structure TGSGEOM

! RCORI      : Coriolis parameter "f = 2 Omega sin(theta)".
! RCORIC     : 2 Omega cos(theta).
! GEMU       : sine of geographic latitude "sin(theta)".
! GSQM2      : cosine of geographic latitude "cos(theta)".
! GELAM      : geographic longitude "lambda".
! GELAT      : geographic latitude "theta".
! GECLO      : cosine of geographic longitude "cos(lambda)".
! GESLO      : sine of geographic longitude "sin(lambda)".
! GM         : mapping factor "M".
! GMAPPA     : approximation of M**2 or RSTRET**2 according to model geometry.
! GOMVRL     : zonal component of vector "2 vec(Omega) wedge a vec(k)".
! GOMVRM     : meridian component of vector "2 vec(Omega) wedge a vec(k)".
! GNORDL     : zonal component "gnordl" of unit vector directed towards the geographic Northern pole.
! GNORDM     : meridian component "gnordm" of unit vector directed towards the geographic Northern pole.
! GNORDLCL   : zonal component of vector "rssavnabla gnordl".
! GNORDMCL   : zonal component of vector "rssavnabla gnordm".
! GNORDMCM   : meridian component of vector "rssavnabla gnordm".
! GAW        : Gaussian weight.
! NGPLAT     : DM-global number of the Gaussian grid latitude.
! NUNIQUEGP  : pointer array (see computation in sugem2.F90).

TYPE TGSGEOM
  REAL(KIND=JPRB),    POINTER :: RCORI(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: RCORIC(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GM(:)        => NULL()
  REAL(KIND=JPRB),    POINTER :: GMAPPA(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GOMVRL(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GOMVRM(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDL(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDM(:)    => NULL()
END TYPE TGSGEOM
  
END
