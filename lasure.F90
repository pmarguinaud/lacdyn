SUBROUTINE LASURE(&
 ! ----- INPUT ---------------------------------------------------------------
 & KLON, KIDIA,KFDIA,PBETADT,PDT,YDGSGEOM,&
 ! ----- OUTPUT --------------------------------------------------------------
 & PDTS2,PBT,LD2TLFF1,PBDT,PREDIV,PESGP,PESGM )  

!**** *LASURE*   Semi-Lagrangian scheme.
!                Set-up for other subroutines called by LACDYN.

!     Purpose.
!     --------
!       Computes some intermediate quantities necessary in the other
!       subroutines called by LACDYN.

!**   Interface.
!     ----------
!        *CALL* *LASURE(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KIDIA  : first element of work.
!          KFDIA   : depth of work.
!          PBETADT : BETADT or 0 according to configuration.
!          PDT     : time step for the first time-integration step of
!                    a leap-frog scheme or all time-integration steps of
!                    a two-time level scheme; 2*time step for the following
!                    time-integration steps of a leap-frog scheme.
!          YDGSGEOM: structure for geographical sphere horizontal geometry

!        OUTPUT:
!          PDTS2   : 0.5*PDT.
!          PBT     : PDTS2*PBETADT.
!          LD2TLFF1: .T./.F.: Refined treatement of (2*Omega Vec r) at
!                    the origin point when there is t-dt (or t in SL2TL)
!                    physics / Other cases.
!          PBDT    : PBT or PBT*(c**2/GM**2) according to LSIDG.
!          PREDIV  : 1. or c**2/GM**2 according to LSIDG.
!          PESGP   : (1 + uncentering factor).
!          PESGM   : (1 - uncentering factor).

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-lagrangian scheme.

!     Externals.
!     ----------
!           none. 
!           Called by LACDYN, LACDYNTL and LACDYNAD.

!     Reference.
!     ----------
!             Arpege documentation about semi-lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after old part one of LACDYN. 
!      Loops are rewritten according to F90 norms.
!      Original : JULY 1995.

!     Modifications.
!     --------------
!      J.Vivoda 03-2002 PC schemes for NH dynamics (LPC_XXXX keys)
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!      K. Yessad (Dec 2011): use YDGSGEOM.
!      T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

USE YOEPHY   , ONLY : YREPHY
USE YOMPHY   , ONLY : YRPHY
USE YOMDIM   , ONLY : YRDIM
USE YEMDYN   , ONLY : YREDYN
USE YOMDYN   , ONLY : YRDYN
USE YOMGEM   , ONLY : YRGEM
USE YOMGSGEOM, ONLY : TGSGEOM

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBETADT
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDT 
TYPE(TGSGEOM)     ,INTENT(IN)    :: YDGSGEOM
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDTS2 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBT 
LOGICAL           ,INTENT(OUT)   :: LD2TLFF1 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBDT(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PREDIV(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PESGP 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PESGM 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) ::  JLON


!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS:
!              ----------------------------

!     * Time step.

PDTS2 = 0.5_JPRB*PDT
PBT = PDTS2*PBETADT

IF (YRDYN%LSIDG) THEN
  DO JLON=KIDIA,KFDIA
    PBDT(JLON)  = PBT
    PREDIV(JLON)= 1.0_JPRB
  ENDDO
ELSEIF (YREDYN%LESIDG) THEN
  DO JLON=KIDIA,KFDIA
    PBDT(JLON)  = PBT*YDGSGEOM%GMAPPA(JLON)/(YDGSGEOM%GM(JLON)*YDGSGEOM%GM(JLON))
    PREDIV(JLON)= YDGSGEOM%GMAPPA(JLON)/(YDGSGEOM%GM(JLON)*YDGSGEOM%GM(JLON))
  ENDDO
ELSE
  DO JLON=KIDIA,KFDIA
    PBDT(JLON)  = PBT*YRGEM%RSTRET*YRGEM%RSTRET/(YDGSGEOM%GM(JLON)*YDGSGEOM%GM(JLON))
    PREDIV(JLON)= YRGEM%RSTRET*YRGEM%RSTRET/(YDGSGEOM%GM(JLON)*YDGSGEOM%GM(JLON))
  ENDDO
ENDIF

!     * ( 1 + uncentering factor ) and other quantities linked with
!       uncentering factor.

PESGP = 1.0_JPRB + YRDYN%VESL
PESGM = 1.0_JPRB - YRDYN%VESL

!     * Switch to activate two additional interpolations for refined
!       computation of Coriolis term.

LD2TLFF1=YRDYN%L2TLFF.AND.(YRPHY%LMPHYS.OR.YREPHY%LEPHYS).AND.(.NOT.YREPHY%LAGPHY)

!     ------------------------------------------------------------------



END SUBROUTINE LASURE
