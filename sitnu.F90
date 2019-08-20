SUBROUTINE SITNU(KST,KPROF,KLON,KFLEVG,PD,PT,PSP)

!**** *SITNU*   - Continuity equation for semi-implicit.

!     Purpose.
!     --------
!           Evaluate operators Tau and Nu in semi-implicit.

!**   Interface.
!     ----------
!        *CALL* *SITNU(...)

!        Explicit arguments :
!        --------------------
!        KLEV   : DISTANCE IN MEMORY BETWEEN VALUES OF THE DIVERGENCE
!                OR TEMPERATURE AT THE SAME VERTICAL
!        KLON   : DISTANCE IN MEMORY BETWEEN VALUES OF THE DIVERGENCE
!                OR TEMPERATURE AT THE SAME LEVEL

!           TYPICAL VALUES ARE  NDLSUR,1  FOR GRID POINT ARRAY
!                               1,NFLSUR  FOR SPECTRAL ARRAY

!        PD    : DIVERGENCE
!        PT    : TEMPERATURE
!        PSP   : SURFACE PRESSURE
!        KNLON : NUMBER OF VERTICAL COLUMNS TREATED

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.   None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 87-10-15

!     Modifications.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      Modified : 09-Oct-2007 by K. YESSAD: possibility to have a specific
!                 value of LVERTFE in the SI NH linear model.
!      F. Vana + NEC 28-Apr-2009: OpenMP + optimization
!      P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!      G. Mozdzynski Oct 2012: OpenMP optimization
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCT0   , ONLY : LNHDYN
USE YOMDIMV  , ONLY : YRDIMV
USE YOMCST   , ONLY : RKAPPA
USE YOMDYN   , ONLY : YRDYN
USE YOMVERT  , ONLY : YRVETA, YRVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSDIVX(KLON,0:YRDIMV%NFLEVG)
INTEGER(KIND=JPIM) :: JLEV, JLON


DO JLON=KST,KPROF
  ZSDIVX(JLON,0)=0.0_JPRB
ENDDO

DO JLEV=1,YRDIMV%NFLEVG
  DO JLON=KST,KPROF
    ZSDIVX(JLON,JLEV)=ZSDIVX(JLON,JLEV-1)+PD(JLON,JLEV)*YRDYN%SIDELP(JLEV)
    PT(JLON,JLEV)=RKAPPA*YRDYN%SITR*(YRDYN%SIRDEL(JLEV)*YRDYN%SILNPR(JLEV)&
     &*ZSDIVX(JLON,JLEV-1)+YRDYN%SIALPH(JLEV)*PD(JLON,JLEV))  
  ENDDO
ENDDO

DO JLON=KST,KPROF
  PSP(JLON)=ZSDIVX(JLON,YRDIMV%NFLEVG)*YRDYN%SIRPRN
ENDDO

END SUBROUTINE SITNU
