SUBROUTINE SITNU(KLEV,KLON,PD,PT,PSP,KNLON)

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

INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KNLON 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD(1+(YRDIMV%NFLEVG-1)*KLEV+(KNLON-1)*KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(1+(YRDIMV%NFLEVG-1)*KLEV+(KNLON-1)*KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KNLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSDIV(KNLON,0:YRDIMV%NFLEVG), ZOUT(KNLON,0:YRDIMV%NFLEVG)
REAL(KIND=JPRB) :: ZIN  (KNLON,0:YRDIMV%NFLEVG+1)
REAL(KIND=JPRB) :: ZSDIVX(0:YRDIMV%NFLEVG,KNLON)
LOGICAL :: LLVFE
INTEGER(KIND=JPIM) :: IDT, JLEV, JLON
REAL(KIND=JPRB) :: ZREC


DO JLON=1,KNLON
  ZSDIVX(0,JLON)=0.0_JPRB
  DO JLEV=1,YRDIMV%NFLEVG
    IDT=1+(JLEV-1)*KLEV+(JLON-1)*KLON
    ZSDIVX(JLEV,JLON)=ZSDIVX(JLEV-1,JLON)+PD(IDT)*YRDYN%SIDELP(JLEV)
    PT(IDT)=RKAPPA*YRDYN%SITR*(YRDYN%SIRDEL(JLEV)*YRDYN%SILNPR(JLEV)*ZSDIVX(JLEV-1,&
     & JLON)&
     & +YRDYN%SIALPH(JLEV)*PD(IDT))  
  ENDDO
  PSP(JLON)=ZSDIVX(YRDIMV%NFLEVG,JLON)*YRDYN%SIRPRN
ENDDO


WRITE (0, *) __FILE__, ':', __LINE__, PSP (1)
END SUBROUTINE SITNU
