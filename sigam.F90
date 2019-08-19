SUBROUTINE SIGAM(KLEV,KLON,PD,PT,PSP,KNLON,KFLEVG)

!**** *SIGAM* - Solve hydrostatic operator in semi-implicit

!     Purpose.
!     --------
!           Operator gamma to compute p.

!**   Interface.
!     ----------
!        *CALL* *SIGAM(...)

!        Explicit arguments :
!        --------------------
!        KLEV   : DISTANCE IN MEMORY BETWEEN VALUES OF THE DIVERGENCE
!                OR TEMPERATURE AT THE VERTICAL
!        KLON   : DISTANCE IN MEMORY BETWEEN VALUES OF THE DIVERGENCE
!                OR TEMPERATURE AT THE SAME LEVEL

!           TYPICAL VALUES ARE  NDLSUR,1  FOR GRID POINT ARRAY
!                               1,NFLSUR  FOR SPECTRAL ARRAY

!        PD    : DIVERGENCE       (output)
!        PT    : TEMPERATURE      (input)
!        PSP   : SURFACE PRESSURE (input)
!        KNLON : NUMBER OF VERTICAL COLUMNS TREATED
!        KFLEVG: NUMBER OF ELEMENTS IN A VERTICAL COLUMN

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
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
!                 value of LVERTFE in the SI linear model.
!      F. Vana + NEC 28-Apr-2009: OpenMP
!      P. Smolikova and J. Vivoda (Oct 2013): new options for VFE-NH
!      G. Mozdzynski Oct 2012: OpenMP optimization
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCT0   , ONLY : LNHDYN
USE YOMCST   , ONLY : RD
USE YOMDYN   , ONLY : YRDYN
USE YOMVERT  , ONLY : YRVETA, YRVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KNLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD(1+(KFLEVG-1)*KLEV+(KNLON-1)*KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(1+(KFLEVG-1)*KLEV+(KNLON-1)*KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KNLON) 

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSPHI(KNLON,0:KFLEVG), ZOUT(KNLON,0:KFLEVG)
REAL(KIND=JPRB) :: ZIN  (KNLON,0:KFLEVG+1)
REAL(KIND=JPRB) :: ZSPHIX(0:KFLEVG,KNLON)
LOGICAL :: LLVFE
INTEGER(KIND=JPIM) :: IDT, JLEV, JLON

!     ------------------------------------------------------------------

!include "verint.intfb.h"

!     ------------------------------------------------------------------

ASSOCIATE(SIALPH=>YRDYN%SIALPH, SILNPR=>YRDYN%SILNPR, SIRPRG=>YRDYN%SIRPRG)

!     ------------------------------------------------------------------

!*       1.    SUM GEOPOTENTIAL, COMPUTES P AND PUT IT IN PD.
!              ----------------------------------------------

IF(LNHDYN.AND.LRNHC1) THEN
  LLVFE=.FALSE.
ELSE
  LLVFE=LVERTFE
ENDIF

IF(LLVFE) THEN

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,IDT)
  DO JLON=1,KNLON
    DO JLEV=1,KFLEVG
      IDT=1+(JLEV-1)*KLEV+(JLON-1)*KLON
      ZSPHI(JLON,JLEV)=-RD*PT(IDT)*SILNPR(JLEV)*YRVETA%VFE_RDETAH(JLEV)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

  IF( LVFE_INTB )THEN
    ZIN(1:KNLON,0)=0.0_JPRB
    ZIN(1:KNLON,KFLEVG+1)=0.0_JPRB
    ZIN(1:KNLON,1:KFLEVG)=ZSPHI(1:KNLON,1:KFLEVG)
!   CALL VERINT(KNLON,1,KNLON,KFLEVG+2,KFLEVG+1,YRVFE%RINTBF11,ZIN,ZOUT,1)
  ELSE
!   CALL VERINT(KNLON,1,KNLON,KFLEVG,KFLEVG+1,YRVFE%RINTE,ZSPHI(1,1),ZOUT,1)
  ENDIF

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLEV,JLON,IDT)
  DO JLON=1,KNLON
    DO JLEV=1,KFLEVG
      IDT=1+(JLEV-1)*KLEV+(JLON-1)*KLON
      PD(IDT)=ZOUT(JLON,JLEV-1)+PSP(JLON)*SIRPRG
    ENDDO
  ENDDO
!$OMP END PARALLEL DO

ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(JLON,JLEV,IDT)
  DO JLON=1,KNLON
    ZSPHIX(KFLEVG,JLON)=0.0_JPRB
    DO JLEV=KFLEVG,1,-1
      IDT=1+(JLEV-1)*KLEV+(JLON-1)*KLON
      ZSPHIX(JLEV-1,JLON)=ZSPHIX(JLEV,JLON)+RD*PT(IDT)*SILNPR(JLEV)
      PD(IDT)=ZSPHIX(JLEV,JLON)+SIALPH(JLEV)*RD*PT(IDT)+PSP(JLON)*SIRPRG
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
ENDIF

!      -----------------------------------------------------------------

END ASSOCIATE
END SUBROUTINE SIGAM
