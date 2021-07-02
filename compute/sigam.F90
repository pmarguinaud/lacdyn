SUBROUTINE SIGAM(YRVFE, YRVETA, YRDYN, KIDIA,KFDIA,KLON,KFLEVG,PD,PT,PSP)



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
USE YOMDYN   , ONLY : TDYN
USE YOMVERT  , ONLY : TVETA, TVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB

!     ------------------------------------------------------------------

IMPLICIT NONE


TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TVETA),      INTENT(IN) :: YRVETA
TYPE (TVFE),       INTENT(IN) :: YRVFE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD(KLON,KFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KLON) 

!     ------------------------------------------------------------------

#include "abor1.intfb.h"


REAL(KIND=JPRB) :: ZSPHIX (KLON,0:KFLEVG)
INTEGER(KIND=JPIM) :: JLEV, JLON

!     ------------------------------------------------------------------

!*       1.    SUM GEOPOTENTIAL, COMPUTES P AND PUT IT IN PD.
!              ----------------------------------------------





DO JLON=KIDIA,KFDIA
  ZSPHIX(JLON,KFLEVG)=0.0_JPRB
ENDDO

DO JLEV=KFLEVG,1,-1
  DO JLON=KIDIA,KFDIA
    ZSPHIX(JLON,JLEV-1)=ZSPHIX(JLON,JLEV)+RD*PT(JLON,JLEV)*YRDYN%SILNPR(JLEV)
    PD(JLON,JLEV)=ZSPHIX(JLON,JLEV)+YRDYN%SIALPH(JLEV)*RD*PT(JLON,JLEV)+PSP(JLON)*YRDYN%SIRPRG
  ENDDO
ENDDO

!      -----------------------------------------------------------------


END SUBROUTINE SIGAM
