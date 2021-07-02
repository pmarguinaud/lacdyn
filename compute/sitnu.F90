!$acc routine(SITNU) seq
SUBROUTINE SITNU(YRVFE, YRVETA, YRDYN, YRDIMV, KIDIA,KFDIA,KLON,KFLEVG,PD,PT,PSP,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

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
USE YOMDIMV  , ONLY : TDIMV
USE YOMCST   , ONLY : RKAPPA
USE YOMDYN   , ONLY : TDYN
USE YOMVERT  , ONLY : TVETA, TVFE
USE YOMCVER  , ONLY : LVERTFE, LRNHC1, LVFE_INTB

!     ------------------------------------------------------------------

IMPLICIT NONE


TYPE (TDIMV),      INTENT(IN) :: YRDIMV
TYPE (TDYN),       INTENT(IN) :: YRDYN
TYPE (TVETA),      INTENT(IN) :: YRVETA
TYPE (TVFE),       INTENT(IN) :: YRVFE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEVG
REAL(KIND=JPRB)   ,INTENT(IN)    :: PD(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT(KLON,KFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSP(KLON) 

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)
!     ------------------------------------------------------------------

#include "abor1.intfb.h"


temp (REAL(KIND=JPRB), ZSDIVX, (KLON,0:YRDIMV%NFLEVG))
INTEGER(KIND=JPIM) :: JLEV, JLON

init_stack ()

alloc (ZSDIVX)




DO JLON=KIDIA,KFDIA
  ZSDIVX(JLON,0)=0.0_JPRB
ENDDO

DO JLEV=1,YRDIMV%NFLEVG
  DO JLON=KIDIA,KFDIA
    ZSDIVX(JLON,JLEV)=ZSDIVX(JLON,JLEV-1)+PD(JLON,JLEV)*YRDYN%SIDELP(JLEV)
    PT(JLON,JLEV)=RKAPPA*YRDYN%SITR*(YRDYN%SIRDEL(JLEV)*YRDYN%SILNPR(JLEV)&
     &*ZSDIVX(JLON,JLEV-1)+YRDYN%SIALPH(JLEV)*PD(JLON,JLEV))  
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  PSP(JLON)=ZSDIVX(JLON,YRDIMV%NFLEVG)*YRDYN%SIRPRN
ENDDO

END SUBROUTINE SITNU
