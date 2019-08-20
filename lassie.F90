SUBROUTINE LASSIE(KLON, YDGMV,KIDIA,KFDIA,PRCORI,PGMV,PGMVS,PGFL,&
 & PSDIV0,PSDIV9,PTOD0,PTOD9,PGAGT0L,PGAGT0M,PGAGT9L,PGAGT9M)  

!**** *LASSIE*   Semi-Lagrangian scheme.
!                Computation of linear terms used in the semi-implicit scheme. 

!     Purpose.
!     --------
!        Computation of linear terms used in the semi-implicit scheme:
!        Nabla(Gamma*T+Mu*Pi), (Tau*D) and (Nu*D).

!**   Interface.
!     ----------
!        *CALL* *LASSIE(..)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KIDIA      - first element of work.
!          KFDIA    - depth of work.
!          PRCORI   - Coriolis parameter 2.OMEGA .
!          PGMV     - GMV variables at t-dt and t.
!          PGMVS    - GMVS variables at t-dt and t.
!          PGFL     - unified_treatment grid-point (GFL) fields.

!        OUTPUT:
!          PSDIV0   - semi-implicit term at time t for continuity equation
!                     (Nu*D).
!          PSDIV9   - semi-implicit term at time t-dt for continuity equation
!                     (Nu*D).
!          PTOD0    - semi-implicit term at time t for temperature equation
!                     (Tau*D).
!          PTOD9    - semi-implicit term at time t-dt for temperature equation
!                     (Tau*D).
!          PGAGT0L  - semi-implicit term at time t for U-wind equation
!                     (zonal component of Nabla(Gamma*T+Mu*Pi)).
!          PGAGT0M  - semi-implicit term at time t for V-wind equation
!                     (meridian component of Nabla(Gamma*T+Mu*Pi)).
!          PGAGT9L  - semi-implicit term at time t-dt for U-wind equation
!                     (zonal component of Nabla(Gamma*T+Mu*Pi)).
!          PGAGT9M  - semi-implicit term at time t-dt for V-wind equation
!                     (meridian component of Nabla(Gamma*T+Mu*Pi)).

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!           Called by LACDYN.

!     Reference.
!     ----------
!             Arpege documentation about semi-lagrangian scheme.

!     Author.
!     -------
!      K. YESSAD (METEO FRANCE/CNRM/GMAP) after part 3.1 of LACDYN.
!      Loops have been recoded according to F90 norms.
!      Original : AUGUST 1995.

!     Modifications.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      Modified 04-02-06: Y. Seity : new arguments to GPRCP (rain, snow and graupel)
!      M.Hamrud  15-Jan-2006  Revised GPRCP
!      K. Yessad Aug 2008: rationalisation of dummy argument interfaces
!      O. Marsden: June 2015 CY42 YRGMV, YRGFL, YRSURF, YRGMV5, and YRGFL5 are now passed by argument
!     ------------------------------------------------------------------

USE YOMGMV   , ONLY : TGMV
USE PARKIND1 , ONLY : JPIM, JPRB

USE YOMCST   , ONLY : RD
USE YOMCT0   , ONLY : LTWOTL, LSPRT
USE YOMDIM   , ONLY : YRDIM
USE YOMDIMV  , ONLY : YRDIMV
USE YOMDYN   , ONLY : YRDYN
USE YOM_YGFL , ONLY : YGFL

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
TYPE(TGMV) , INTENT(INOUT) :: YDGMV
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRCORI(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMV(KLON,YRDIMV%NFLEVG,YDGMV%NDIMGMV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMVS(KLON,YDGMV%NDIMGMVS)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGFL(KLON,YRDIMV%NFLEVG,YGFL%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV0(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSDIV9(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD0(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTOD9(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0L(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT0M(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9L(KLON,YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGAGT9M(KLON,YRDIMV%NFLEVG) 
!     ------------------------------------------------------------------
REAL(KIND=JPRB) :: ZR9  (KLON,YRDIMV%NFLEVG)

INTEGER(KIND=JPIM) :: IPROFS, JLEV, JROF


!     ------------------------------------------------------------------

#include "gprcp.intfb.h"
#include "sigam.intfb.h"
#include "sitnu.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

IPROFS=KFDIA-KIDIA+1

! * Variables at time 0 (SL3TL and SL2TL).

!   - Computation of Nu*D (SI term for continuity equation)
!     and Tau*D (SI term for temperature equation).
CALL SITNU(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,&
          &PGMV(1,1,YDGMV%YT0%MDIV),PTOD0,PSDIV0)
!   - Computation of Nabla(Gamma*T+Mu*Pi) (SI term for momentum equation).
CALL SIGAM(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,PGAGT0L,&
          &PGMV(1,1,YDGMV%YT0%MTL),PGMVS(1,YDGMV%YT0%MSPL))
CALL SIGAM(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,PGAGT0M,&
          &PGMV(1,1,YDGMV%YT0%MTM),PGMVS(1,YDGMV%YT0%MSPM))

! * Variables at time 9 (SL3TL only).

IF (.NOT.LTWOTL) THEN
  ! - Computation of Nu*D (SI term for continuity equation)
  !   and Tau*D (SI term for temperature equation).
  CALL SITNU(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,&
           &PGMV(1,1,YDGMV%YT9%MDIV),PTOD9,PSDIV9)
  ! - Computation of Nabla(Gamma*T+Mu*Pi) (SI term for momentum equation).
  CALL SIGAM(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,PGAGT9L,&
            &PGMV(1,1,YDGMV%YT9%MTL),PGMVS(1,YDGMV%YT9%MSPL))
  CALL SIGAM(KIDIA,KFDIA,KLON,YRDIMV%NFLEVG,PGAGT9M,&
            &PGMV(1,1,YDGMV%YT9%MTM),PGMVS(1,YDGMV%YT9%MSPM))
ENDIF

! * For "spectral RT" option, adjust semi-implicit term (Tau*D) in
!   T-equation to compensate for later multiplication by R/Rd

IF (LSPRT) THEN
  IF (LTWOTL) THEN
    ! remark for lpc_full:
    !  predictor: treatment of "t" data.
    !  corrector: treatment of provisional "t+dt" data.
    ! So in this case this is always the Y[X]%MP data which are used.
    CALL GPRCP(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG,PGFL=PGFL,PR=ZR9)  
    DO JLEV=1,YRDIMV%NFLEVG
      DO JROF=KIDIA,KFDIA
        PTOD0(JROF,JLEV)=RD*PTOD0(JROF,JLEV)/ZR9(JROF,JLEV)
      ENDDO
    ENDDO
  ELSE
    CALL GPRCP(KLON,KIDIA,KFDIA,YRDIMV%NFLEVG,PGFL=PGFL,KGFLTYP=9,PR=ZR9)  
    DO JLEV=1,YRDIMV%NFLEVG
      DO JROF=KIDIA,KFDIA
        PTOD0(JROF,JLEV)=RD*PTOD0(JROF,JLEV)/ZR9(JROF,JLEV)
        PTOD9(JROF,JLEV)=RD*PTOD9(JROF,JLEV)/ZR9(JROF,JLEV)
      ENDDO
    ENDDO
  ENDIF
ENDIF

! * Add semi-implicit Coriolis terms to Nabla(Gamma*T+Mu*Pi)
!   if required (LIMPF=.T.).

IF (YRDYN%LIMPF) THEN
  DO JLEV=1,YRDIMV%NFLEVG
    DO JROF=KIDIA,KFDIA
      PGAGT0L(JROF,JLEV)=PGAGT0L(JROF,JLEV)-PRCORI(JROF)*PGMV(JROF,JLEV,YDGMV%YT0%MV)
      PGAGT0M(JROF,JLEV)=PGAGT0M(JROF,JLEV)+PRCORI(JROF)*PGMV(JROF,JLEV,YDGMV%YT0%MU)
    ENDDO
  ENDDO
  IF (.NOT.LTWOTL) THEN
    DO JLEV=1,YRDIMV%NFLEVG
      DO JROF=KIDIA,KFDIA
        PGAGT9L(JROF,JLEV)=PGAGT9L(JROF,JLEV) &
         & -PRCORI(JROF)*PGMV(JROF,JLEV,YDGMV%YT9%MV)
        PGAGT9M(JROF,JLEV)=PGAGT9M(JROF,JLEV) &
         & +PRCORI(JROF)*PGMV(JROF,JLEV,YDGMV%YT9%MU)
      ENDDO
    ENDDO
  ENDIF
ENDIF

!     ------------------------------------------------------------------



END SUBROUTINE LASSIE

