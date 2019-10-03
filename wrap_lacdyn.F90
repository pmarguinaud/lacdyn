#include "simple4.h"
#define ATTR_ARG

USE PARKIND1,  ONLY : JPIM, JPRB
USE PTRSLB1  , ONLY : YRPTRSLB1

IMPLICIT NONE

INTEGER :: IBL, ICOUNT, KLON, JJ, JIDIA, JFDIA, KIDIA, KFDIA

CALL LACDYN_LOAD_ALL ("t0031/LACDYN.CONST")


KLON = 64
KIDIA = 1
KFDIA = KLON

ICOUNT = 1

PRINT *, __FILE__//':', __LINE__
PRINT *, YRPTRSLB1%NFLDSLB1
!$acc kernels
PRINT *, YRPTRSLB1%NFLDSLB1
!$acc end kernels
PRINT *, __FILE__//':', __LINE__


#ifdef ACC
!$acc parallel loop gang vector private(IBL) private(JJ) collapse(2) 
#else
!$OMP PARALLEL DO PRIVATE (IBL,JJ,JIDIA,JFDIA)
#endif

DO IBL = 1, ICOUNT
  DO JJ = KIDIA, KFDIA
  PRINT *, YRPTRSLB1%NFLDSLB1
    JIDIA=JJ
    JFDIA=JJ
  ENDDO

ENDDO
#ifdef ACC
!$acc end parallel
#else
!$OMP END PARALLEL DO
#endif


END 
