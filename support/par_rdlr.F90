MODULE PAR_RDLR


#include "create.h"

! Parameter pointers and dimensions for array (Z,P)RDLR, used in the
! Wood and Staniforth deep-layer formulation of NH equations.
! (Z,P)RDLR contains (r/a), (a/r), grad(r/a), at full and half levels.

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! JP_RSAF : parameter pointer for (r/a) at full levels.
! JP_RSAH : parameter pointer for (r/a) at half levels.
! JP_ASRF : parameter pointer for (a/r) at full levels.
! JP_ASRH : parameter pointer for (a/r) at half levels.
! JP_RSAFL: parameter pointer for zonal comp of grad(r/a) at full levels.
! JP_RSAFM: parameter pointer for merid comp of grad(r/a) at full levels.
! JP_RSAHL: parameter pointer for zonal comp of grad(r/a) at half levels.
! JP_RSAHM: parameter pointer for merid comp of grad(r/a) at half levels.
! JP_DIMR : total number of fields in (Z,P)RDLR.
! JP_DIMR0: total number of fields in (Z,P)RDLR at time t.
! JP_DIMR9: total number of fields in (Z,P)RDLR at time t-dt (useless grad.).
! JP_DIMR1: total number of fields in (Z,P)RDLR at time t+dt (useless grad.).
! Remark for gradient: the gradient computed is the semi-reduced one
!  (i.e. (r/a)*nabla).

INTEGER(KIND=JPIM) :: JP_RSAF
create (JP_RSAF)
INTEGER(KIND=JPIM) :: JP_RSAH
create (JP_RSAH)
INTEGER(KIND=JPIM) :: JP_ASRF
create (JP_ASRF)
INTEGER(KIND=JPIM) :: JP_ASRH
create (JP_ASRH)
INTEGER(KIND=JPIM) :: JP_RSAFL
create (JP_RSAFL)
INTEGER(KIND=JPIM) :: JP_RSAFM
create (JP_RSAFM)
INTEGER(KIND=JPIM) :: JP_RSAHL
create (JP_RSAHL)
INTEGER(KIND=JPIM) :: JP_RSAHM
create (JP_RSAHM)
INTEGER(KIND=JPIM) :: JP_DIMR0
create (JP_DIMR0)
INTEGER(KIND=JPIM) :: JP_DIMR9
create (JP_DIMR9)
INTEGER(KIND=JPIM) :: JP_DIMR1
create (JP_DIMR1)
INTEGER(KIND=JPIM) :: JP_DIMR

create (JP_DIMR)
END MODULE PAR_RDLR
