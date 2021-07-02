MODULE COPY_TYPE_T1_MOD

USE TYPE_GMVS, ONLY : TYPE_T1

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_T1
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_T1 (YD)

IMPLICIT NONE
TYPE (TYPE_T1), INTENT (IN) :: YD

!$acc update device (YD%NDIM)

!$acc update device (YD%NDIMS)

!$acc update device (YD%MU)

!$acc update device (YD%MV)

!$acc update device (YD%MT)

!$acc update device (YD%MSPD)

!$acc update device (YD%MSVD)

!$acc update device (YD%MVOR)

!$acc update device (YD%MDIV)

!$acc update device (YD%MNHX)

!$acc update device (YD%MSP)

END SUBROUTINE

END MODULE
