MODULE COPY_TYPE_T0_MOD

USE TYPE_GMVS, ONLY : TYPE_T0

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_T0
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_T0 (YD)

IMPLICIT NONE
TYPE (TYPE_T0), INTENT (IN) :: YD

!$acc update device (YD%NDIM)

!$acc update device (YD%NDIMS)

!$acc update device (YD%NDIMUV)

!$acc update device (YD%MU)

!$acc update device (YD%MV)

!$acc update device (YD%MT)

!$acc update device (YD%MTL)

!$acc update device (YD%MTM)

!$acc update device (YD%MDIV)

!$acc update device (YD%MVOR)

!$acc update device (YD%MUL)

!$acc update device (YD%MVL)

!$acc update device (YD%MSPD)

!$acc update device (YD%MSPDL)

!$acc update device (YD%MSPDM)

!$acc update device (YD%MSVD)

!$acc update device (YD%MSVDL)

!$acc update device (YD%MSVDM)

!$acc update device (YD%MNHX)

!$acc update device (YD%MNHXL)

!$acc update device (YD%MNHXM)

!$acc update device (YD%MEDOT)

!$acc update device (YD%MSGRTL)

!$acc update device (YD%MSGRTM)

!$acc update device (YD%MSP)

!$acc update device (YD%MSPL)

!$acc update device (YD%MSPM)

END SUBROUTINE

END MODULE
