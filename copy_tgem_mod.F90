MODULE COPY_TGEM_MOD

USE YOMGEM, ONLY : TGEM

INTERFACE COPY
MODULE PROCEDURE COPY_TGEM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TGEM (YD)

IMPLICIT NONE
TYPE (TGEM), INTENT (IN) :: YD

!$acc update device (YD%NGPTOT)

!$acc update device (YD%NGPTOT_CAP)

!$acc update device (YD%NGPTOTMX)

!$acc update device (YD%NGPTOTG)

!$acc update device (YD%RDELXN)

!$acc update device (YD%SLHDP)

!$acc update device (YD%RMUCEN)

!$acc update device (YD%RLOCEN)

!$acc update device (YD%RSTRET)

!$acc update device (YD%NSTTYP)

!$acc update device (YD%NHTYP)

!$acc update device (YD%RNLGINC)

!$acc update device (YD%R4JP)

!$acc update device (YD%RC2P1)

!$acc update device (YD%RC2M1)

!$acc update device (YD%RCOR0)

!$acc update device (YD%RCOR1)

END SUBROUTINE

END MODULE
