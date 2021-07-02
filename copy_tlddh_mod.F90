MODULE COPY_TLDDH_MOD

USE YOMLDDH, ONLY : TLDDH

INTERFACE COPY
MODULE PROCEDURE COPY_TLDDH
END INTERFACE

CONTAINS

SUBROUTINE COPY_TLDDH (YD)

IMPLICIT NONE
TYPE (TLDDH), INTENT (IN) :: YD

!$acc update device (YD%LSDDH)

!$acc update device (YD%LHDGLB)

!$acc update device (YD%LHDZON)

!$acc update device (YD%LHDDOP)

!$acc update device (YD%LHDLFA)

!$acc update device (YD%LHDHKS)

!$acc update device (YD%LHDMCI)

!$acc update device (YD%LHDENT)

!$acc update device (YD%LHDPRG)

!$acc update device (YD%LHDPRZ)

!$acc update device (YD%LHDPRD)

!$acc update device (YD%LHDPR)

!$acc update device (YD%LHDEFG)

!$acc update device (YD%LHDEFZ)

!$acc update device (YD%LHDEFD)

!$acc update device (YD%LHDLIST)

!$acc update device (YD%LHDOUFG)

!$acc update device (YD%LHDOUFZ)

!$acc update device (YD%LHDOUFD)

!$acc update device (YD%LHDOUP)

!$acc update device (YD%LHDFIL)

!$acc update device (YD%LONLYVAR)

!$acc update device (YD%LHDORIGP)

!$acc update device (YD%LHDCDPI)

!$acc update device (YD%LFLEXDIA)

!$acc update device (YD%LRDDHDYN)

!$acc update device (YD%LRSLDDH)

!$acc update device (YD%LRSIDDH)

!$acc update device (YD%LRHDDDH)

!$acc update device (YD%LDDH_OMP)

END SUBROUTINE

END MODULE
