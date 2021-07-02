MODULE COPY_TGMV_MOD

USE YOMGMV, ONLY : TGMV

INTERFACE COPY
MODULE PROCEDURE COPY_TGMV
END INTERFACE

CONTAINS

SUBROUTINE COPY_TGMV (YD)
USE COPY_TYPE_PH9_MOD
USE COPY_TYPE_T0_MOD
USE COPY_TYPE_T1_MOD
USE COPY_TYPE_T9_MOD
IMPLICIT NONE
TYPE (TGMV), INTENT (IN) :: YD

!$acc enter data create (YD%GMV)
!$acc update device (YD%GMV)
!$acc enter data attach (YD%GMV)

!$acc enter data create (YD%GMVS)
!$acc update device (YD%GMVS)
!$acc enter data attach (YD%GMVS)

!$acc enter data create (YD%GMVT1)
!$acc update device (YD%GMVT1)
!$acc enter data attach (YD%GMVT1)

!$acc enter data create (YD%GMVT1S)
!$acc update device (YD%GMVT1S)
!$acc enter data attach (YD%GMVT1S)

!$acc update device (YD%NDIMGMV)

!$acc update device (YD%NDIMGMVS)

CALL COPY (YD%YT0)

CALL COPY (YD%YT9)

CALL COPY (YD%YT1)

CALL COPY (YD%YPH9)

!$acc update device (YD%MGPGMV_U)

!$acc update device (YD%MGPGMV_V)

!$acc update device (YD%MGPGMV_T)

!$acc update device (YD%MGPGMV_SPD)

!$acc update device (YD%MGPGMV_SVD)

!$acc update device (YD%MGPGMV_NHX)

!$acc update device (YD%NGPGMV)

END SUBROUTINE

END MODULE
