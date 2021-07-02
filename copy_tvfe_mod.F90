MODULE COPY_TVFE_MOD

USE YOMVERT, ONLY : TVFE

INTERFACE COPY
MODULE PROCEDURE COPY_TVFE
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVFE (YD)

IMPLICIT NONE
TYPE (TVFE), INTENT (IN) :: YD

!$acc enter data create (YD%RINTE)
!$acc update device (YD%RINTE)
!$acc enter data attach (YD%RINTE)

!$acc enter data create (YD%RINTBF11)
!$acc update device (YD%RINTBF11)
!$acc enter data attach (YD%RINTBF11)

!$acc enter data create (YD%RDERI)
!$acc update device (YD%RDERI)
!$acc enter data attach (YD%RDERI)

!$acc enter data create (YD%RDERB)
!$acc update device (YD%RDERB)
!$acc enter data attach (YD%RDERB)

!$acc enter data create (YD%RDDERI)
!$acc update device (YD%RDDERI)
!$acc enter data attach (YD%RDDERI)

!$acc enter data create (YD%RDERBH00)
!$acc update device (YD%RDERBH00)
!$acc enter data attach (YD%RDERBH00)

!$acc enter data create (YD%RDERBF00)
!$acc update device (YD%RDERBF00)
!$acc enter data attach (YD%RDERBF00)

!$acc enter data create (YD%RDERBF01)
!$acc update device (YD%RDERBF01)
!$acc enter data attach (YD%RDERBF01)

!$acc enter data create (YD%RDDERBF01)
!$acc update device (YD%RDDERBF01)
!$acc enter data attach (YD%RDDERBF01)

END SUBROUTINE

END MODULE
