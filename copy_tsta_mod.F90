MODULE COPY_TSTA_MOD

USE YOMSTA, ONLY : TSTA

INTERFACE COPY
MODULE PROCEDURE COPY_TSTA
END INTERFACE

CONTAINS

SUBROUTINE COPY_TSTA (YD)

IMPLICIT NONE
TYPE (TSTA), INTENT (IN) :: YD

!$acc enter data create (YD%STPREH)
!$acc update device (YD%STPREH)
!$acc enter data attach (YD%STPREH)

!$acc enter data create (YD%STPRE)
!$acc update device (YD%STPRE)
!$acc enter data attach (YD%STPRE)

!$acc enter data create (YD%STPHI)
!$acc update device (YD%STPHI)
!$acc enter data attach (YD%STPHI)

!$acc enter data create (YD%STTEM)
!$acc update device (YD%STTEM)
!$acc enter data attach (YD%STTEM)

!$acc enter data create (YD%STDEN)
!$acc update device (YD%STDEN)
!$acc enter data attach (YD%STDEN)

!$acc enter data create (YD%STZ)
!$acc update device (YD%STZ)
!$acc enter data attach (YD%STZ)

!$acc update device (YD%RZTROP)

!$acc update device (YD%RZSTRA)

!$acc update device (YD%RZSTR2)

!$acc update device (YD%RZSTPO)

!$acc update device (YD%RZMESO)

!$acc update device (YD%RZMES2)

!$acc update device (YD%RZMEPO)

!$acc update device (YD%RZABOV)

!$acc update device (YD%RTSUR)

!$acc update device (YD%RTTROP)

!$acc update device (YD%RTSTRA)

!$acc update device (YD%RTSTR2)

!$acc update device (YD%RTSTPO)

!$acc update device (YD%RTMESO)

!$acc update device (YD%RTMES2)

!$acc update device (YD%RTMEPO)

!$acc update device (YD%RTABOV)

!$acc update device (YD%RPTROP)

!$acc update device (YD%RPSTRA)

!$acc update device (YD%RPSTR2)

!$acc update device (YD%RPSTPO)

!$acc update device (YD%RPMESO)

!$acc update device (YD%RPMES2)

!$acc update device (YD%RPMEPO)

!$acc update device (YD%RPABOV)

!$acc update device (YD%RDTDZ1)

!$acc update device (YD%RDTDZ2)

!$acc update device (YD%RDTDZ3)

!$acc update device (YD%RDTDZ4)

!$acc update device (YD%RDTDZ5)

!$acc update device (YD%RDTDZ6)

!$acc update device (YD%RDTDZ7)

!$acc update device (YD%RDTDZ8)

!$acc update device (YD%RDTDZ9)

!$acc update device (YD%VZTROP)

!$acc update device (YD%VZSTRA)

!$acc update device (YD%VZSTR2)

!$acc update device (YD%VZSTPO)

!$acc update device (YD%VZMESO)

!$acc update device (YD%VZMES2)

!$acc update device (YD%VZMEPO)

!$acc update device (YD%VZABOV)

!$acc update device (YD%VTSUR)

!$acc update device (YD%VTTROP)

!$acc update device (YD%VTSTRA)

!$acc update device (YD%VTSTR2)

!$acc update device (YD%VTSTPO)

!$acc update device (YD%VTMESO)

!$acc update device (YD%VTMES2)

!$acc update device (YD%VTMEPO)

!$acc update device (YD%VTABOV)

!$acc update device (YD%VPTROP)

!$acc update device (YD%VPSTRA)

!$acc update device (YD%VPSTR2)

!$acc update device (YD%VPSTPO)

!$acc update device (YD%VPMESO)

!$acc update device (YD%VPMES2)

!$acc update device (YD%VPMEPO)

!$acc update device (YD%VPABOV)

!$acc update device (YD%VDTDZ1)

!$acc update device (YD%VDTDZ2)

!$acc update device (YD%VDTDZ3)

!$acc update device (YD%VDTDZ4)

!$acc update device (YD%VDTDZ5)

!$acc update device (YD%VDTDZ6)

!$acc update device (YD%VDTDZ7)

!$acc update device (YD%VDTDZ8)

!$acc update device (YD%VDTDZ9)

!$acc update device (YD%HEXTRAP)

!$acc update device (YD%NLEXTRAP)

END SUBROUTINE

END MODULE
