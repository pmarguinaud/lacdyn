MODULE COPY_TYPE_T9_MOD

USE TYPE_GMVS, ONLY : TYPE_T9

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_T9
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_T9 (YD)

IMPLICIT NONE
TYPE (TYPE_T9), INTENT (IN) :: YD

!$acc update device (YD%NDIM)

!$acc update device (YD%NDIMS)

!$acc update device (YD%MU)

!$acc update device (YD%MV)

!$acc update device (YD%MT)

!$acc update device (YD%MTL)

!$acc update device (YD%MTM)

!$acc update device (YD%MEDOT)

!$acc update device (YD%MUNL)

!$acc update device (YD%MUNL_SI)

!$acc update device (YD%MVNL)

!$acc update device (YD%MVNL_SI)

!$acc update device (YD%MTNL)

!$acc update device (YD%MTNL_SI)

!$acc update device (YD%MSPNL)

!$acc update device (YD%MSPNL_SI)

!$acc update device (YD%MVCASRS)

!$acc update device (YD%MRDLASR)

!$acc update device (YD%MVWVNL)

!$acc update device (YD%MSVDNL_SI)

!$acc update device (YD%MSPDNL)

!$acc update device (YD%MSPDNL_SI)

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

!$acc update device (YD%MGW)

!$acc update device (YD%MCURHS)

!$acc update device (YD%MCVRHS)

!$acc update device (YD%MCTRHS)

!$acc update device (YD%MCSPRHS)

!$acc update device (YD%MCSPDRHS)

!$acc update device (YD%MCSVDRHS)

!$acc update device (YD%MCUNL)

!$acc update device (YD%MCVNL)

!$acc update device (YD%MCTNL)

!$acc update device (YD%MCSPNL)

!$acc update device (YD%MCVWVNL)

!$acc update device (YD%MCSPDNL)

!$acc update device (YD%MCUPT)

!$acc update device (YD%MCVPT)

!$acc update device (YD%MCTPT)

!$acc update device (YD%MCSPDPT)

!$acc update device (YD%MCSVDPT)

!$acc update device (YD%MDPHI)

!$acc update device (YD%MNHXNL)

!$acc update device (YD%MCNHXNL)

!$acc update device (YD%MSGRTL)

!$acc update device (YD%MSGRTM)

!$acc update device (YD%MSP)

!$acc update device (YD%MSPL)

!$acc update device (YD%MSPM)

!$acc update device (YD%MCSPPT)

!$acc update device (YD%MDBBC)

!$acc update device (YD%MGWS)

END SUBROUTINE

END MODULE
