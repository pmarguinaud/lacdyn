MODULE COPY_TMDDH_MOD

USE YOMMDDH, ONLY : TMDDH

INTERFACE COPY
MODULE PROCEDURE COPY_TMDDH
END INTERFACE

CONTAINS

SUBROUTINE COPY_TMDDH (YD)

IMPLICIT NONE
TYPE (TMDDH), INTENT (IN) :: YD

!$acc update device (YD%NDHKD)

!$acc update device (YD%NDHNPU)

!$acc update device (YD%NDHBPU)

!$acc update device (YD%NDHBPX)

!$acc update device (YD%NDHNOM)

!$acc update device (YD%NDHDDX)

!$acc update device (YD%NDHIDH)

!$acc update device (YD%NDHCS)

!$acc update device (YD%NDHCV)

!$acc update device (YD%NDHCVSU)

!$acc update device (YD%NDHCSSU)

!$acc update device (YD%NDHCVSUN)

!$acc update device (YD%NDHCVSUL)

!$acc update device (YD%NDHVV)

!$acc update device (YD%NDHFVD)

!$acc update device (YD%NDHFVP)

!$acc update device (YD%NDHVS)

!$acc update device (YD%NDHFSD)

!$acc update device (YD%NDHFSP)

!$acc update device (YD%NDHFFS)

!$acc update device (YD%NDHVFS)

!$acc update device (YD%NFSVAR_AERO)

!$acc update device (YD%NFSFLX_AERO)

!$acc update device (YD%NDHVTLS)

!$acc update device (YD%NDHFTLS)

!$acc update device (YD%NDHVTSS)

!$acc update device (YD%NDHFTSS)

!$acc update device (YD%NDHVTTS)

!$acc update device (YD%NDHFTTS)

!$acc update device (YD%NDHVTIS)

!$acc update device (YD%NDHFTIS)

!$acc update device (YD%NDHVSSS)

!$acc update device (YD%NDHFSSS)

!$acc update device (YD%NDHVIIS)

!$acc update device (YD%NDHFIIS)

!$acc update device (YD%NDHVWLS)

!$acc update device (YD%NDHFWLS)

!$acc update device (YD%NDHTHK)

!$acc update device (YD%NDHVHK)

!$acc update device (YD%NDHFHKD)

!$acc update device (YD%NDHFHKP)

!$acc update device (YD%NDHTMC)

!$acc update device (YD%NDHVMC)

!$acc update device (YD%NDHFMCD)

!$acc update device (YD%NDHFMCP)

!$acc update device (YD%NDHTEN)

!$acc update device (YD%NDHVEN)

!$acc update device (YD%NDHFEND)

!$acc update device (YD%NDHFENP)

!$acc update device (YD%NDHAVD)

!$acc update device (YD%NDHBVD)

!$acc update device (YD%NDHAVP)

!$acc update device (YD%NDHBVP)

!$acc update device (YD%NDHAHKD)

!$acc update device (YD%NDHBHKD)

!$acc update device (YD%NDHAHKP)

!$acc update device (YD%NDHBHKP)

!$acc update device (YD%NDHAMCD)

!$acc update device (YD%NDHBMCD)

!$acc update device (YD%NDHAMCP)

!$acc update device (YD%NDHBMCP)

!$acc update device (YD%NDHAEND)

!$acc update device (YD%NDHBEND)

!$acc update device (YD%NDHAENP)

!$acc update device (YD%NDHBENP)

!$acc update device (YD%NDHZPR)

!$acc update device (YD%FNODDH)

!$acc update device (YD%BDEDDH)

!$acc update device (YD%HDSFGL)

!$acc update device (YD%NDIMHDGFL)

!$acc update device (YD%NDIMSIGMV)

!$acc update device (YD%MSIDDH_U1)

!$acc update device (YD%MSIDDH_V1)

!$acc update device (YD%MSIDDH_T1)

!$acc update device (YD%MSIDDH_PD1)

!$acc update device (YD%MSIDDH_VD1)

!$acc update device (YD%MSIDDH_U0)

!$acc update device (YD%MSIDDH_V0)

!$acc update device (YD%MSIDDH_T0)

!$acc update device (YD%MSIDDH_PD0)

!$acc update device (YD%MSIDDH_VD0)

!$acc update device (YD%MSIDDH_U9)

!$acc update device (YD%MSIDDH_V9)

!$acc update device (YD%MSIDDH_T9)

!$acc update device (YD%MSIDDH_PD9)

!$acc update device (YD%MSIDDH_VD9)

!$acc update device (YD%MHDDDH_U)

!$acc update device (YD%MHDDDH_V)

!$acc update device (YD%MHDDDH_T)

!$acc update device (YD%MHDDDH_Q)

!$acc update device (YD%MHDDDH_PD)

!$acc update device (YD%MHDDDH_VD)

!$acc update device (YD%MHDDDH_NHX)

!$acc update device (YD%MSLDDH_U)

!$acc update device (YD%MSLDDH_V)

!$acc update device (YD%MSLDDH_T)

!$acc update device (YD%MSLDDH_PD)

!$acc update device (YD%MSLDDH_VD)

!$acc update device (YD%MSLDDH_NHX)

!$acc update device (YD%CFPATHDDH)

!$acc update device (YD%NFIELDS3D_AUTO)

!$acc update device (YD%NFIELDS3D_OFFSET)

!$acc update device (YD%NFIELDSMAX)

!$acc update device (YD%NFIELDS2D_AUTO)

!$acc update device (YD%NFIELDS2D_OFFSET)

END SUBROUTINE

END MODULE
