MODULE COPY_TPARAR_MOD

USE YOMPARAR, ONLY : TPARAR

INTERFACE COPY
MODULE PROCEDURE COPY_TPARAR
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPARAR (YD)

IMPLICIT NONE
TYPE (TPARAR), INTENT (IN) :: YD

!$acc update device (YD%CMICRO)

!$acc update device (YD%CSEDIM)

!$acc update device (YD%NSPLITR)

!$acc update device (YD%NRR)

!$acc update device (YD%NRRL)

!$acc update device (YD%NRRI)

!$acc update device (YD%NSV)

!$acc update device (YD%NSWB_MNH)

!$acc update device (YD%NGPAR)

!$acc update device (YD%MINPRR)

!$acc update device (YD%MACPRR)

!$acc update device (YD%MINPRS)

!$acc update device (YD%MACPRS)

!$acc update device (YD%MINPRG)

!$acc update device (YD%MACPRG)

!$acc update device (YD%MALBDIR)

!$acc update device (YD%MALBSCA)

!$acc update device (YD%MRAIN)

!$acc update device (YD%MSNOW)

!$acc update device (YD%MGZ0)

!$acc update device (YD%MGZ0H)

!$acc update device (YD%MVQS)

!$acc update device (YD%MVTS)

!$acc update device (YD%MVEMIS)

!$acc update device (YD%MSWDIR)

!$acc update device (YD%MSWDIF)

!$acc update device (YD%MFRTHDS)

!$acc update device (YD%MLSM)

!$acc update device (YD%LOSUBG_COND)

!$acc update device (YD%LOSEDIC)

!$acc update device (YD%LOWARM)

!$acc update device (YD%LOSIGMAS)

!$acc update device (YD%LOLSMC)

!$acc update device (YD%LOTOWNC)

!$acc update device (YD%LOCND2)

!$acc update device (YD%RADGR)

!$acc update device (YD%RADSN)

!$acc update device (YD%VSIGQSAT)

!$acc update device (YD%CMF_UPDRAFT)

!$acc update device (YD%CMF_CLOUD)

!$acc update device (YD%LMIXUV)

!$acc update device (YD%XALP_PERT)

!$acc update device (YD%XABUO)

!$acc update device (YD%XBENTR)

!$acc update device (YD%XBDETR)

!$acc update device (YD%XCMF)

!$acc update device (YD%XENTR_MF)

!$acc update device (YD%XCRAD_MF)

!$acc update device (YD%XENTR_DRY)

!$acc update device (YD%XDETR_DRY)

!$acc update device (YD%XDETR_LUP)

!$acc update device (YD%XKCF_MF)

!$acc update device (YD%XKRC_MF)

!$acc update device (YD%XTAUSIGMF)

!$acc update device (YD%XPRES_UV)

!$acc update device (YD%XFRAC_UP_MAX)

!$acc update device (YD%XALPHA_MF)

!$acc update device (YD%XSIGMA_MF)

!$acc update device (YD%XA1)

!$acc update device (YD%XB)

!$acc update device (YD%XC)

!$acc update device (YD%XBETA1)

!$acc update device (YD%XR)

!$acc update device (YD%XLAMBDA)

!$acc update device (YD%LTHETAS)

!$acc update device (YD%LSQUALL)

!$acc update device (YD%NREFROI1)

!$acc update device (YD%NREFROI2)

!$acc update device (YD%VSQUALL)

!$acc update device (YD%NPTP)

!$acc update device (YD%NPRINTFR)

!$acc update device (YD%LDIAGWMAX)

!$acc update device (YD%NDIAGWMAX)

!$acc update device (YD%NDTCHEM)

!$acc update device (YD%LAROBU_ENABLE)

!$acc update device (YD%XLINI)

!$acc update device (YD%CSUBG_AUCV_RC)

!$acc update device (YD%CSUBG_RC_RR_ACCR)

!$acc update device (YD%CSUBG_RR_EVAP)

!$acc update device (YD%CSUBG_RPR_PDF)

!$acc update device (YD%RCRIAUTI)

!$acc update device (YD%RCRIAUTC)

!$acc update device (YD%RT0CRIAUTI)

!$acc update device (YD%LCRIAUTI)

!$acc update device (YD%LFPREC3D)

!$acc update device (YD%XCQVR)

!$acc update device (YD%GQVPLIM)

!$acc update device (YD%GQVTOP)

!$acc update device (YD%LQVTOP)

END SUBROUTINE

END MODULE
