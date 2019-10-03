
FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O0 -ip -debug full

TOP=..


ifeq ($(ARCH), GPU)
FC = pgf90 -DACC -acc -byteswapio -Mlarge_arrays -ta=tesla:managed:cc60 -Minfo=all -Mcuda -O1 -I$(TOP)
LD = pgf90 -DACC -acc -byteswapio -Mlarge_arrays -ta=tesla:managed:cc60 -Minfo=all -Mcuda=rdc #code relogable à gérer
endif

ifeq ($(ARCH), CPU)
FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -I$(TOP)
LD = pgf90 -mp -byteswapio -Mlarge_arrays 
endif



all: wrap_lacdyn.x

MODULES=parkind1.o intdyn_mod.o par_rdlr.o ptrslb1.o ptrslb2.o yomcst.o yomct0.o yomct3.o yomcver.o yomdim.o yomdimv.o yomdyn.o yomdyna.o yomgmv.o yomorog.o yomsta.o yomvert.o load_mod.o yemdyn.o yoephy.o yom_ygfl.o yomgem.o yomlddh.o yommddh.o yomparar.o yomphy.o yomgsgeom.o crmdims.o type_gmvs.o xrd_unix_env.o xrd_getoptions.o

yomlddh.o: $(TOP)/yomlddh.F90 
	$(FC) -c $(TOP)/yomlddh.F90

yomparar.o: $(TOP)/yomparar.F90 parkind1.o
	$(FC) -c $(TOP)/yomparar.F90

par_rdlr.o: $(TOP)/par_rdlr.F90 parkind1.o
	$(FC) -c $(TOP)/par_rdlr.F90

yommddh.o: $(TOP)/yommddh.F90 parkind1.o
	$(FC) -c $(TOP)/yommddh.F90

yomdim.o: $(TOP)/yomdim.F90 parkind1.o
	$(FC) -c $(TOP)/yomdim.F90

yomorog.o: $(TOP)/yomorog.F90 parkind1.o
	$(FC) -c $(TOP)/yomorog.F90

yomct0.o: $(TOP)/yomct0.F90 parkind1.o
	$(FC) -c $(TOP)/yomct0.F90

lattes.o: $(TOP)/lattes.F90 $(MODULES)
	$(FC) -c $(TOP)/lattes.F90

yomcver.o: $(TOP)/yomcver.F90 parkind1.o
	$(FC) -c $(TOP)/yomcver.F90

crmdims.o: $(TOP)/crmdims.F90 parkind1.o
	$(FC) -c $(TOP)/crmdims.F90

yemdyn.o: $(TOP)/yemdyn.F90 parkind1.o
	$(FC) -c $(TOP)/yemdyn.F90

type_gmvs.o: $(TOP)/type_gmvs.F90 parkind1.o
	$(FC) -c $(TOP)/type_gmvs.F90

lacdyn_load_all.o: $(TOP)/lacdyn_load_all.F90 $(MODULES)
	$(FC) -c $(TOP)/lacdyn_load_all.F90

ptrslb1.o: $(TOP)/ptrslb1.F90 parkind1.o
	$(FC) -c $(TOP)/ptrslb1.F90

yomgsgeom.o: $(TOP)/yomgsgeom.F90 parkind1.o
	$(FC) -c $(TOP)/yomgsgeom.F90

load_mod.o: $(TOP)/load_mod.F90 parkind1.o yom_ygfl.o yomgmv.o yomgsgeom.o yomorog.o
	$(FC) -c $(TOP)/load_mod.F90

sitnu.o: $(TOP)/sitnu.F90 $(MODULES)
	$(FC) -c $(TOP)/sitnu.F90

sigam.o: $(TOP)/sigam.F90 $(MODULES)
	$(FC) -c $(TOP)/sigam.F90

yom_ygfl.o: $(TOP)/yom_ygfl.F90 crmdims.o parkind1.o
	$(FC) -c $(TOP)/yom_ygfl.F90

yomphy.o: $(TOP)/yomphy.F90 parkind1.o
	$(FC) -c $(TOP)/yomphy.F90

lasure.o: $(TOP)/lasure.F90 $(MODULES)
	$(FC) -c $(TOP)/lasure.F90

lavabo.o: $(TOP)/lavabo.F90 $(MODULES)
	$(FC) -c $(TOP)/lavabo.F90

yomvert.o: $(TOP)/yomvert.F90 parkind1.o
	$(FC) -c $(TOP)/yomvert.F90

ptrslb2.o: $(TOP)/ptrslb2.F90 parkind1.o
	$(FC) -c $(TOP)/ptrslb2.F90

yomdyna.o: $(TOP)/yomdyna.F90 parkind1.o
	$(FC) -c $(TOP)/yomdyna.F90

yomdimv.o: $(TOP)/yomdimv.F90 parkind1.o
	$(FC) -c $(TOP)/yomdimv.F90

lattex.o: $(TOP)/lattex.F90 $(MODULES)
	$(FC) -c $(TOP)/lattex.F90

lacdyn.o: $(TOP)/lacdyn.F90 $(MODULES)
	$(FC) -c $(TOP)/lacdyn.F90

lassie.o: $(TOP)/lassie.F90 $(MODULES)
	$(FC) -c $(TOP)/lassie.F90

gprcp.o: $(TOP)/gprcp.F90 $(MODULES)
	$(FC) -c $(TOP)/gprcp.F90

lattex_dnt.o: $(TOP)/lattex_dnt.F90 $(MODULES)
	$(FC) -c $(TOP)/lattex_dnt.F90

yomdyn.o: $(TOP)/yomdyn.F90 parkind1.o
	$(FC) -c $(TOP)/yomdyn.F90

yomgmv.o: $(TOP)/yomgmv.F90 parkind1.o type_gmvs.o
	$(FC) -c $(TOP)/yomgmv.F90

yoephy.o: $(TOP)/yoephy.F90 parkind1.o
	$(FC) -c $(TOP)/yoephy.F90

yomsta.o: $(TOP)/yomsta.F90 parkind1.o
	$(FC) -c $(TOP)/yomsta.F90

intdyn_mod.o: $(TOP)/intdyn_mod.F90 parkind1.o
	$(FC) -c $(TOP)/intdyn_mod.F90

yomcst.o: $(TOP)/yomcst.F90 parkind1.o
	$(FC) -c $(TOP)/yomcst.F90

yomct3.o: $(TOP)/yomct3.F90 parkind1.o
	$(FC) -c $(TOP)/yomct3.F90

xrd_unix_env.o: $(TOP)/xrd_unix_env.F90 parkind1.o
	$(FC) -c $(TOP)/xrd_unix_env.F90

xrd_getoptions.o: $(TOP)/xrd_getoptions.F90 parkind1.o xrd_unix_env.o
	$(FC) -c $(TOP)/xrd_getoptions.F90

parkind1.o: $(TOP)/parkind1.F90 
	$(FC) -c $(TOP)/parkind1.F90

lavent.o: $(TOP)/lavent.F90 $(MODULES)
	$(FC) -c $(TOP)/lavent.F90

yomgem.o: $(TOP)/yomgem.F90 parkind1.o
	$(FC) -c $(TOP)/yomgem.F90

abor1.o: $(TOP)/abor1.F90
	$(FC) -c $(TOP)/abor1.F90

notfunny.o: $(TOP)/notfunny.F90
	$(FC) -c $(TOP)/notfunny.F90

wrap_lacdyn.o: $(TOP)/wrap_lacdyn.F90 $(MODULES)
	$(FC) -c $(TOP)/wrap_lacdyn.F90

wrap_lacdyn.x: wrap_lacdyn.o notfunny.o yomlddh.o yomparar.o par_rdlr.o yommddh.o yomdim.o yomorog.o yomct0.o lattes.o yomcver.o crmdims.o yemdyn.o type_gmvs.o lacdyn_load_all.o ptrslb1.o yomgsgeom.o load_mod.o sitnu.o yom_ygfl.o yomphy.o lasure.o lavabo.o yomvert.o ptrslb2.o yomdyna.o yomdimv.o lattex.o lacdyn.o lassie.o gprcp.o lattex_dnt.o yomdyn.o yomgmv.o yoephy.o yomsta.o intdyn_mod.o yomcst.o yomct3.o parkind1.o lavent.o yomgem.o sigam.o abor1.o xrd_unix_env.o xrd_getoptions.o
	$(LD) -o wrap_lacdyn.x wrap_lacdyn.o notfunny.o yomlddh.o yomparar.o par_rdlr.o yommddh.o yomdim.o yomorog.o yomct0.o lattes.o yomcver.o crmdims.o yemdyn.o type_gmvs.o lacdyn_load_all.o ptrslb1.o yomgsgeom.o load_mod.o sitnu.o yom_ygfl.o yomphy.o lasure.o lavabo.o yomvert.o ptrslb2.o yomdyna.o yomdimv.o lattex.o lacdyn.o lassie.o gprcp.o lattex_dnt.o yomdyn.o yomgmv.o yoephy.o yomsta.o intdyn_mod.o yomcst.o yomct3.o parkind1.o lavent.o yomgem.o abor1.o sigam.o xrd_unix_env.o xrd_getoptions.o

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

