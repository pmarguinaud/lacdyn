
FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O0 -ip -debug full

#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS)
FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays
FC = pgf90 -DACC -acc -byteswapio -Mlarge_arrays -ta=tesla:managed,cc60 -Minfo=all 

all: wrap_lacdyn.x

MODULES=parkind1.o intdyn_mod.o par_rdlr.o ptrslb1.o ptrslb2.o yomcst.o yomct0.o yomct3.o yomcver.o yomdim.o yomdimv.o yomdyn.o yomdyna.o yomgmv.o yomorog.o yomsta.o yomvert.o load_mod.o yemdyn.o yoephy.o yom_ygfl.o yomgem.o yomlddh.o yommddh.o yomparar.o yomphy.o yomgsgeom.o crmdims.o type_gmvs.o xrd_unix_env.o xrd_getoptions.o

yomlddh.o: yomlddh.F90 
	$(FC) -c yomlddh.F90

yomparar.o: yomparar.F90 parkind1.o
	$(FC) -c yomparar.F90

par_rdlr.o: par_rdlr.F90 parkind1.o
	$(FC) -c par_rdlr.F90

yommddh.o: yommddh.F90 parkind1.o
	$(FC) -c yommddh.F90

yomdim.o: yomdim.F90 parkind1.o
	$(FC) -c yomdim.F90

yomorog.o: yomorog.F90 parkind1.o
	$(FC) -c yomorog.F90

yomct0.o: yomct0.F90 parkind1.o
	$(FC) -c yomct0.F90

lattes.o: lattes.F90 $(MODULES)
	$(FC) -c lattes.F90

yomcver.o: yomcver.F90 parkind1.o
	$(FC) -c yomcver.F90

crmdims.o: crmdims.F90 parkind1.o
	$(FC) -c crmdims.F90

yemdyn.o: yemdyn.F90 parkind1.o
	$(FC) -c yemdyn.F90

type_gmvs.o: type_gmvs.F90 parkind1.o
	$(FC) -c type_gmvs.F90

lacdyn_load_all.o: lacdyn_load_all.F90 $(MODULES)
	$(FC) -c lacdyn_load_all.F90

ptrslb1.o: ptrslb1.F90 parkind1.o
	$(FC) -c ptrslb1.F90

yomgsgeom.o: yomgsgeom.F90 parkind1.o
	$(FC) -c yomgsgeom.F90

load_mod.o: load_mod.F90 parkind1.o yom_ygfl.o yomgmv.o yomgsgeom.o yomorog.o
	$(FC) -c load_mod.F90

sitnu.o: sitnu.F90 $(MODULES)
	$(FC) -c sitnu.F90

sigam.o: sigam.F90 $(MODULES)
	$(FC) -c sigam.F90

yom_ygfl.o: yom_ygfl.F90 crmdims.o parkind1.o
	$(FC) -c yom_ygfl.F90

yomphy.o: yomphy.F90 parkind1.o
	$(FC) -c yomphy.F90

lasure.o: lasure.F90 $(MODULES)
	$(FC) -c lasure.F90

lavabo.o: lavabo.F90 $(MODULES)
	$(FC) -c lavabo.F90

yomvert.o: yomvert.F90 parkind1.o
	$(FC) -c yomvert.F90

ptrslb2.o: ptrslb2.F90 parkind1.o
	$(FC) -c ptrslb2.F90

yomdyna.o: yomdyna.F90 parkind1.o
	$(FC) -c yomdyna.F90

yomdimv.o: yomdimv.F90 parkind1.o
	$(FC) -c yomdimv.F90

lattex.o: lattex.F90 $(MODULES)
	$(FC) -c lattex.F90

lacdyn.o: lacdyn.F90 $(MODULES)
	$(FC) -c lacdyn.F90

lassie.o: lassie.F90 $(MODULES)
	$(FC) -c lassie.F90

gprcp.o: gprcp.F90 $(MODULES)
	$(FC) -c gprcp.F90

lattex_dnt.o: lattex_dnt.F90 $(MODULES)
	$(FC) -c lattex_dnt.F90

yomdyn.o: yomdyn.F90 parkind1.o
	$(FC) -c yomdyn.F90

yomgmv.o: yomgmv.F90 parkind1.o type_gmvs.o
	$(FC) -c yomgmv.F90

yoephy.o: yoephy.F90 parkind1.o
	$(FC) -c yoephy.F90

yomsta.o: yomsta.F90 parkind1.o
	$(FC) -c yomsta.F90

intdyn_mod.o: intdyn_mod.F90 parkind1.o
	$(FC) -c intdyn_mod.F90

yomcst.o: yomcst.F90 parkind1.o
	$(FC) -c yomcst.F90

yomct3.o: yomct3.F90 parkind1.o
	$(FC) -c yomct3.F90

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90

xrd_getoptions.o: xrd_getoptions.F90 parkind1.o xrd_unix_env.o
	$(FC) -c xrd_getoptions.F90

parkind1.o: parkind1.F90 
	$(FC) -c parkind1.F90

lavent.o: lavent.F90 $(MODULES)
	$(FC) -c lavent.F90

yomgem.o: yomgem.F90 parkind1.o
	$(FC) -c yomgem.F90

abor1.o: abor1.F90
	$(FC) -c abor1.F90

wrap_lacdyn.o: wrap_lacdyn.F90 $(MODULES)
	$(FC) -c wrap_lacdyn.F90

wrap_lacdyn.x: wrap_lacdyn.o yomlddh.o yomparar.o par_rdlr.o yommddh.o yomdim.o yomorog.o yomct0.o lattes.o yomcver.o crmdims.o yemdyn.o type_gmvs.o lacdyn_load_all.o ptrslb1.o yomgsgeom.o load_mod.o sitnu.o yom_ygfl.o yomphy.o lasure.o lavabo.o yomvert.o ptrslb2.o yomdyna.o yomdimv.o lattex.o lacdyn.o lassie.o gprcp.o lattex_dnt.o yomdyn.o yomgmv.o yoephy.o yomsta.o intdyn_mod.o yomcst.o yomct3.o parkind1.o lavent.o yomgem.o sigam.o abor1.o xrd_unix_env.o xrd_getoptions.o
	$(FC) -o wrap_lacdyn.x wrap_lacdyn.o yomlddh.o yomparar.o par_rdlr.o yommddh.o yomdim.o yomorog.o yomct0.o lattes.o yomcver.o crmdims.o yemdyn.o type_gmvs.o lacdyn_load_all.o ptrslb1.o yomgsgeom.o load_mod.o sitnu.o yom_ygfl.o yomphy.o lasure.o lavabo.o yomvert.o ptrslb2.o yomdyna.o yomdimv.o lattex.o lacdyn.o lassie.o gprcp.o lattex_dnt.o yomdyn.o yomgmv.o yoephy.o yomsta.o intdyn_mod.o yomcst.o yomct3.o parkind1.o lavent.o yomgem.o abor1.o sigam.o xrd_unix_env.o xrd_getoptions.o

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

