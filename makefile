

TOP=..


ifeq ($(ARCH), GPU)
FC = pgf90 -DACC -acc -byteswapio -Mlarge_arrays -ta=tesla:managed:cc60 -Minfo=all -Mcuda -O2 -I$(TOP)
LD = pgf90 -DACC -acc -byteswapio -Mlarge_arrays -ta=tesla:managed:cc60 -Minfo=all -Mcuda=rdc 
endif

ifeq ($(ARCH), CPU)
FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -I$(TOP)
LD = pgf90 -mp -byteswapio -Mlarge_arrays 
endif



all: wrap_lacdyn.x


wrap_lacdyn.o: $(TOP)/wrap_lacdyn.F90 
	$(FC) -c $(TOP)/wrap_lacdyn.F90

wrap_lacdyn.x: wrap_lacdyn.o 
	$(LD) -o wrap_lacdyn.x wrap_lacdyn.o 

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

