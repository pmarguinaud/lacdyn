
FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O0 -ip -debug full

#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS)
FC = pgf90 -mp -byteswapio -Mlarge_arrays -O0 -g -DUSE_STACK
#FC = pgf90 -acc -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O2 -g -DUSE_STACK -ta=tesla:lineinfo -DUSE_ACC

FC = pgf90 -acc -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla -O2 -g -DUSE_STACK -ta=tesla:lineinfo -DUSE_ACC \
     -L/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/cuda/10.2/lib64 -Wl,-rpath,/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/cuda/10.2/lib64 \
     -L/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/math_libs/10.2/lib64 -Wl,-rpath,/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/math_libs/10.2/lib64 \
     -L/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/compilers/lib -Wl,-rpath,/gpfslocalsys/nvhpc/21.5/Linux_x86_64/21.5/compilers/lib \
     -lcudafor -lcudart -lcudafor_102 -lcublas -lcudanvhpc 


