CXX         = /usr/bin/CC

EXTRA_FLAGS = -LANG:std -O2 -64 -mips4 -all -DHAVE_CONFIG_H \
              -woff 1183,3322 -w2 -DOPENGL -g -gslim -ansi \
              -D_IRIX -mp -DUSE_OMP

COMPILE     = ${CXX} $(EXTRA_FLAGS) $(OMNIFLAGS)

LINK        = ${CXX} -LANG:std -64 -mp

#-----------------------Juggler Stuff-----------------------
VJ_INCLUDES = -I$(VJ_BASE_DIR)/include -I$(srcdir) -I/usr/include \
               -I$(VJ_BASE_DIR)/external/GMTL \
               -I$(VJ_BASE_DIR)/include/boost/compatibility/cpp_c_headers

VJ_LIBS     = -B dynamic -L${VJ_BASE_DIR}/lib64 \
               -lJuggler -lJuggler_pf 

#VJ_LIB_DEPS = $(VJ_BASE_DIR)/lib32/libJuggler.a \
              $(VJ_BASE_DIR)/lib32/libJuggler_ogl.a \
              $(VJ_BASE_DIR)/lib32/libJuggler_pf.a

#------------------------Extra Stuff------------------------
EXTRA_LIBS  = -L/usr/lib64 -L/usr/lib64 -L/usr \
              -L/usr/lib64/Performer/Debug -lpf -lpfdu -lpfui -lpfutil \
              -lGLU -lGL -lgadget -ljccl -lvpr \
              -limage -lXmu -lXext -lXt -lX11 \
              -lm -lC
