EXTRA_CXXFLAGS+= $(shell wx-config --cxxflags) -DWXUSINGDLL
EXTRA_LIBS+= $(shell wx-config --libs)
