EXTRA_CXXFLAGS+= $(shell wx-config --cxxflags) -DWXUSING_PLUGIN_DLL -DWXUSINGDLL
EXTRA_LIBS+= $(shell wx-config --libs)
