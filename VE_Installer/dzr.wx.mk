EXTRA_CXXFLAGS+= $(shell wx-config --cxxflags) -D WXUSING_PLUGIN_DLL -DWXUSINGDLL=1
EXTRA_LIBS+= $(shell wx-config --libs)
