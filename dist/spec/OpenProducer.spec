BuildRequires: aaa_base acl attr bash binutils bison bzip2 coreutils cpio cpp devs diffutils doxygen e2fsprogs expat file filesystem findutils gawk gcc gcc-c++ glibc glibc-devel glibc-locale grep gzip info klogd libacl libattr libgcc libstdc++ libstdc++-devel m4 make man mktemp module-init-tools patch permissions popt procinfo procps psmisc pwdutils readline sed sysvinit tar timezone unzip util-linux xorg-x11-Mesa xorg-x11-Mesa-devel xorg-x11-devel xorg-x11-libs zlib zlib-devel openthreads openthreads-devel

Name: OpenProducer
Summary: A C++ Windowing and Device API for OpenGL applications
License: OSGPL
Group: Graphics
Version: 1.1.0
Release: 1 
Source0: %{name}-%{version}.tar.bz2
Packager: Kenneth Mark Bryden 

%description
Open Producer (or simply Producer) is a cross-platform C++/OpenGL library that
is focused on Camera control.  Producer's Camera provides projection ¹, field
of view, viewpoint control, and frame control. Further, Producer can be used in
a multi-tasking environment to allow multiple Camera's to run in parallel
supporting hardware configurations with multiple display subsystems. Threading,
Camera synchronization and frame rate control are simplified in the Producer
programming interface.  

Authors:
--------
   Sean Spicer
   Jack Lees
   Don Burns
   Robert Osfield
   Boris Bralo

%package devel
Summary: Development headers for OpenProducer 
License: OSGPL
Group: Development/C++ 
Version: 1.1.0
Release: 1 

%description devel
This package contains the development headers necessary to compile applications
that use Open Producer.

Open Producer (or simply Producer) is a cross-platform C++/OpenGL library that
is focused on Camera control.  Producer's Camera provides projection ¹, field
of view, viewpoint control, and frame control. Further, Producer can be used in
a multi-tasking environment to allow multiple Camera's to run in parallel
supporting hardware configurations with multiple display subsystems. Threading,
Camera synchronization and frame rate control are simplified in the Producer
programming interface.  

Authors:
--------
Sean Spicer
Jack Lees
Don Burns
Robert Osfield
Boris Bralo

%prep

%setup

%build

make INST_LOCATION=%{_prefix}

%install

make DESTDIR=$RPM_BUILD_ROOT INST_LOCATION=%{_prefix} install

# Install the pkg-config file as well.
touch %{name}.pc.paths
echo "# %{name} pkg-config file" >> %{name}.pc.paths
echo "prefix=%{_prefix}" >> %{name}.pc.paths
echo "exec_prefix=%{_exec_prefix}" >> %{name}.pc.paths
echo "libdir=%{_libdir}" >> %{name}.pc.paths
echo "includedir=%{_includedir}" >> %{name}.pc.paths
cat %{name}.pc.paths producer.pc.rpmin > producer.pc
cp producer.pc %{_prefix}/lib/pkgconfig/producer.pc

%post
%{run_ldconfig}

%postun
%{run_ldconfig}

%files
%defattr(755, root, root)
%{_libdir}/libProducer.so

%files devel
%defattr(644, root, root)
%{_prefix}/lib/pkgconfig/producer.pc
%{_includedir}/Producer/Block
%{_includedir}/Producer/BlockingQueue
%{_includedir}/Producer/Camera
%{_includedir}/Producer/CameraConfig
%{_includedir}/Producer/CameraGroup
%{_includedir}/Producer/Events
%{_includedir}/Producer/Export
%{_includedir}/Producer/InputArea
%{_includedir}/Producer/Keyboard
%{_includedir}/Producer/KeyboardMouse
%{_includedir}/Producer/Math
%{_includedir}/Producer/PipeTimer
%{_includedir}/Producer/Referenced
%{_includedir}/Producer/RefOpenThreads
%{_includedir}/Producer/RenderSurface
%{_includedir}/Producer/System
%{_includedir}/Producer/Timer
%{_includedir}/Producer/Trackball
%{_includedir}/Producer/Types
%{_includedir}/Producer/Utils
%{_includedir}/Producer/Version
%{_includedir}/Producer/VisualChooser
%{_includedir}/Producer/Window3D

