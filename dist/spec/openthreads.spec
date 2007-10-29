BuildRequires: aaa_base acl attr bash binutils bison bzip2 coreutils cpio cpp devs diffutils doxygen e2fsprogs expat file filesystem findutils gawk gcc gcc-c++ glibc glibc-devel glibc-locale grep gzip info klogd libacl libattr libgcc libstdc++ libstdc++-devel m4 make man mktemp module-init-tools patch permissions pkgconfig popt procinfo procps psmisc pwdutils readline sed sysvinit tar timezone unzip util-linux zlib zlib-devel

Name: openthreads
Summary: A light weight, cross-platform threading library
License: LGPL
Group: Real-Time
Version: 1.5.0
Release: 1
Source: %{name}-%{version}.tar.bz2
Packager: Kenneth Mark Bryden 

%description

This library is intended to provide a minimal & complete Object-Oriented (OO)
thread interface for C++ programmers.  It is loosely modeled on the Java thread
API, and the POSIX Threads standards. 

Authors:
--------
Sean Spicer
Jack Lees
Don Burns
Robert Osfield
Boris Bralo

%package devel
Summary: Development headers for OpenThreads
Group: Development/C++
Version: 1.5.0
Release: 1

%description devel
This package contains the development headers necessary for compilation of
programs using OpenThreads.

This library is intended to provide a minimal & complete Object-Oriented (OO)
thread interface for C++ programmers.  It is loosely modeled on the Java thread
API, and the POSIX Threads standards. 

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
cat %{name}.pc.paths openthreads.pc.rpmin > openthreads.pc
cp openthreads.pc %{_prefix}/lib/pkgconfig/openthreads.pc

%post
%{run_ldconfig}

%postun
%{run_ldconfig}

%files
%defattr(755, root, root)
%{_libdir}/libOpenThreads.so

%files devel
%defattr(644, root, root)
%{_prefix}/lib/pkgconfig/openthreads.pc
%{_includedir}/OpenThreads/Barrier
%{_includedir}/OpenThreads/Condition
%{_includedir}/OpenThreads/Exports
%{_includedir}/OpenThreads/Mutex
%{_includedir}/OpenThreads/ScopedLock
%{_includedir}/OpenThreads/Thread

