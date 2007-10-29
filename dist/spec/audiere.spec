BuildRequires: aaa_base acl attr bash bison bzip2 coreutils cpio cpp devs diffutils e2fsprogs file filesystem findutils flac flac-devel gawk gcc gcc-c++ glibc glibc-devel glibc-locale grep gzip info klogd libacl libattr libgcc libogg libogg-devel libstdc++ libstdc++-devel m4 make man mktemp module-init-tools patch permissions popt procinfo procps psmisc pwdutils readline sed sysvinit tar texinfo timezone unzip util-linux zlib zlib-devel autoconf automake binutils doxygen gcc gettext rpm

# threads
# values: pthreads, none
%define threads pthreads

Summary:	Audiere Audio System
Name:	   audiere	
Version:	1.9.4
Release: 1
Source0: %{name}-%{version}.tar.bz2
URL:		http://audiere.sourceforge.net
License: LGPL	
Group:		Libraries
Prefix:		%{_prefix}

%description
Audiere is a portable audio library which supports playing MP3, Ogg Vorbis,
FLAC, WAV, IT, XM, S3M, and MOD files. You can use it from C, C++, Python,
Java, Delphi, and any language that supports XPCOM (JavaScript in Mozilla, for
example).

%package devel
Requires:	audiere = %{version}
Group:		Development/Libraries
Summary:	Header files for Audiere Sound System Development 

%description devel
Header files you can use to develop sound applications with Audiere. 

Audiere is a portable audio library which supports playing MP3, Ogg Vorbis,
FLAC, WAV, IT, XM, S3M, and MOD files. You can use it from C, C++, Python,
Java, Delphi, and any language that supports XPCOM (JavaScript in Mozilla, for
example).

%prep
%setup -q

%build
./configure --prefix=%{_prefix}
make

%install
make PREFIX=$RPM_BUILD_ROOT%{_prefix} install
rm -f $RPM_BUILD_ROOT%{_libdir}/libaudiere.a
%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(755,root,root)
%{_libdir}/libaudiere.la
%{_libdir}/libaudiere.so
%{_libdir}/libaudiere-1.9.4.so

%files devel
%defattr(-,root,root)
%{_prefix}/bin/audiere-config
%{_prefix}/include/audiere.h

%changelog
* Fri Dec 27 2006 Johnathan Gurley <johnathan.gurley@avant-gardeit.com>
- Initial creation.
