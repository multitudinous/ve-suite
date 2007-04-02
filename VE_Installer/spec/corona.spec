BuildRequires: aaa_base acl attr autoconf automake bash binutils bison bzip2 coreutils cpio cpp devs diffutils e2fsprogs file filesystem findutils gawk gcc gcc-c++ glibc glibc-devel glibc-locale grep gzip info klogd libacl libattr libgcc libstdc++ libstdc++-devel libjpeg libjpeg-devel libpng libpng-devel m4 make man mktemp module-init-tools patch permissions popt procinfo procps psmisc pwdutils readline rpm sed sysvinit tar texinfo timezone unzip util-linux zlib zlib-devel

Summary:	Corona Image Library
Name:	   corona	
Version:	1.0.2
Release: 1
Source0: %{name}-%{version}.tar.bz2
URL:		http://corona.sourceforge.net
License: zlib	
Group:   Libraries
Distribution: SuSE Linux Enterprise 10 (%{_arch})

%description
Corona is an image input/output library that can read, write, and manipulate
image files in just a few lines of code. It can write PNG and TGA files, and
read PNG, JPEG, PCX, BMP, TGA, and GIF. Corona was designed to be easy to use,
and exports a straightforward C++ API. With just a few lines of C++, you can
add image loading to your application.  

Authors: 
-------- 
   Chad Austin <aegis@aegisknight.org>

%package devel
Requires:	corona = %{version}
Group:		Development/Libraries
Summary:	Header files for Corona Image Library 

%description devel
Header files necessary to compile applications using corona. 

Corona is an image input/output library that can read, write, and manipulate
image files in just a few lines of code. It can write PNG and TGA files, and
read PNG, JPEG, PCX, BMP, TGA, and GIF. Corona was designed to be easy to use,
and exports a straightforward C++ API. With just a few lines of C++, you can
add image loading to your application.  

%prep
%setup -q

%build
./configure --prefix=%{_prefix}
make

%install
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%post
%run_ldconfig

%postun
%run_ldconfig

%files
%defattr(755,root,root)
%{_bindir}/corconvert
%{_libdir}/libcorona.a
%{_libdir}/libcorona.la
%{_libdir}/libcorona.so
%{_libdir}/libcorona-1.0.2.so

%files devel
%defattr(-,root,root)
%{_bindir}/corona-config
%{_includedir}/corona.h

%changelog
* Fri Dec 27 2006 Johnathan Gurley <johnathan.gurley@avant-gardeit.com>
- Initial creation.
