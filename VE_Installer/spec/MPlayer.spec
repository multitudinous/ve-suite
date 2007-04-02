# norootforbuild
# Packmangroup: Multimedia
# Packmanpackagename: MPlayer
# Packman: Henne Vogelsang
# Packmandepends: lame xvid mad x264

%define suseversion  %(echo "SUSE Linux `echo -e "scale=1\\n%{suse_version}/100" | bc` (%_build_cpu)" || Unknown)

Name:		MPlayer
License:	GPL
Group:		Productivity/Multimedia/Video/Players
Version:	1.0pre8
Release:	8.pm.svn20060811
Summary:	Multimedia Player
Summary(de):	Multimedia-Abspielprogramm

Packager:	%packager
Vendor:		%vendor
Distribution:   SUSE Linux Enterprise 10 (i586)

Source0:	http://www1.mplayerhq.hu/MPlayer/releases/MPlayer-1.0pre8.tar.bz2
Source1:	http://www1.mplayerhq.hu/MPlayer/releases/fonts/font-arial-iso-8859-1.tar.bz2
Source2:	http://www1.mplayerhq.hu/MPlayer/Skin/Blue-1.5.tar.bz2
Source3:	mplayer.conf
Source4:	mencvcd
Source5:	%name.desktop
Source6:	%name.png

Patch3:		MPlayer-decl.patch
Patch8:		MPlayer-comparison.patch
Patch13:	MPlayer-1.0pre7-depends.patch
Patch14:	MPlayer-1.0pre7-lzo2.patch
Patch18:	MPlayer-pci_linux_gnu_source.patch

URL:		http://www.mplayerhq.hu 
BuildRoot:	%{_tmppath}/%{name}-%{version}-build
Prefix:		%{_prefix}

%ifarch %ix86 
Requires:      	w32codec-all
BuildRequires:  w32codec-all RealPlayer live
%endif

BuildRequires:  lame toolame libtheora-devel libsmbclient-devel SDL-devel ladspa aalib-devel esound-devel gtk2-devel
BuildRequires:  glib2-devel mad-devel dvb lzo-devel libdca libpng-devel libogg-devel libvorbis-devel libdv
BuildRequires:  DirectFB arts-devel lirc bc cdparanoia xvid-devel lzo lzo-devel fribidi-devel speex-devel
BuildRequires:  faac libmpeg2-devel
BuildRequires:  x264-devel >= 0.0svn20060728

# suse since 9.2 include giflib and libjpeg is splitted
%if %suse_version >= 920
BuildRequires:  giflib giflib-devel libjpeg-devel
%else
BuildRequires:  libungif libjpeg pkgconfig
%endif
# 10.1 stuff:
# suse since 10.1 builds libsmbclient against kerberos.
# We need devs because the v4l configure checks look for v4l devices.
# libdv got split
# caca got added  
%if %suse_version > 1000
BuildRequires:  krb5-devel openldap2-devel libgssapi devs libdv-devel libcaca-devel
%endif

%description
MPlayer plays most MPEG/VOB, AVI, Ogg/OGM, VIVO, ASF/WMA/WMV, QT/MOV/MP4,
RealMedia, Matroska, NUT, NuppelVideo, FLI, YUV4MPEG, FILM, RoQ, PVA files,
supported by many native, XAnim, and Win32 DLL codecs. You can watch VideoCD,
SVCD, DVD, 3ivx, DivX 3/4/5 and even WMV movies..

%description -l de
MPlayer spielt die meisten MPEG/VOB, AVI, Ogg/OGM, VIVO, ASF/WMA/WMV,
QT/MOV/MP4, RealMedia, Matroska, NUT, NuppelVideo, FLI, YUV4MPEG, FILM, RoQ,
PVA-Dateien, unterstuetzt von vielen nativen, XAnim, und Win32 DLL-Codecs ab.
Mit MPlayer koennen sie VideoCD, SVCD, DVD, 3ivx, DivX 3/4/5 und sogar WMV-Filme
anschauen.


%prep
%setup -q -n mplayer
%patch3
%patch8 -p1
%if %suse_version > 1000
%patch13
%endif
%if %suse_version >= 1000
%patch14 -p1
%endif
%patch18
mv mmx.h mmx.h.orig
cp libavcodec/i386/mmx.h .

%build

tar xjf %{SOURCE1}
tar xjf %{SOURCE2}

awk '{gsub ("%{version}-","%{version}-%{suseversion}-Packman-"); print $0}' version.sh > version.sh~
cp version.sh~ version.sh

RPM_OPT_FLAGS="$RPM_OPT_FLAGS -fno-strict-aliasing"
sed "s%%-O4%%$RPM_OPT_FLAGS -O4%%g" configure >configure~
cat configure~ > configure

./configure --prefix=%{_prefix} \
 --confdir=%{_sysconfdir}/mplayer \
 --datadir=%{_datadir}/mplayer \
 --libdir=%{_libdir} \
 --mandir=%{_mandir} \
%ifarch %ix86 
 --enable-runtime-cpudetection \
 --enable-qtx \
%endif
 --enable-bl --enable-fbdev --enable-zr \
 --enable-gui --enable-menu --language=all \
 --enable-largefiles --enable-smb --enable-joystick

%{__make} %{?jobs:-j%{jobs}}

%install
make MANDIR=$RPM_BUILD_ROOT%{_mandir} DESTDIR=$RPM_BUILD_ROOT install
# font
install -m 755 -d $RPM_BUILD_ROOT%{_datadir}/mplayer/fonts
cp -r font-arial-iso-8859-1/* $RPM_BUILD_ROOT%{_datadir}/mplayer/fonts/
# skin
install -m 755 -d $RPM_BUILD_ROOT%{_datadir}/mplayer/Skin/default
cp -r Blue/* $RPM_BUILD_ROOT%{_datadir}/mplayer/Skin/default/
# configs
install -m 644 %{SOURCE3} $RPM_BUILD_ROOT%{_sysconfdir}/mplayer/
install -m 644 etc/input.conf $RPM_BUILD_ROOT%{_sysconfdir}/mplayer/
install -m 644 etc/menu.conf $RPM_BUILD_ROOT%{_sysconfdir}/mplayer/
# mencvcd 
install -m 755 %{SOURCE4} $RPM_BUILD_ROOT%{_bindir}
rm -f TOOLS/mencvcd
# desktop file
install -m 755 -d $RPM_BUILD_ROOT%{_datadir}/applications
install -m 644 %{SOURCE5} $RPM_BUILD_ROOT%{_datadir}/applications/
rm $RPM_BUILD_ROOT%{_datadir}/applications/mplayer.desktop
# mplayer.png
%__install -D -m 644 %{SOURCE6} $RPM_BUILD_ROOT%{_datadir}/pixmaps/MPlayer.png

cd $RPM_BUILD_ROOT%{_datadir}/mplayer/
rmdir font
ln -s fonts/font-arial-14-iso-8859-1 font

%clean
rm -rf $RPM_BUILD_ROOT 

%files
%defattr(-, root, root)
%doc DOCS TOOLS README AUTHORS ChangeLog
%dir %{_sysconfdir}/mplayer
%config(noreplace) %verify(not size mtime md5)  %{_sysconfdir}/mplayer/mplayer.conf
%config(noreplace) %verify(not size mtime md5)  %{_sysconfdir}/mplayer/input.conf
%config(noreplace) %verify(not size mtime md5)  %{_sysconfdir}/mplayer/menu.conf
%_mandir/man1/mplayer.1.gz
%_mandir/man1/mencoder.1.gz
%_mandir/cs/man1/mplayer.1.gz
%_mandir/cs/man1/mencoder.1.gz
%_mandir/de/man1/mplayer.1.gz
%_mandir/de/man1/mencoder.1.gz
%_mandir/es/man1/mplayer.1.gz
%_mandir/es/man1/mencoder.1.gz
%_mandir/fr/man1/mplayer.1.gz
%_mandir/fr/man1/mencoder.1.gz
%_mandir/hu/man1/mplayer.1.gz
%_mandir/hu/man1/mencoder.1.gz
%_mandir/it/man1/mplayer.1.gz
%_mandir/it/man1/mencoder.1.gz
%_mandir/pl/man1/mplayer.1.gz
%_mandir/pl/man1/mencoder.1.gz
%_mandir/sv/man1/mplayer.1.gz
%_mandir/sv/man1/mencoder.1.gz

%{_bindir}/*
%dir %{_datadir}/mplayer
%dir %{_datadir}/mplayer/Skin
%{_datadir}/mplayer/Skin/default
%{_datadir}/mplayer/fonts
%{_datadir}/mplayer/font

%{_datadir}/applications/*
%{_datadir}/pixmaps/*

%_libdir/libdha.so*
%_libdir/mplayer/

%changelog
* Sat Aug 12 2006 - Detlef Reichelt <detlef@links2linux.de>
- fix MPlayer desktop
* Fri Aug 11 2006 - Detlef Reichelt <detlef@links2linux.de>
- build svn -> wmv9-support for x86_64 :)
* Sat Jul 29 2006 - Leon Freitag <leon@links2linux.de>
- build against a new x264 release (r48)
* Wed Jun 14 2006 - Henne Vogelsang <henne@links2linux.de>
- build against gtk2 not gtk1
* Wed Jun 14 2006 - Henne Vogelsang <henne@links2linux.de>
- Update to version 1.0pre8
- build with fribidi and speex support
- build with caca support for 10.1
- get rid of -fno-unit-at-a-time in the optflags it just causes problems
- build with internal vidix drivers
- Get rid of the patches because they are upstream
  - MPlayer-1.0pre7-gcc4.patch
  - MPlayer-1.0pre7-gcc4-x86_64.patch
  - MPlayer-1.0pre7-pcm-overflow.patch
  - MPlayer-1.0pre7-tivo.patch
  - MPlayer-1.0pre7try3.patch
  - x264.diff
- Update patches match pre8
  - MPlayer-1.0pre8.patch
  - asm-fixes.diff
  - MPlayer-1.0pre7-gcc4-altivec.patch
* Wed Apr 26 2006 - Leon Freitag <leon@links2linux.de>
- bumped the release because of x264 update
- fixed some typos
* Tue Mar 28 2006 - Henne Vogelsang <henne@links2linux.de>
- Quite a lot of x86_64 fixes
- use runtime cpu detection only on x86
- some buildrequires fixes
* Wed Mar 22 2006 - Henne Vogelsang <henne@links2linux.de>
- disable vidix
- get rid of the xvmc unichrome patch. i dont build xvmc
- hopefully fix the try3 patch
- RealPlayer is only on ix86 as buildrequire
- add gcc4 altivec patch for ppc 
* Sun Mar 19 2006 Leon Freitag <leon@links2linux.de>
- Enabled x264 encoding support through x264 package
- Fixed some typos in the spec file
* Thu Mar 02 2006 Henne Vogelsang <henne@links2linux.de>
- apply buffer overflow fix to libmpdemux/demuxer.h
- bump version to 1.0pre7try3
- i dont need a pm in Release anymore because the last
  suse version with mplayer got dropped recently
* Tue Feb 28 2006 Henne Vogelsang <henne@links2linux.de>
- implement suseversion in the spec
- 10.1 needs lzo2 patches
- fix comparsion patch
- fix libsmbclient deps for 10.1
- reenable xvid because too much people complained
- package missing man pages 
* Fri Aug 26 2005 Henne Vogelsang <henne@links2linux.de>
- more fixes needed to make it compile with gcc 4.0.2
- add security patch for the buffer overflow reported on
  full disclosure
- add patch to make playback of tivo streams working again
* Tue May 24 2005 Henne Vogelsang <henne@links2linux.de>
- add lirc again
* Wed Apr 20 2005 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre7
- change mplayer -v output to include distro, provider
- make package ready for gcc4
* Sat Apr 09 2005 Henne Vogelsang <henne@links2linux.de>
- actually its now called libdca not libdts
* Fri Apr 01 2005 Henne Vogelsang <henne@links2linux.de>
- build against libdts
* Fri Jan 07 2005 Henne Vogelsang <henne@links2linux.de>
- add demuxer patch to avoid a crash while seeking 
  in MPEG ES/PS files
- update our desktop file to match the offical one
- remove the official mplayer desktop file
- pass target right to x86_64
- lowercase conf and data directory
* Thu Jan 06 2005 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre6a
* Fri Dec 24 2004 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre6
- get rid of all patches most of them are upstream
- dont package codecs.conf anymore
* Mon Oct 18 2004 Henne Vogelsang <henne@links2linux.de>
- disabled live. does not compile on 9.2 and nobody seems
  to use it anyway.
- added some patches for 9.2
* Thu Jul 15 2004 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre5
- change default skin to Blue
- add patch for dynamic arrays in older ditributions
* Thu Jun 10 2004 Henne Vogelsang <henne@links2linux.de>
- incorporated manfreds ppc changes and translations
- disabled xvid and divx4linux usage. libavcodec can do that.
  Kudos to Diego Biurrun for pointing that out.
* Sun Jun 06 2004 Henne Vogelsang <henne@links2linux.de>
- beautify .desktop file
* Wed Mar 31 2004 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre4
- move live libs to own package 
- install desktop file
* Wed Mar 31 2004 Henne Vogelsang <henne@links2linux.de>
- add patch to fix remote buffer overflow
* Sun Jan 18 2004 Henne Vogelsang <henne@links2linux.de>
- update to version 1.0pre3
- reenable divx
- build all languages
- use pm in release
* Sat Dec 06 2003 Henne Vogelsang <henne@links2linux.de>
- build against the new xvid
* Wed Oct 15 2003 Henne Vogelsang <henne@links2linux.de>
- fixed mp3lib problem with fno-strict-aliasing
- updated fonts
- cleaned patch namings
* Mon Oct 13 2003 Henne Vogelsang <henne@links2linux.de>
- updated live libs
* Sun Sep 28 2003 Henne Vogelsang <henne@links2linux.de>
- Updated to version 0.92
* Sun Aug 31 2003 Henne Vogelsang <henne@links2linux.de>
- make use of --target
- removed libdvdcss2 dep
- reintroduced cpu runtime protection (to many bugreports)
* Wed Aug 13 2003 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.91
* Sat Aug 09 2003 Henne Vogelsang <henne@links2linux.de>
- patched version
- skin subpackage moved to a seperate package
- removed dvdnav support (broken and not used anyway)
- updated mencvcd and moved to bindir
- added dvd2divxscript.pl to bindir
* Mon May 19 2003 Henne Vogelsang <henne@links2linux.de>
- disable cpu runtime protection
* Sat Apr 12 2003 Henne Vogelsang <henne@links2linux.de>
- changed the skin.files call to the BEST possible hack
* Sat Apr 12 2003 Henne Vogelsang <henne@links2linux.de>
- cleaned Requires
- skin files list was borked. readded skin.files
- added 82 if around the dvdnav diff
* Wed Apr 09 2003 Henne Vogelsang <henne@links2linux.de>
- updated to final version of 0.90
- updated skins and made it easier to package them
- updated live to 2003.04.09 src
- enabled faad2
- enabled smb support
* Sun Mar 23 2003 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90rc5
- enabled runtime cpu detection
- removed --target and BuildArch
- removed march/mcpu from live diff
- added $RPM_OPT_FLAGS to live diff
* Mon Feb 10 2003 Henne Vogelsang <henne@links2linux.de>
- updated version to 0.90rc4
- updated live.com libs to 2003.02.10
* Tue Feb 04 2003 Henne Vogelsang <henne@links2linux.de>
- removed libGLcore.so.1 dependencie
- enabled blinkenlights
* Sun Jan 26 2003 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90rc3
- enabled quicktime codecs 
* Mon Jan 13 2003 Henne Vogelsang <henne@links2linux.de>
- removed libpostproc subpackage again. created an own package
- added -Wno-deprecated to live Makefile
* Sun Jan 12 2003 Henne Vogelsang <henne@links2linux.de>
- fixed man page bug (kudos to Bernd Leibing)
- added live.com libs (kudos to Tuukka Pasanen)
- added skins subpackage
- added libpostproc subpackage for transcode
- a little bit spec work
* Wed Dec 25 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90rc2
- beautified spec
* Sat Oct 12 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90pre8
* Sun Sep 15 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90pre7
- removed 2 patches
* Tue Aug 13 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90pre6
- reworked spec file completely 
- added 2 patches
* Sun May 25 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90pre4
* Sun May 05 2002 Henne Vogelsang <henne@links2linux.de>
- updated to version 0.90pre3
- removed patch
- enabled GUI
* Sat Oct 27 2001 Waldemar Brodkorb <waldemar@links2linux.de>
- first build
