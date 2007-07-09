# Copyright (C) 2005, Giovanni Bajo
# Based on previous work under copyright (c) 2001, 2002 McMillan Enterprises, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

hiddenimports = []

# FIXME: for a strange bug in Python's import machinery, we need to adjust
# the module name before proceeding to the PIL import. The name would
# otherwise be "hooks.hook-PIL.Image", which will then produce this
# monstruosity:
#   <module 'hooks.hook-PIL.PIL.Image' from 'C:\python24\lib\site-packages\PIL\Image.pyc'>
#
__name__ = "hook-image"

def install_Image(lis):
    import Image
    # PIL uses lazy initialization.
    # you candecide if you want only the
    # default stuff:
    Image.preinit()
    # or just everything:
    Image.init()
    import sys
    for name in sys.modules:
        if name[-11:] == "ImagePlugin":
            lis.append(name)

install_Image(hiddenimports)
