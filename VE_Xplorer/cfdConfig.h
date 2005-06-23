/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdVEBaseClass.h,v $
 * Date modified: $Date: 2005-05-25 23:32:59 -0500 (Wed, 25 May 2005) $
 * Version:       $Rev: 2321 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_CONFIG_H
#define CFD_CONFIG_H

#include <wx/dlimpexp.h>

#if defined(VEUSINGDLL) && \
    (defined(VEMAKING_PLUGIN_DLL) || defined(VEUSING_PLUGIN_DLL))

#  if defined(VEMAKING_PLUGIN_DLL)
      // When building the DLL WXPLUGINDECLSPEC exports classes
#     define VEPLUGIN_DECLSPEC            WXEXPORT
#  elif defined(VEUSING_PLUGIN_DLL)
      // When building the DLL WXPLUGINDECLSPEC imports classes
#     define VEPLUGIN_DECLSPEC            WXIMPORT
#  endif // defined(WXBUILD_PLUGIN_DLL)
#else
   // When building the static library nullify the effect of WXPLUGIN_DECLSPEC
#  define VEPLUGIN_DECLSPEC
#endif // WXUSINGDLL && (WXMAKING_PLUGIN_DLL || WXUSING_PLUGIN_DLL)

#endif
