/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SUITE_CONFIG_
#define VE_SUITE_CONFIG_ 1

#define VE_PHYSICS 1

#if defined(_MSC_VER)
   //#pragma warning( disable : 4244 )
   #pragma warning( disable : 4251 )
   //#pragma warning( disable : 4267 )
   //#pragma warning( disable : 4275 )
   //#pragma warning( disable : 4290 )
   //#pragma warning( disable : 4786 )
   //#pragma warning( disable : 4305 )
   //#pragma warning( disable : 4996 )
#endif

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined( __BCPLUSPLUS__) || defined( __MWERKS__)
   #  ifdef VE_SCENEGRAPH_LIBRARY
   #    define VE_SCENEGRAPH_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_SCENEGRAPH_EXPORTS   __declspec(dllimport)
   #  endif /* VE_SCENEGRAPH_LIBRARY */

   #  ifdef VE_UTIL_LIBRARY
   #    define VE_UTIL_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_UTIL_EXPORTS   __declspec(dllimport)
   #  endif /* VE_UTIL_LIBRARY */

   #  ifdef VE_XPLORER_LIBRARY
   #    define VE_XPLORER_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_XPLORER_EXPORTS   __declspec(dllimport)
   #  endif /* VE_XPLORER_LIBRARY */

   #  ifdef VE_CONDUCTOR_LIBRARY
   #    define VE_CONDUCTOR_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_CONDUCTOR_EXPORTS   __declspec(dllimport)
   #  endif /* VE_CONDUCTOR_LIBRARY */

   #  ifdef VE_GUIPLUGINS_LIBRARY
   #    define VE_GUIPLUGINS_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_GUIPLUGINS_EXPORTS   __declspec(dllimport)
   #  endif /* VE_GUIPLUGINS_LIBRARY */

   #  ifdef VE_GRAPHICALPLUGINS_LIBRARY
   #    define VE_GRAPHICALPLUGINS_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_GRAPHICALPLUGINS_EXPORTS   __declspec(dllimport)
   #  endif /* VE_GRAPHICALPLUGINS_LIBRARY */

   #  ifdef VE_TEXTURE_BASED_LIBRARY
   #    define VE_TEXTURE_BASED_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_TEXTURE_BASED_EXPORTS   __declspec(dllimport)
   #  endif /* VE_TEXTURE_BASED_LIBRARY */

   #  ifdef VE_USER_PLUGIN_LIBRARY
   #    define VE_USER_PLUGIN_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_USER_PLUGIN_EXPORTS   __declspec(dllimport)
   #  endif /* VE_USER_PLUGIN_LIBRARY */

   #  ifdef VE_BUILDER_LIBRARY
   #    define VE_BUILDER_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_BUILDER_EXPORTS   __declspec(dllimport)
   #  endif /* VE_BUILDER_LIBRARY */

   #  ifdef VE_USER_BUILDER_LIBRARY
   #    define VE_USER_BUILDER_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_USER_BUILDER_EXPORTS   __declspec(dllimport)
   #  endif /* VE_USER_BUILDER_LIBRARY */

   #  ifdef VE_XML_LIBRARY
   #    define VE_XML_EXPORTS   __declspec(dllexport)
   #    define VE_CAD_EXPORTS   __declspec(dllexport)
   #    define VE_SHADER_EXPORTS   __declspec(dllexport)
   #    define VE_MODEL_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_XML_EXPORTS   __declspec(dllimport)
   #    define VE_CAD_EXPORTS   __declspec(dllimport)
   #    define VE_SHADER_EXPORTS   __declspec(dllimport)
   #    define VE_MODEL_EXPORTS   __declspec(dllimport)
   #  endif /* VE_XML_LIBRARY */


   #  ifdef VE_CONDUCTOR_UTILS_LIBRARY
   #    define VE_CONDUCTOR_UTILS_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_CONDUCTOR_UTILS_EXPORTS   __declspec(dllimport)
   #  endif /* VE_CONDUCTOR_UTILS_LIBRARY */

   #  ifdef VE_SCENEGRAPH_UTILS_LIBRARY
   #    define VE_SCENEGRAPH_UTILS_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_SCENEGRAPH_UTILS_EXPORTS   __declspec(dllimport)
   #  endif /* VE_SCENEGRAPH_UTILS_LIBRARY */

   #  ifdef VE_CE_UTILS_LIBRARY
   #    define VE_CE_UTILS_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_CE_UTILS_EXPORTS   __declspec(dllimport)
   #  endif /* VE_SCENEGRAPH_UTILS_LIBRARY */

   #  ifdef VE_CE_UNIT_WRAPPER_LIBRARY
   #    define VE_CE_UNIT_WRAPPER_EXPORTS   __declspec(dllexport)
   #  else
   #    define VE_CE_UNIT_WRAPPER_EXPORTS   __declspec(dllimport)
   #  endif /* VE_SCENEGRAPH_UTILS_LIBRARY */

   #  ifdef VE_OPEN_MODULE_LIBRARY
   #    define VE_OPEN_MODULE_EXPORTS __declspec(dllexport)
   #  else
   #    define VE_OPEN_MODULE_EXPORTS __declspec(dllimport)
   #  endif /* VE_OPEN_MODULE_LIBRARY */

   #  ifdef XPLORER_NETWORK_LIBRARY
   #    define VE_XPLORER_NETWORK_EXPORTS __declspec(dllexport)
   #  else
   #    define VE_XPLORER_NETWORK_EXPORTS __declspec(dllimport)
   #  endif /* XPLORER_NETWORK_LIBRARY */

   #  ifdef VE_NURBS_LIBRARY
   #    define VE_NURBS_EXPORTS __declspec(dllexport)
   #  else
   #    define VE_NURBS_EXPORTS __declspec(dllimport)
   #  endif /* VE_NURBS_LIBRARY */
   
   #  ifdef VE_NURBS_UTILS_LIBRARY
   #    define VE_NURBS_UTILS_EXPORTS __declspec(dllexport)
   #  else
   #    define VE_NURBS_UTILS_EXPORTS __declspec(dllimport)
   #  endif /* VE_NURBS_UTILS_LIBRARY */
#else
   #  define VE_SCENEGRAPH_EXPORTS
   #  define VE_UTIL_EXPORTS
   #  define VE_XPLORER_EXPORTS
   #  define VE_CONDUCTOR_EXPORTS
   #  define VE_GUIPLUGINS_EXPORTS
   #  define VE_GRAPHICALPLUGINS_EXPORTS
   #  define VE_TEXTURE_BASED_EXPORTS
   #  define VE_USER_PLUGIN_EXPORTS
   #  define VE_BUILDER_EXPORTS
   #  define VE_USER_BUILDER_EXPORTS
   #  define VE_XML_EXPORTS
   #  define VE_CAD_EXPORTS
   #  define VE_SHADER_EXPORTS
   #  define VE_MODEL_EXPORTS
   #  define VE_CONDUCTOR_UTILS_EXPORTS
   #  define VE_SCENEGRAPH_UTILS_EXPORTS
   #  define VE_CE_UTILS_EXPORTS
   #  define VE_CE_UNIT_WRAPPER_EXPORTS
   #  define VE_OPEN_MODULE_EXPORTS
   #  define VE_XPLORER_NETWORK_EXPORTS
   #  define VE_NURBS_EXPORTS
   #  define VE_NURBS_UTILS_EXPORTS
#endif

#endif
