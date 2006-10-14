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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_NURBS_2_OCC_NURBS_H
#define VE_NURBS_2_OCC_NURBS_H
/*!\file VENURBS2OCCNURBS.h
  VENURBS2OCCNURBS API
  */
/*!\class VENURBS2OCCNURBS
 */
namespace NURBS
{
   class NURBSSurface;
}
class Geom_BSplineSurface;
#include <VE_Installer/include/VEConfig.h>
namespace NURBS_Utilities
{
class VE_NURBS_UTILS_EXPORTS VENURBS2OCCNURBS
{
public:
   ///Constructor
   VENURBS2OCCNURBS();
   ///Destructor
   ~VENURBS2OCCNURBS();

   ///Get a VE NURBS patch and return an OCC BSpline Surface
   ///\param veNURBSSurface NURBS surface to be converted
   Geom_BSplineSurface* GetOCCNURBSSurface( NURBS::NURBSSurface* veNURBSSurface );
};
}
#endif //VE_NURBS_2_OCC_NURBS_H

