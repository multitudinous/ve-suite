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
#ifndef OCC_NURBS_2_VE_NURBS_H
#define OCC_NURBS_2_VE_NURBS_H
/*!\file OCCNURBS2VENURBS.h
  OCCNURBS2VENURBS API
  */
/*!\class OCCNURBS2VENURBS
 */
namespace NURBS
{
   class NURBSSurface;
}
#include <VE_Installer/include/VEConfig.h>
namespace NURBS_Utilities
{
class VE_NURBS_UTILS_EXPORTS OCCNURBS2VENURBS
{
public:
   ///Constructor
   OCCNURBS2VENURBS();
   ///Destructor
   ~OCCNURBS2VENURBS();

   ///Get a OCC NURBS patch and return an VE NURBS Surface
   ///\param occNURBSSurface NURBS surface to be converted
   NURBS::NURBSSurface* GetVENURBSSurface( Geom_BSplineSurface* occNURBSSurface );
};
}
#endif //OCC_NURBS_2_VE_NURBS_H

