/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef CFD_PRESET_MOMENTUM_H
#define CFD_PRESET_MOMENTUM_H

#include <ves/xplorer/event/viz/cfdContourBase.h>

namespace ves
{
namespace xplorer
{
class cfdCuttingPlane;
}
}

class vtkCutter;
class vtkWarpVector;


namespace ves
{
namespace xplorer
{
/*!\file cfdPresetMomentum.h
cfdPresetMomentum API
*/
/*!\class ves::xplorer::cfdPresetMomentum
*  A class that takes input data set(s) and generates a
*  cutting plane of momentum profile based on the position
*  and direction selected. Update member function will update
*  the plane position and direction.
*/
class VE_XPLORER_EXPORTS cfdPresetMomentum : public cfdContourBase
{
public:
    ///Initialize the pipeline.
    ///(and set the number of cutting plane increments for blue menu)
    ///\param xyz
    ///\param numSteps
    cfdPresetMomentum( const int xyz, int numSteps = 10 );
    ///Destructor
    virtual ~cfdPresetMomentum( void );

    ///Update the position, x, and normal direction to cut. Output a updated pfGeoSet.
    virtual void Update( void );
//         void CreatePlane( void );

private:
    vtkWarpVector   * warper;///<warper.
};
}
}
#endif
