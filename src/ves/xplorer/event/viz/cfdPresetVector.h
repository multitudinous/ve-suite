/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_PRESET_VECTOR_H
#define CFD_PRESET_VECTOR_H

#include <ves/xplorer/event/viz/cfdVectorBase.h>


namespace ves
{
namespace xplorer
{
class cfdCuttingPlane;
}
}

namespace ves
{
namespace xplorer
{
/*!\file cfdPresetVector.h
cfdPresetVector API
*/
/*!\class ves::xplorer::cfdPresetVector
* A class that takes input data set(s) and generates a
* cutting plane of vector profile based on the position
* and direction selected. Update member function will update
* the plane position and direction.
*/
class VE_XPLORER_EXPORTS cfdPresetVector : public cfdVectorBase
{
public:
    ///Initialize the VTK objects and pipeline, (and set the number of isosurface increments for blue menu)
    ///\param xyz
    ///\param numSteps
    cfdPresetVector( const int xyz, const int numSteps = 10 );
    ///Destructor
    virtual ~cfdPresetVector();
    ///Update the preset vectors
    virtual void Update( void );

private:
    int xyz;///<value of location
    int numSteps;///<number of steps

    cfdCuttingPlane * cuttingPlane;///<cutting plane
};
}
}
#endif
