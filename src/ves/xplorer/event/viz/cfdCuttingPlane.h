/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef CFD_CUTTING_PLANE
#define CFD_CUTTING_PLANE

#include <ves/VEConfig.h>
class vtkPlane;
class vtkDataSet;

namespace ves
{
namespace xplorer
{
/*!\file cfdCuttingPlane.h
cfdCuttingPlane API
*/
/*!\class ves::xplorer::cfdCuttingPlane
*
*/
class VE_XPLORER_EXPORTS cfdCuttingPlane
{
public:
    ///Constructor
    cfdCuttingPlane( const double bounds[6], const int xyz,
                     const int numSteps = 10 );
    ///Destructor
    ~cfdCuttingPlane( );

    void SetBounds( const double bounds[6] );

    vtkPlane * GetPlane( );

    ///This reads in the requested value for the cutting plane.
    ///\param requestedValue
    void Advance( int requestedValue );

    ///This reads in the requested value for the cutting plane.
    ///\param Origin
    void GetOrigin( double Origin[ 3 ] );

private:
    double origin[3];    // Position of cut.

    ///This computes the origin as requested from the user.
    ///\param requestedValue
    void ComputeOrigin( int requestedValue );

    ///Used to test if past range.
    int isPastEnd();

    ///Used to test if at end of range.
    int isAtEnd();

    ///Used to test if at start of range.
    int isAtStart();

    ///Used to reset the origin to low value.
    void ResetOriginToLow();

    ///Used to reset the origin to high value.
    void ResetOriginToHigh();

    ///Used to advance the origin.
    void IncrementOrigin();

    vtkPlane * plane;///<Plane for vtk.

    double normal[3];///<Normal direction to cut.

    double bd[6];///<Boundary of the whole data sets.

    float dx;///<Used only by blue menu.

    int type;///<Plane direction: 0=X, 1=Y, 2=Z.
};
}
}
#endif
