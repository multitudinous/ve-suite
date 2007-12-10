/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef CFD_CONTOUR_H
#define CFD_CONTOUR_H

#include <ves/xplorer/event/viz/cfdContourBase.h>

class vtkPlane;
class vtkCutter;

#ifdef USE_OMP
#define MAX_CONTOUR 20
class vtkAppendPolyData;
#endif

namespace ves
{
namespace xplorer
{
/*!\file cfdContour.h
cfdContour API
*/
/*!\class ves::xplorer::cfdContour
*   A class to takes input data set(s) and generates on
*   cutting planes based on the position and direction
*   selected. Update member function will be update
*   the position and direction as each "Update" being called.
*/
class VE_XPLORER_EXPORTS cfdContour : public cfdContourBase
{
public:
    ///Initialize the VTK objects and pipeline.
    cfdContour( );
    ///Destructor
    virtual ~cfdContour( );

    ///Update the position, x, and normal direction to cut.
    virtual void Update( void );

private:

#ifdef USE_OMP
    vtkPlane *plane[MAX_CONTOUR];///<Contour cutting plane.
    vtkCutter *cutter[MAX_CONTOUR];///<Used for the cutter code.
    vtkAppendPolyData *append;///<Appends to the dataset for number of processors.
    float nData;///<Total number of data to be parallel processed.
#else
    vtkPlane *plane;///<Plane for vtk.
    vtkCutter *cutter;///<Cutter for vtk.
#endif
};
}
}
#endif
