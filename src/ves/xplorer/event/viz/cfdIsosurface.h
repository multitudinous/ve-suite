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
#ifndef CFD_ISOSURFACE_H
#define CFD_ISOSURFACE_H

#include <ves/xplorer/event/viz/cfdObjects.h>

#ifdef USE_OMP
#define MAX_ISOSURFACE 20
#include <vtkAppendPolyData.h>
#endif

#include <string>

class vtkContourFilter;
class vtkPolyDataNormals;
class vtkPolyDataMapper;

namespace ves
{
namespace xplorer
{
/*!\file cfdIsosurface.h
cfdIsosurface API
*/
/*!\class ves::xplorer::cfdIsosurface
*   A class that takes input data set(s) and generates an isosurface
*   based on the value selected.  Update member function will
*   update the value.
*/
class VE_XPLORER_EXPORTS cfdIsosurface : public cfdObjects
{
public:
    ///Initialize pipeline
    ///(and set the number of isosurface increments for blue menu)
    ///\param step set to 10
    cfdIsosurface( int step = 10 );

    ///Destructor.
    virtual ~cfdIsosurface();

    ///Update the isosurface.
    virtual void Update( void );

    ///Get the current isosurface value.
    double GetValue();

    ///Override the cfdObjects function
    virtual void UpdateCommand( void );

private:
    double convertPercentage( const int percentage );

    int totalId;///<Total number of steps through the isosurface range.

    double value;///<Current value of isosurface.

    std::string colorByScalar;///<Color the isosurface by a different color.
    double minValue;///<Minimum scalar value displayed.
    double maxValue;///<Maximum scalar value displayed.
#ifdef USE_OMP
    float nData;///<Total number of data to be parallel processed.
    vtkContourFilter *contour[MAX_ISOSURFACE];///<Contour filter for vtk.
    vtkPolyDataNormals *normals[MAX_ISOSURFACE];///<Normal for polydata in vtk.
    vtkAppendPolyData *append;///<Append to dataset.
#else
    vtkPolyDataNormals *normals;///<Normals for vtk.
#endif

    vtkPolyDataMapper* mapper;///<Mapper for vtk.
};
}
}
#endif
