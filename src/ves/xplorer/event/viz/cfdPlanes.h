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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_PLANES_H
#define CFD_PLANES_H

#include <ves/VEConfig.h>

class vtkPolyData;

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
/*!\file cfdPlanes.h
cfdPlanes API
*/
/*!\class ves::xplorer::cfdPlanes
*   A class that reads in precomputed plane data files corresponding to a 
*   specific axis direction.  The files are located in a specified directory.
*   The plane files were created by the preprocessor acting on a flowdata.vtk.
*/
class VE_XPLORER_EXPORTS cfdPlanes
{
public:
   ///Initialize the VTK objects and pipeline.
   ///\param xyz 0 = x plane cuts, 1 = y plane cuts, and 2 = z plane cuts.
   ///\param directory Location of where to store planes.
   ///\param bounds Boundary of cutting planes.
   cfdPlanes( const int xyz, const char directory[], const double bounds[ 6 ] );
   ///Constructor
   cfdPlanes();
   ///Destructor
   ~cfdPlanes();

   ///Set all planes to be selected concatenate them all into one
   void SetAllPlanesSelected( void );

   // Get the cut planes polydata
   vtkPolyData * GetPlanesData();

   ///Selects the closest cutting plane depending on the slider bar position.
   ///0 <= sliderBarPos <= 100
   ///\param sliderBarPos The position of the slider bar.
   vtkPolyData * GetClosestPlane( const int sliderBarPos );

   ///Concatenate them all into one.
   void ConcatenateSelectedPlanes( void );

   ///Get the number of planes.
   int GetNumberOfPlanes();
   
   ///Get a particular plane.
   ///\param i
   vtkPolyData * GetPlane( const int i );

private:
   int numPlanes;///<Total number of precomputed planes found in the directory.
   int type;///<Direction of cuts. 0=x planes, 1=y planes, 2=z planes.
   char typeLabel;///<'X', 'Y', or 'Z'.


   cfdCuttingPlane *cuttingPlane;///<Cutting plane for data.
   //vtkTriangleFilter *tFilter;
   //vtkDecimatePro *deci;
   
   vtkPolyData ** append;///<Individual polydata planes of data.
   
   int * isPlaneSelected;///<array that keeps track of which planes are selected for display.
   
   float * sliceLocation;///<array that keeps track of the physical location of a particular plane.

  
   vtkPolyData * collectivePolyData;///<polydata planes of data stored in a single object.
};
}
}
#endif
