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
 * File:          $RCSfile: cfdPlanes.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_PLANES_H
#define CFD_PLANES_H

class vtkPolyData;

namespace VE_Xplorer
{
   class cfdCuttingPlane;
}
//! VTK contour plane renderer.
/*!
  A class that reads in precomputed plane data files corresponding to a 
  specific axis direction.  The files are located in a specified directory.
  The plane files were created by the preprocessor acting on a flowdata.vtk.
*/
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdPlanes
   {
      public:
         // Initialize the VTK objects and pipeline.
         // xyz: 0 = x plane cuts, 1 = y plane cuts, and 2 = z plane cuts.
         cfdPlanes( const int xyz, const char directory[], const double bounds[ 6 ] );
         cfdPlanes();

         ~cfdPlanes();

         void SetAllPlanesSelected( void );

         // Get the cut planes polydata
         vtkPolyData * GetPlanesData();

         // 0 <= sliderBarPos <= 100
         vtkPolyData * GetClosestPlane( const int sliderBarPos );

         void ConcatenateSelectedPlanes( void );
  
         int GetNumberOfPlanes();

         vtkPolyData * GetPlane( const int i );

      private:
         int numPlanes;  // Total number of precomputed planes found in the direcory
         int type;       // Direction of cuts. 0=x planes, 1=y planes, 2=z planes.
         char typeLabel; // 'X', 'Y', or 'Z'


         cfdCuttingPlane *cuttingPlane;
         //vtkTriangleFilter *tFilter;
         //vtkDecimatePro *deci;

         // Individual polydata planes of data.
         vtkPolyData ** append;

         // array that keeps track of which planes are selected for display
         int * isPlaneSelected;

         // array that keeps track of the physical location of a particular plane
         float * sliceLocation;

         // polydata planes of data stored in a single object.
         vtkPolyData * collectivePolyData;
   };
}
#endif
