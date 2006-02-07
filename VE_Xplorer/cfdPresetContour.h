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
 * File:          $RCSfile: cfdPresetContour.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_PRESET_CONTOUR_H
#define CFD_PRESET_CONTOUR_H

#include "VE_Xplorer/cfdContourBase.h"

class vtkCutter;
class vtkPolyData;

namespace VE_Xplorer
{
   class cfdCuttingPlane;
}
//! VTK contour plane renderer.
/*!
  A class that takes input data set(s) and generates a 
  cutting plane based on the position and direction
  selected. Update member function will update
  the plane position and direction.
*/
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdPresetContour : public cfdContourBase
   {
      public:
         // Initialize the pipeline.
         // (and set the number of cutting plane increments for blue menu)
         cfdPresetContour( const int xyz, const int numSteps = 10 );

         ~cfdPresetContour();

         virtual void Update( void );
         void CreatePlane( void );

      private:
         int xyz;
         int numSteps;
         vtkCutter       * cutter;

         cfdCuttingPlane * cuttingPlane;
         vtkPolyData     * polydata;
   };
}
#endif
