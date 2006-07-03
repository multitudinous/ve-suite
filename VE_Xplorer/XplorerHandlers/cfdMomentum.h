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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_MOMENTUM_H
#define CFD_MOMENTUM_H

#ifdef USE_OMP
#include <vtkAppendPolyData.h>
#define MAX_MOMENTUM 20
#endif

#include "VE_Xplorer/XplorerHandlers/cfdContourBase.h"

class vtkPlane;
class vtkCutter;
class vtkWarpVector;

//! VTK momentum plane renderer.
/*!
  A class to takes input data set(s) and generates a 
  cutting planes of momentum profile based on the position 
  and direction selected. Update member function will be update
  the position and direction as each "Update" being called.
*/
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdMomentum : public cfdContourBase
   {
      public:
         // Initialize the VTK objects and pipeline.
         cfdMomentum( void );

         ~cfdMomentum( void );

         /* 
         Update the position, x, and normal direction to cut.
         Output a updated pfGeoSet.  
         */
         virtual void Update( void );

      private:  
      #ifdef USE_OMP
         vtkPlane *plane[MAX_MOMENTUM];
         vtkCutter *cutter[MAX_MOMENTUM];
         vtkAppendPolyData *append;
         float nData;
      #else
         vtkPlane *plane;
         vtkCutter *cutter;
      #endif
         vtkWarpVector *warper;
   };
}
#endif
