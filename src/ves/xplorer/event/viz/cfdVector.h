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
#ifndef CFD_VECTOR_H
#define CFD_VECTOR_H
/*!\file cfdVector.h
cfdVector API
*/
/*!\class VE_Xplorer::cfdVector
* A class to takes input data set(s) and generates a 
* cutting planes of vector profile based on the position 
* and direction selected. Update member function will update
* the position and direction as each "Update" being called.
*/

#ifdef USE_OMP
class vtkAppendFilter;
#define MAX_VECTOR 20
#endif

#include <ves/xplorer/event/viz/cfdVectorBase.h>

class vtkPlane;
class vtkCutter;
class vtkGlyph3D;
class vtkMaskPoints;

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdVector : public cfdVectorBase
   {
      public:
         // Initialize the VTK objects and pipeline.
		  ///Constructor
         cfdVector( );
		 ///Destructor
         virtual ~cfdVector( );
  
         /* Update the position, x, and normal direction to cut.
         Output a updated pfGeoSet.  */
         virtual void Update( void );

      private:

      #ifdef USE_OMP
         vtkPlane *plane[MAX_VECTOR];
         vtkCutter *cutter[MAX_VECTOR];
         vtkGlyph3D *glyph[MAX_VECTOR];
         vtkMaskPoints   *ptmask[MAX_VECTOR];
         vtkAppendFilter *append;
         float nData;
      #else
         vtkPlane           *plane;
         vtkCutter          *cutter;
      #endif
   };
}
#endif
