/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdAnimatedStreamlineCone.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ANIMATED_STREAMLINE_CONE_H
#define CFD_ANIMATED_STREAMLINE_CONE_H

#include "VE_Xplorer/cfdObjects.h"

class vtkPolyDataMapper;
class vtkPolyData;
class vtkGlyph3D;
class vtkSphereSource;
namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdAnimatedStreamlineCone : public cfdObjects
{
public:
   cfdAnimatedStreamlineCone( void );
   ~cfdAnimatedStreamlineCone();

   //void Initialize();
   //void SetDirection( float [3]);
   //void SetCenter( float [3]);
   //void CleanUpSequence( void );
   void SetPolyDataSource( vtkPolyData * );

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray* _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   virtual void Update( void );

private:
   vtkPolyDataMapper *mapper;
   vtkPolyData *polyData;
   vtkPolyData *polydata;
   vtkGlyph3D *glyph;
   vtkSphereSource *sphere;

   float particleDiameter;
   enum STREAM_DIRECTION
   {
      FORWARD,
      BACKWARD,
      BOTH
   };

   STREAM_DIRECTION streamDir;
   //int nPts, nStr, iPts;
   //float ptData[3], direction[3], center[3];   
};
}
#endif
