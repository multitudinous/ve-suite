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
 * File:          $RCSfile: cfdStreamers.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_STREAMERS_H
#define CFD_STREAMERS_H

class vtkStreamLine;
class vtkTubeFilter;
class vtkPolyDataMapper;
class vtkPolyData;
class vtkRungeKutta45;

#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"

namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_XML
{
   class Command;
}

//! VTK streamers plane renderer.
/*!
  A class to takes input data set(s) and generates streamlines 
  based on the active glyph. Update member function will update
  the position and direction.
*/
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdStreamers : public cfdObjects
{
public:
   /* Initialize the VTK objects and pipeline.
   Glyph(s) are from cfdPlanes's multiple points plane cursor.  */
   cfdStreamers( void );

   ~cfdStreamers();

   // compare VjObs_i commandArray with its child's value
   //virtual bool CheckCommandId( cfdCommandArray* _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   virtual void Update( void );

   vtkPolyData* GetStreamersOutput( void );

   void SetIntegrationDirection( int );

   void SetPropagationTime( int );

   void SetIntegrationStepLength( int );

   void SetStepLength( int );
   
   int cfdIso_value;

private:
   //void UpdateTracker( float x[3], float v[3] ); // Update the position of the cursors.

   vtkStreamLine*     stream;
   vtkTubeFilter*     tubeFilter;
   vtkPolyDataMapper* mapper;
   vtkRungeKutta45*   integ;  

   float propagationTime;
   float integrationStepLength;
   float stepLength;
   int   integrationDirection;
   float lineDiameter;
   int   streamArrows;
   float arrowDiameter;
};
}
#endif
