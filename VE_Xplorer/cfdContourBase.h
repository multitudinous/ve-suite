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
 * File:          $RCSfile: cfdContourBase.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_CONTOUR_BASE_H
#define CFD_CONTOUR_BASE_H

#include "VE_Xplorer/cfdObjects.h"

class vtkPolyData;
class vtkPolyDataMapper;
class vtkGeometryFilter;
class vtkContourFilter;
class vtkBandedPolyDataContourFilter;
class vtkDecimatePro;
class vtkTriangleFilter;
class vtkStripper;
class vtkPolyDataNormals;

namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdContourBase : public cfdObjects
{
public:
   cfdContourBase();

   virtual ~cfdContourBase();

   // update the actor
   virtual void Update( void ) = 0;

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   void SetMapperInput( vtkPolyData * );

   void SetFillType( const int );

protected:
   vtkPolyDataMapper*   mapper;
   vtkGeometryFilter*   filter;
   vtkContourFilter*    cfilter;
   vtkBandedPolyDataContourFilter* bfilter;
   vtkDecimatePro*      deci;
   vtkTriangleFilter*   tris;
   vtkStripper*         strip;
   vtkPolyDataNormals*  normals;

   int fillType;
   double warpedContourScale;
   double contourOpacity;
   int contourLOD; 
};
}
#endif
