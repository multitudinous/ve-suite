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
#ifndef CFD_CONTOUR_BASE_H
#define CFD_CONTOUR_BASE_H
/*!\file cfdContourBase.h
cfdContourBase API
*/
/*!\class VE_Xplorer::cfdContourBase
* 
*/
#include <VE_Xplorer/XplorerHandlers/cfdObjects.h>

class vtkPolyData;
class vtkPolyDataMapper;
class vtkMultiGroupPolyDataMapper;
class vtkGeometryFilter;
class vtkMultiGroupDataGeometryFilter;
class vtkContourFilter;
class vtkBandedPolyDataContourFilter;
class vtkDecimatePro;
class vtkTriangleFilter;
class vtkStripper;
class vtkPolyDataNormals;
class vtkCutter;

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdCuttingPlane;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdContourBase : public cfdObjects
{
public:
   ///Constructor
   cfdContourBase();
   ///Destructor
   virtual ~cfdContourBase();

   ///update the actor
   virtual void Update( void ) = 0;

   ///compare VjObs_i commandArray with its child's value
   ///\param _cfdCommandArray
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   ///Sets the Mapper Input
   ///\param polydata
   void SetMapperInput( vtkPolyData* polydata );

   ///Sets the Fill Type
   ///\param fillType
   void SetFillType( const int fillType);

   ///Create the Plane
   void CreatePlane();

protected:
   //vtkPolyDataMapper*   mapper;
   vtkMultiGroupPolyDataMapper* mapper;///<mapper.
   vtkMultiGroupDataGeometryFilter*   filter;
   vtkContourFilter*    cfilter;///<Contour filter.
   vtkBandedPolyDataContourFilter* bfilter;///<banded contour filter.
   vtkDecimatePro*      deci;///<decimator.
   vtkTriangleFilter*   tris;///<trangle filter for vtk.
   vtkStripper*         strip;///<strip.
   vtkPolyDataNormals*  normals;///<polydata normals.
   vtkCutter*           cutter;///<cutter for vtk.

   cfdCuttingPlane* cuttingPlane;///<cutting plane

   int fillType;///<Representation of contour line.
   double warpedContourScale;///<Scale of warped contour
   double contourOpacity;///<Level of Opacity.
   int contourLOD;///<Level of Detail.
   int xyz;///<Value of surface.
   int numSteps;///<Number of timesteps.
};
}
#endif
