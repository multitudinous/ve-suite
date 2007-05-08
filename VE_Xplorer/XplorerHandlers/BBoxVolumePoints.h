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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef BBOXVOLUMEPOINTS_H
#define BBOXVOLUMEPOINTS_H
/*!\file BBoxVolumePoints.h
  BBoxVolumePoints API
  */
/*!\class BBoxVolumePoints
 * Class for handling volume points in a volume box.
 */
namespace VE_SceneGraph
{
   
}

class vtkGlyph3D;
class vtkCubeSource;
class vtkSphereSource;
class vtkPolyData;
class vtkPolyDataNormals;
class vtkPolyDataMapper;
class vtkActor;
class vtkPointSource;
class vtkPlaneSource;
class vtkLineSource;
class vtkPolyDataSource;

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdDataSet;
}

namespace VE_XML
{
   class Command;
}

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

//! Virtual cursors
/*!
   A class to build virtual cursors. Type of virtual
   cursor built are single point, arrow, and multiple points.
*/
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS BBoxVolumePoints : public cfdGlobalBase
{
public:
   ///Constructor
   BBoxVolumePoints( void );
   ///Destructor
   ~BBoxVolumePoints( void );

private:
};
}
#endif //BBOXVOLUMEPOINTS_H
