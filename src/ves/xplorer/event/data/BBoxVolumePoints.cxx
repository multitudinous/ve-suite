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
#include <ves/xplorer/event/viz/cfdCursor.h>
#include <ves/xplorer/event/viz/cfdEnum.h>
#include <ves/xplorer/scenegraph/cfdDCS.h>
#include <ves/xplorer/scenegraph/cfdGeode.h>
#include <ves/xplorer/event/viz/cfdCommandArray.h>
#include <ves/xplorer/scenegraph/cfdGroup.h>
#include <ves/xplorer/event/viz/cfdDataSet.h>
#include <ves/xplorer/event/viz/cfdObjects.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/event/viz/cfdModel.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#ifdef _PERFORMER
#include <Performer/pfdu.h>
#include <Performer/pf/pfNode.h>
#include <vrj/Draw/Pf/PfUtil.h>
#elif _OSG
#include <osg/Node>
#endif

#include <vtkPolyData.h>
#include <vtkPolyDataSource.h>
#include <vtkCubeSource.h>
#include <vtkGlyph3D.h>
#include <vtkLineSource.h>
#include <vtkPlaneSource.h>
#include <vtkPointSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkSphereSource.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

#define CURSOR_DIST 2.0f
#define BOX_LENGTH 2.0f

#include <gmtl/gmtl.h>
#include <ves/xplorer/cfdDebug.h>
#include <gmtl/Matrix.h>
#include <gmtl/Xforms.h>
#include <gmtl/Vec.h>
#include <gmtl/VecOps.h>
#include <gmtl/Output.h>
#include <gmtl/EulerAngle.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#include <gmtl/EulerAngle.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Output.h>
#include <gmtl/AxisAngleOps.h>

using namespace gmtl; //added by Gengxun
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
////////////////////////////////////////////////////////////////////////////////
BBoxVolumePoints::BBoxVolumePoints( void )
{
}
////////////////////////////////////////////////////////////////////////////////
BBoxVolumePoints::~BBoxVolumePoints()
{
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* BBoxVolumePoints::GetSeedPointPolyData( void )
{
   return pointPolyData;
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* BBoxVolumePoints::GetCursorPolyData( void )
{
   return cursorPolyData;
}
////////////////////////////////////////////////////////////////////////////////
void BBoxVolumePoints::CreatePointsCursor( void )
{
   //collect desired number of points from user
   cout <<"Enter in number of points desired along X-axis: " <<endl;
   cin>> xValue;		
   
   cout <<"Enter in number of points desired along Y-axis: " <<endl;
   cin>> yValue;
   
   cout <<"Enter in number of points desired along Z-axis: " <<endl;
   cin>> zValue;	
   
   
   
   //put bounds into an array and then extract, used for computing placement of points
   
   wireframe->GetOutput()->GetBounds(bounds);
   xMin=bounds[0];
   xMax=bounds[1];
   yMin=bounds[2];
   yMax=bounds[3];
   zMin=bounds[4];
   zMax=bounds[5];
   
   
   //insert evenly spaced points inside bounding box
   vtkPoints* points = vtkPoints::New();
   
   for (int i = 1; i <= xValue ; ++i)
   {
      
      for (int j = 1; j <= yValue; ++j){
         
         for(int k = 1; k <= zValue; k++)			
         {
            //points added in ptMin + length*iteration/(number of equal segments)
            //where (number of equal segments) = ptValue+1
            
            points->InsertPoint(number,( xMin + ((xMax-xMin)*(i))/(xValue+1)),( yMin + ((yMax-yMin)*(j))/(yValue+1)),( zMin +
                                                                                                                       ((zMax-zMin)*(k))/(zValue+1))); 
            number=number+1;
         }
      }
      
   }
   
   
   //create polydata to be glyphed
   vtkPolyData* poly = vtkPolyData::New();
   poly->SetPoints(points);
   
   //use sphere as glyph source
   vtkSphereSource *sphere = vtkSphereSource::New();
   sphere->SetRadius(0.1);
   sphere->SetPhiResolution(10);
   sphere->SetThetaResolution(10);
   
   vtkGlyph3D* glyphPoints = vtkGlyph3D::New();
   glyphPoints->SetInput(poly);
   glyphPoints->SetSource(sphere->GetOutput());
   
   vtkPolyDataMapper* sphereMapper = vtkPolyDataMapper::New();
   sphereMapper->SetInput(glyphPoints->GetOutput());
   
   //create Actor for glyph
   vtkActor *glyphActor = vtkActor::New();
   glyphActor->SetMapper(sphereMapper);
   glyphActor->GetProperty()->SetDiffuseColor(1,0,0);
   glyphActor->GetProperty()->SetSpecular(0.3);
   glyphActor->GetProperty()->SetSpecularPower(30);
}
