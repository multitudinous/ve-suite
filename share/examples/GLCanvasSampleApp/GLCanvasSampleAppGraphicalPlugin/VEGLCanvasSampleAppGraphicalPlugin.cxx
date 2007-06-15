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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VEGLCanvasSampleAppGraphicalPlugin.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Conductor/Framework/string_ops.h"
#include "VE_Xplorer/cfdCursor.h"

#include <fstream>
#include <sstream>
//#include <cstdlib>
#include <string>
#include <map>

#include <vrj/Util/Debug.h>

#include <vtkSphereSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>

using namespace std;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// Constructor
VEGLCanvasSampleAppGraphicalPlugin::VEGLCanvasSampleAppGraphicalPlugin( void ) : cfdVEBaseClass()
{
  _objectName ="GLCanvasSampleApp"; // Needs to match plugin name
   //_onSceneGraph = false;
   _geode = NULL;
   _param.clear();
}

// Destructor
VEGLCanvasSampleAppGraphicalPlugin::~VEGLCanvasSampleAppGraphicalPlugin( void )
{
   if ( !_param.empty() )
      _param.clear();
   if ( _geode != NULL )
      delete _geode;
}

void VEGLCanvasSampleAppGraphicalPlugin::InitializeNode( cfdDCS* veworldDCS )
{
   cfdVEBaseClass::InitializeNode( veworldDCS );
   //_param.assign( "./VEGLCanvasSampleAppGraphicalPlugin.param");
   CreateObjects();
}

// Was initially as "void VEOPPDmod::"
// Balu Changed it to "void VEGLCanvasSampleAppGraphicalPlugin::" on 1/25/2006
void VEGLCanvasSampleAppGraphicalPlugin::CreateCustomVizFeature( int input )
{
   xcoord = myInterface.getDouble("xcoord");
   ycoord = myInterface.getDouble("ycoord");


   vtkSphereSource*    sphereSrc      = vtkSphereSource::New();
   vtkPolyDataNormals* sphereNorm     = vtkPolyDataNormals::New();
   vtkPolyDataMapper*  sphereMapper   = vtkPolyDataMapper::New();
   vtkActor*           sphereActor    = vtkActor::New();


   ///////////////////////////////////////////////
   cout << " sphere stuff " << endl;
   sphereSrc->SetRadius( radius );
   sphereSrc->SetEndPhi( 90 );

   sphereSrc->SetCenter( 0, 0, 0 );
   sphereSrc->Update();

   sphereNorm->SetInput( sphereSrc->GetOutput() );
   sphereNorm->Update();

   sphereMapper->SetInput( sphereNorm->GetOutput() );
   sphereMapper->Update();

   sphereActor->SetMapper( sphereMapper );
   sphereActor->GetProperty()->SetColor( 0.0, 0.0, 0.8 );
   sphereActor->GetProperty()->SetOpacity( 0.5 );
   sphereActor->GetProperty()->SetInterpolationToPhong();
   // Can also set opacity

   //GetWorldDCS()->RemoveChild( _geode );

   _geode = new cfdGeode();

   _geode->TranslateTocfdGeode( sphereActor );

   sphereSrc->Delete();
   sphereNorm->Delete();
   sphereMapper->Delete();
   sphereActor->Delete();

   GetWorldDCS()->AddChild( _geode );

   std::ostringstream geomFileStream;
   geomFileStream << "./Plugins/Cube.stl";
   _model->CreateGeomDataSet( geomFileStream.str() );
   _model->GetGeomDataSet(-1)->GetDCS()->SetScaleArray( scale );
   _model->GetGeomDataSet(-1)->GetDCS()->SetTranslationArray( trans );
   _model->GetGeomDataSet(-1)->GetDCS()->SetRotationArray(rotate);
   _model->GetGeomDataSet(-1)->SetFILEProperties(color, transFlag, stlColor);
   _model->GetGeomDataSet(-1)->setOpac(1.0f);

}

