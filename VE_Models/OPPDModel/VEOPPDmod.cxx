/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdVEBaseClass.cxx,v $
 * Date modified: $Date: 2004-08-28 12:35:59 -0700 (Sat, 28 Aug 2004) $
 * Version:       $Rev: 858 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VEOPPDmod.h"
//#include "cfdVEBaseClass.h"
#include "cfdModuleGeometry.h"
#include "cfdGroup.h"
#include "cfdModel.h"
#include "cfdReadParam.h"
#include "fileIO.h"
#include "cfdFILE.h"
#include "cfdDataSet.h"
#include "cfdGeode.h"
#include "cfdDCS.h"

#include <fstream>
#include <cstdlib>
#include <string>
#include <map>

#include <vrj/Util/Debug.h>

#include <vtkSphereSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>

#ifndef _WIN32 // not windows
#include <sys/dir.h>
#else // it is windows
#include <windows.h>
#include <direct.h>
#endif

using namespace std;

IMPLEMENT_DYNAMIC_CLASS( VEOPPDmod, cfdVEBaseClass )

// Constructor
VEOPPDmod::VEOPPDmod( void ) : cfdVEBaseClass()
{
   _objectName ="OPPD";
   //_onSceneGraph = false;
   _geode = NULL;
}

// Destructor
VEOPPDmod::~VEOPPDmod( void )
{
   if ( _param )
      delete [] _param;
   //delete this->dataRepresentation;
}

void VEOPPDmod::InitializeNode( cfdDCS* veworldDCS )
{
   cfdVEBaseClass::InitializeNode( veworldDCS );
   this->_param = new char[100];
   this->_param = "/home/vr/Applications/TSVEG/Test_Pit/OPPD/Current_Demo/vrxpr.param.geomorg.useonwall";
   //cout << _param << endl;
   CreateObjects();
}

cfdGeode* VEOPPDmod::GetCustomVizFeature( int input )
{
   // Case 0 -- single point with sphere polygon.
   //   Building the sphere source.
   vtkSphereSource*    sphereSrc      = vtkSphereSource::New();
   vtkPolyDataNormals* sphereNorm     = vtkPolyDataNormals::New();
   vtkPolyDataMapper*  sphereMapper   = vtkPolyDataMapper::New();
   vtkActor*           sphereActor    = vtkActor::New();

   sphereSrc->SetRadius( 0.05f );
   sphereSrc->SetCenter( 0.0f, 0.0f, 0.0f );
   sphereSrc->Update();

   sphereNorm->SetInput( sphereSrc->GetOutput() );
   sphereNorm->Update();

   sphereMapper->SetInput( sphereNorm->GetOutput() );
   sphereMapper->Update();

   sphereActor->SetMapper( sphereMapper );
   sphereActor->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
   // Can also set opacity
   if ( _geode != NULL )
   {
      _dcs->RemoveChild( (cfdSceneNode*)_geode );
      delete _geode;
   }

   _geode = new cfdGeode();

   _geode->TranslateTocfdGeode( sphereActor );

   sphereSrc->Delete();
   sphereNorm->Delete();
   sphereMapper->Delete();
   sphereActor->Delete();

   _dcs->AddChild( (cfdSceneNode*)_geode );
   return NULL;
}

