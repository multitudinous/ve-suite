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
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdNode.h"
#include "VE_SceneGraph/cfdSceneNode.h"
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

using namespace VE_SceneGraph;
using namespace VE_Xplorer;

// Constructor
VEOPPDmod::VEOPPDmod( void ) : cfdVEBaseClass()
{
   _objectName ="OPPD";
   _geode = NULL;
   _param.clear();
}

// Destructor
VEOPPDmod::~VEOPPDmod( void )
{
   if ( !_param.empty() )
      _param.clear();
   //delete this->dataRepresentation;
   if ( _geode != NULL )
      delete _geode;
}

void VEOPPDmod::InitializeNode( cfdDCS* veworldDCS )
{
   cfdVEBaseClass::InitializeNode( veworldDCS );
   _param.assign( "./Plugins/vrxpr.param.split");
   CreateObjects();
   _model->GetGeomDataSet( 18 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 18 )->GetNode() );
   _model->GetGeomDataSet( 19 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 19 )->GetNode() );
   _model->GetGeomDataSet( 20 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 20 )->GetNode() );
   _model->GetGeomDataSet( 21 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 21 )->GetNode() );
   _model->GetGeomDataSet( 22 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 22 )->GetNode() );
   _model->GetGeomDataSet( 23 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 23 )->GetNode() );
   _model->GetGeomDataSet( 24 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 24 )->GetNode() );
   _model->GetGeomDataSet( 25 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 25 )->GetNode() );
}

void VEOPPDmod::CreateCustomVizFeature( int input )
{
   for ( int i=2; i<10; i++ )
   {
      if ( _model->GetGeomDataSet( i )->GetDCS()->SearchChild( _model->GetGeomDataSet( i )->GetNode() ) == -1 )
      {
         _model->GetGeomDataSet( i )->GetDCS()->AddChild( _model->GetGeomDataSet( i )->GetNode() );
      }
   }

   for ( int i=18; i<26; i++ )
   {
      if ( _model->GetGeomDataSet( i )->GetDCS()->SearchChild( _model->GetGeomDataSet( i )->GetNode() ) != -1 )
      {
         _model->GetGeomDataSet( i )->GetDCS()->RemoveChild( _model->GetGeomDataSet( i )->GetNode() );
      }
   }

   if ( v_value.empty() )
      return;

   double radius = 0;
   double visdist = 10000000;
   double hotgastemp = 0;
   bool damagedtrays = false;

   // Find radius result
   for ( unsigned int i = 0; i < v_desc.size(); i++ )
   {
      if( v_desc[ i ] == std::string( "Pool_Fire_Flame_Height_Hesk_FT" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from OPPD " << var << endl;
         string_to_double( var, radius );
      }
      else if ( v_desc[ i ] == std::string( "Vis_Dist_Through_Smoke" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from OPPD " << var << endl;
         string_to_double( var, visdist );
      }
      else if ( v_desc[ i ] == std::string( "Hot_Gas_Layer_Temp_Closed_F" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from OPPD " << var << endl;
         string_to_double( var, hotgastemp );
      }
   }


   if ( hotgastemp >= 500 )
   {
      damagedtrays = true;
      cout << "Damage to Cables -> " << damagedtrays << std::endl;
   }

   _cursor->GetLocalLocationVector();
   double* cursorLoc = _cursor->ReturnLocalLocationVector();
   cout << "Cursor[0]: " << cursorLoc[ 0 ] << endl;
   cout << "Cursor[1]: " << cursorLoc[ 1 ] << endl;
   cout << "Cursor[2]: " << cursorLoc[ 2 ] << endl;

/*Geometry File List
   0-walls.ive         
   1-floor.ive         
   2-cabletray1.ive    
   3-cabletray2.ive    
   4-cabletray3.ive    
   5-cabletray4.ive    
   6-cabletray5.ive    
   7-cabletray6.ive    
   8-cabletray7.ive    
   9-compressors.ive
   10-blue_pipes.ive    
   11-white_pipes.ive   
   12-tanks.ive         
   13-rooms.ive         
   14-pumps.ive         
   15-lights.ive        
   16-ducts.ive         
   17-sprinkler.ive      
   18-red_cabletray1.ive    
   19-red_cabletray2.ive    
   20-red_cabletray3.ive    
   21-red_cabletray4.ive    
   22-red_cabletray5.ive    
   23-red_cabletray6.ive    
   24-red_cabletray7.ive 
   25-red_compressors.ive     
*/

   if ( damagedtrays )
   {
      //if ( cursorLoc[ 0 ] > 0 && cursorLoc[ 0 ] < 10 )
      //{
         if ( cursorLoc[ 1 ] > -110 && cursorLoc[ 1 ] < -80 )
         {
            _model->GetGeomDataSet( 2 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 2 )->GetNode() );
            _model->GetGeomDataSet( 18 )->GetDCS()->AddChild( _model->GetGeomDataSet( 18 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > -90 && cursorLoc[ 1 ] < -50 )
         {
            _model->GetGeomDataSet( 3 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 3 )->GetNode() );
            _model->GetGeomDataSet( 19 )->GetDCS()->AddChild( _model->GetGeomDataSet( 19 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > -60 && cursorLoc[ 1 ] < -15 )
         {
            _model->GetGeomDataSet( 4 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 4 )->GetNode() );
            _model->GetGeomDataSet( 20 )->GetDCS()->AddChild( _model->GetGeomDataSet( 20 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > -25 && cursorLoc[ 1 ] < 15 )
         {
            _model->GetGeomDataSet( 5 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 5 )->GetNode() );
            _model->GetGeomDataSet( 21 )->GetDCS()->AddChild( _model->GetGeomDataSet( 21 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > 5 && cursorLoc[ 1 ] < 30 )
         {
            _model->GetGeomDataSet( 6 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 6 )->GetNode() );
            _model->GetGeomDataSet( 22 )->GetDCS()->AddChild( _model->GetGeomDataSet( 22 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > 20 && cursorLoc[ 1 ] < 55 )
         {
            _model->GetGeomDataSet( 7 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 7 )->GetNode() );
            _model->GetGeomDataSet( 23 )->GetDCS()->AddChild( _model->GetGeomDataSet( 23 )->GetNode() );
         }  
         if ( cursorLoc[ 1 ] > 45 && cursorLoc[ 1 ] < 85 )
         {
            _model->GetGeomDataSet( 8 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 8 )->GetNode() );
            _model->GetGeomDataSet( 24 )->GetDCS()->AddChild( _model->GetGeomDataSet( 24 )->GetNode() );
         }  

      //}
   }

   if ( ( cursorLoc[ 1 ] - radius ) < -81 && ( cursorLoc[ 1 ] + radius ) > -105 )
   {
      if ( ( cursorLoc[ 0 ] - radius ) < 10 && ( cursorLoc[ 0 ] + radius ) > 4.5 )
      {
         _model->GetGeomDataSet( 9 )->GetDCS()->RemoveChild( _model->GetGeomDataSet( 9 )->GetNode() );
         _model->GetGeomDataSet( 25 )->GetDCS()->AddChild( _model->GetGeomDataSet( 25 )->GetNode() );        
      }
   }


   ////////////////////////////////////////////
   // Case 0 -- single point with sphere polygon.
   //           Building the sphere source.  
   vtkSphereSource*    sphereSrc      = vtkSphereSource::New();
   vtkPolyDataNormals* sphereNorm     = vtkPolyDataNormals::New();
   vtkPolyDataMapper*  sphereMapper   = vtkPolyDataMapper::New();
   vtkActor*           sphereActor    = vtkActor::New();


   ///////////////////////////////////////////////
   cout << " sphere stuff " << endl;
   sphereSrc->SetRadius( radius );
   sphereSrc->SetEndPhi( 90 );

   //sphereSrc->SetCenter( cursorLoc[ 0 ], cursorLoc[ 1 ], -12 );
   sphereSrc->SetCenter( 0, 0, 0 );
   sphereSrc->Update();

   sphereNorm->SetInput( sphereSrc->GetOutput() );
   sphereNorm->Update();

   sphereMapper->SetInput( sphereNorm->GetOutput() );
   sphereMapper->Update();

   sphereActor->SetMapper( sphereMapper );
   sphereActor->GetProperty()->SetColor( 1.0, 0.0, 0.0 );
   sphereActor->GetProperty()->SetOpacity( 0.5 );
   sphereActor->GetProperty()->SetInterpolationToPhong();
   // Can also set opacity
   /*if ( _geode != NULL )
   {
      GetWorldDCS()->RemoveChild( _geode );
      delete _geode;
   }*/
   _cursor->GetcfdDCS()->RemoveChild( _geode );

   _geode = new cfdGeode();

   _geode->TranslateTocfdGeode( sphereActor );

   sphereSrc->Delete();
   sphereNorm->Delete();
   sphereMapper->Delete();
   sphereActor->Delete();

   //GetWorldDCS()->AddChild( _geode );
   _cursor->GetcfdDCS()->AddChild( _geode );
   ///////////////////////////////////////////////
   ///////////////////////////////////////////////
   cout << " Turn on fog " << endl;

   for ( unsigned int i = 0; i < _model->GetNumberOfGeomDataSets(); i++ )
   {
      cout << i << endl;
      _model->GetGeomDataSet( i )->setFog( visdist );
   }
}

