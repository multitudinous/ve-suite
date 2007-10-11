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
#include <ves/xplorer/event/CreateVisObjectEventHandler.h>
#include <ves/xplorer/event/cfdModel.h>
#include <ves/xplorer/event/ModelCADHandler.h>
#include <ves/xplorer/event/cfdModelHandler.h>
#include <ves/xplorer/event/cfdDataSet.h>
#include <ves/xplorer/util/fileIO.h>

#include "VE_Xplorer/XplorerHandlers/cfdPolyData.h"      
#include "VE_Xplorer/XplorerHandlers/cfdIsosurface.h"    
#include "VE_Xplorer/XplorerHandlers/cfdPresetContour.h" 
#include "VE_Xplorer/XplorerHandlers/cfdContours.h"      
#include "VE_Xplorer/XplorerHandlers/cfdMomentum.h"      
#include <ves/xplorer/event/cfdPresetMomentum.h>
#include "VE_Xplorer/XplorerHandlers/cfdMomentums.h"     
#include "VE_Xplorer/XplorerHandlers/cfdVector.h"        
#include "VE_Xplorer/XplorerHandlers/cfdPresetVector.h"  
#include "VE_Xplorer/XplorerHandlers/cfdVectors.h"       
#include "VE_Xplorer/XplorerHandlers/cfdStreamers.h"     
#include "VE_Xplorer/XplorerHandlers/cfdPolyData.h"      
#include "VE_Xplorer/XplorerHandlers/cfdImage.h"         
#include "VE_Xplorer/XplorerHandlers/cfdAnimatedImage.h" 
#include <ves/xplorer/event/cfdAnimatedStreamlineCone.h>
#include <ves/xplorer/event/cfdContour.h>
#include <ves/xplorer/event/cfdModelHandler.h>
#include <ves/xplorer/event/cfdEnum.h>
#include <ves/xplorer/event/cfdEnvironmentHandler.h>
#include <ves/xplorer/event/cfdSteadyStateVizHandler.h>
#include <ves/xplorer/event/cfdCursor.h>
#include <ves/xplorer/event/cfdTextOutput.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/event/cfdDebug.h>

#include <VE_Xplorer/TextureBased/cfdTextureDataSet.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_TextureBased;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler()
:VE_EVENTS::EventHandler()
{
   this->surface = 0;
   this->isosurface = 0;
   this->contour = 0;
   this->momentum = 0;
   this->vector = 0;
   this->x_contour = 0;
   this->y_contour = 0;
   this->z_contour = 0;
   this->x_momentum = 0;
   this->y_momentum = 0;
   this->z_momentum = 0;
   this->x_vector = 0;
   this->y_vector = 0;
   this->z_vector = 0;
   this->x_contours = 0;
   this->y_contours = 0;
   this->z_contours = 0;
   this->x_momentums = 0;
   this->y_momentums = 0;
   this->z_momentums = 0;
   this->x_vectors = 0;
   this->y_vectors = 0;
   this->z_vectors = 0;
   this->streamlines = 0;
   this->particles = 0;
   this->image = 0;
   this->animStreamer = 0;
   this->animImg = 0;
   this->textOutput = 0;
   //_activeModel = 0;
   
   // Initialize all the vis objects from ssvishandler
   //
   // Initiate the isosurface.
   //
   std::pair< std::string, std::pair< std::string, std::string > > objectType;
   objectType = std::make_pair( std::string( "UPDATE_ISOSURFACE_SETTINGS" ), std::make_pair( "", "" ) );
   std::cout << "| 14. Initializing...................................... Isosurface |" << std::endl;
   this->isosurface = new cfdIsosurface( 10 );
   this->isosurface->SetObjectType( ISOSURFACE );
   visObjectMap[ objectType ] = this->isosurface;

   //
   // Initiate the interactive contour.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("wand");
   objectType.second.second = std::string("Single");
   std::cout << "| 15. Initializing......................................... Contour |" << std::endl;
   this->contour = new cfdContour();
   this->contour->SetObjectType( CONTOUR );
   visObjectMap[ objectType ] = this->contour;

   //
   // Initiate the interactive momentum.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("wand");
   objectType.second.second = std::string("Single-warp");
   std::cout << "| 16. Initializing........................................ Momemtum |" << std::endl;
   this->momentum = new cfdMomentum();
   this->momentum->SetObjectType( MOMENTUM );
   visObjectMap[ objectType ] = this->momentum;

   //
   // Initiate the interactive vector.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("wand");
   objectType.second.second = std::string("Single");
   std::cout << "| 17. Initializing.......................................... Vector |" << std::endl;
   this->vector = new cfdVector();
   this->vector->SetObjectType( VECTOR );
   visObjectMap[ objectType ] = this->vector;

   //
   // Initiate the preset x contour.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Single");
   std::cout << "| 19. Initializing................................ Preset x Contour |" << std::endl;
   this->x_contour = new cfdPresetContour( 0, 10 );
   this->x_contour->SetObjectType( X_CONTOUR );
   visObjectMap[ objectType ] = this->x_contour;

   //
   // Initiate the preset y contour.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Single");
   std::cout << "| 20. Initializing................................ Preset y Contour |" << std::endl;
   this->y_contour = new cfdPresetContour( 1, 10 );
   this->y_contour->SetObjectType( Y_CONTOUR );
   visObjectMap[ objectType ] = this->y_contour;

   //
   // Initiate the preset z contour.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Single");
   std::cout << "| 21. Initializing................................ Preset z Contour |" << std::endl;
   this->z_contour = new cfdPresetContour( 2, 10 );
   this->z_contour->SetObjectType( Z_CONTOUR );
   visObjectMap[ objectType ] = this->z_contour;

   //
   // Initiate the preset x momentum.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Single-warp");
   std::cout << "| 22. Initializing............................... Preset x Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->x_momentum = new cfdPresetMomentum( 0, 10 );
   this->x_momentum->SetObjectType( X_MOMENTUM );
   visObjectMap[ objectType ] = this->x_momentum;

   //
   // Initiate the preset y momentum.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Single-warp");
   std::cout << "| 23. Initializing............................... Preset y Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->y_momentum = new cfdPresetMomentum( 1, 10 );
   this->y_momentum->SetObjectType( Y_MOMENTUM );
   visObjectMap[ objectType ] = this->y_momentum;

   //
   // Initiate the preset z momentum.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Single-warp");
   std::cout << "| 24. Initializing............................... Preset z Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->z_momentum = new cfdPresetMomentum( 2, 10 );
   this->z_momentum->SetObjectType( Z_MOMENTUM );
   visObjectMap[ objectType ] = this->z_momentum;

   //
   // Initiate the preset x vector.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Single");
   std::cout << "| 25. Initializing................................. Preset x Vector |" << std::endl;
   this->x_vector = new cfdPresetVector( 0, 10 );
   this->x_vector->SetObjectType( X_VECTOR );
   visObjectMap[ objectType ] = this->x_vector;

   //
   // Initiate the preset y vector.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Single");
   std::cout << "| 26. Initializing................................. Preset y Vector |" << std::endl;
   this->y_vector = new cfdPresetVector( 1, 10 );
   this->y_vector->SetObjectType( Y_VECTOR );
   visObjectMap[ objectType ] = this->y_vector;

   //
   // Initiate the preset z vector.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Single");
   std::cout << "| 27. Initializing................................. Preset z Vector |" << std::endl;
   this->z_vector = new cfdPresetVector( 2, 10 );
   this->z_vector->SetObjectType( Z_VECTOR );
   visObjectMap[ objectType ] = this->z_vector;

   //
   // Initiate the preset x contour lines.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 28. Initializing....................Multiple X-planes of Contours |" << std::endl;
   this->x_contours = new cfdContours( 0 );
   this->x_contours->SetObjectType( X_CONTOURS );
   visObjectMap[ objectType ] = this->x_contours;

   //
   // Initiate the preset y contour lines.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 29. Initializing....................Multiple Y-planes of Contours |" << std::endl;
   this->y_contours = new cfdContours( 1 );
   this->y_contours->SetObjectType( Y_CONTOURS );
   visObjectMap[ objectType ] = this->y_contours;

   //
   // Initiate the preset z contour lines.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
   this->z_contours = new cfdContours( 2 );
   this->z_contours->SetObjectType( Z_CONTOURS );
   visObjectMap[ objectType ] = this->z_contours;

   //
   // Initiate the preset x momentums.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Multiple-warp");
   std::cout << "| 31. Initializing.......Multiple X-planes of Precomputed Momentums |" << std::endl;
   this->x_momentums = new cfdMomentums( 0 );
   this->x_momentums->SetObjectType( X_MOMENTUMS );
   visObjectMap[ objectType ] = this->x_momentums;

   //
   // Initiate the preset y momentums.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Multiple-warp");
   std::cout << "| 32. Initializing.......Multiple Y-planes of Precomputed Momentums |" << std::endl;
   this->y_momentums = new cfdMomentums( 1 );
   this->y_momentums->SetObjectType( Y_MOMENTUMS );
   visObjectMap[ objectType ] = this->y_momentums;

   //
   // Initiate the preset z momentums.
   //
   objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Multiple-warp");
   std::cout << "| 33. Initializing.......Multiple Z-planes of Precomputed Momentums |" << std::endl;
   this->z_momentums = new cfdMomentums( 2 );
   this->z_momentums->SetObjectType( Z_MOMENTUMS );
   visObjectMap[ objectType ] = this->z_momentums;

   //
   // Initiate the preset x vectors.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("x");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
   this->x_vectors = new cfdVectors( 0 );
   this->x_vectors->SetObjectType( X_VECTORS );
   visObjectMap[ objectType ] = this->x_vectors;

   //
   // Initiate the preset y vectors.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("y");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
   this->y_vectors = new cfdVectors( 1 );
   this->y_vectors->SetObjectType( Y_VECTORS );
   visObjectMap[ objectType ] = this->y_vectors;

   //
   // Initiate the preset z vectors.
   //
   objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
   objectType.second.first = std::string("z");
   objectType.second.second = std::string("Multiple");
   std::cout << "| 36. Initializing.........Multiple Z-planes of Precomputed Vectors |" << std::endl;
   this->z_vectors = new cfdVectors( 2 );
   this->z_vectors->SetObjectType( Z_VECTORS );
   visObjectMap[ objectType ] = this->z_vectors;

   //
   // Initiate the streamlines.
   //
   objectType.first = std::string( "UPDATE_STREAMLINE_SETTINGS" );
   objectType.second.first = std::string("");
   objectType.second.second = std::string("");
   std::cout << "| 37. Initializing..................................... Streamlines |" << std::endl;
   this->streamlines = new cfdStreamers();
   this->streamlines->SetObjectType( STREAMLINES );
   visObjectMap[ objectType ] = this->streamlines;

   //
   // Initiate the animated streamers.
   //
   objectType.first = std::string( "UPDATE_STREAMLINE_SETTINGS" );
   objectType.second.first = std::string("animated");
   objectType.second.second = std::string("");
   std::cout << "| 39. Initializing............................. Animated Streamline |" << std::endl;
   this->animStreamer = new cfdAnimatedStreamlineCone();
   this->animStreamer->SetObjectType( ANIMATED_STREAMLINES );
   visObjectMap[ objectType ] = this->animStreamer;

   //
   // Initiate the animated Images.
   //
   //std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
   //this->animImg = new cfdAnimatedImage( _param.c_str() );
   //this->animImg->SetObjectType( ANIMATED_IMAGES );
   //this->dataList.push_back( this->animImg);  

   //
   // Initiate the PolyData File
   //
   objectType.first = std::string( "UPDATE_PARTICLE_SETTINGS" );
   objectType.second.first = std::string("");
   objectType.second.second = std::string("");
   std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
   this->particles = new cfdPolyData();
   this->particles->SetObjectType( PARTICLES );
   visObjectMap[ objectType ] = this->particles;

   objectType.first = std::string( "UPDATE_POLYDATA_SETTINGS" );
   objectType.second.first = std::string("");
   objectType.second.second = std::string("");
   std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
   this->surface = new cfdPolyData( 1.0 );
   this->surface->SetObjectType( POLYDATA );
   visObjectMap[ objectType ] = this->surface;

   //
   // Initiate PIV data from INEL
   //
   //std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
   /*this->image = new cfdImage( _param );
   this->image->SetObjectType( IMAGE_EX );
   this->dataList.push_back( this->image ); */    
   //
   // Initiate the Performer objects.
   //
   //std::cout << "| 51. Initializing........................................ pfGeodes |" << std::endl;
   
   //for ( int i = 0; i < (int)this->dataList.size(); i++ )
   /*std::map< int, cfdObjects* >::iterator iter;
   for ( iter = visObjectMap.begin(); iter != viObjectMap.end(); ++iter )
   {
      // Initialize all the geode creation flags and dcs flags for all the geodes
      iter->SetUpdateFlag( false );
      iter->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
   }*/
}
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler(const CreateVisObjectEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::~CreateVisObjectEventHandler()
{
   //delete all the objects from ssvishandler
   if ( this->isosurface ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->isosurface" << std::endl << vprDEBUG_FLUSH;
      delete this->isosurface;
   }
   
   if ( this->contour ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->contour" << std::endl << vprDEBUG_FLUSH;
      delete this->contour;
   }
   
   if ( this->momentum ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->momentum;
   }
   
   if ( this->vector ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->vector" << std::endl << vprDEBUG_FLUSH;
      delete this->vector;
   }
   
   if ( this->x_contour ) 
   {
      //vprDEBUG(vesDBG,2)   
      //<< "deleting this->x_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contour;
   }
   
   if ( this->y_contour ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->y_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contour;
   }
   
   if ( this->z_contour ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->z_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contour;
   }
   
   if ( this->x_momentum ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->x_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentum;
   }
   
   if ( this->y_momentum ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->y_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentum;
   }
   
   if ( this->z_momentum ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->z_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentum;
   }
   
   if ( this->x_vector ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->x_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vector;
   }
   
   if ( this->y_vector ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->y_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vector;
   }
   
   if ( this->z_vector ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->z_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->z_vector;
   }
   
   if ( this->x_contours ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->x_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contours;
   }
   
   if ( this->y_contours ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->y_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contours;
   }
   
   if ( this->z_contours ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->z_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contours;
   }
   
   if ( this->x_momentums ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->x_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentums;
   }
   
   if ( this->y_momentums ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->y_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentums;
   }
   
   if ( this->z_momentums ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->z_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentums;
   }
   
   if ( this->x_vectors ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->x_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vectors;
   }
   
   if ( this->y_vectors ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->y_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vectors;
   }
   
   if ( this->z_vectors ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->z_vectors" << std::endl << vprDEBUG_FLUSH; 
      delete this->z_vectors;
   }
   
   if ( this->streamlines ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->streamlines" << std::endl << vprDEBUG_FLUSH;
      delete this->streamlines;
      
      // Delete the polydata array used for seed points
      // if ( this->lastSource != NULL )
      //{
      //   this->lastSource->Delete();
      //}
   }
   
   if ( this->particles ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->particles" << std::endl << vprDEBUG_FLUSH;
      delete this->particles;
   }
   
   if ( this->surface ) 
   {
      //vprDEBUG(vesDBG,2) 
      //<< "deleting this->surface" << std::endl << vprDEBUG_FLUSH;
      delete this->surface;
   }
   
   if ( this->image ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->image" << std::endl << vprDEBUG_FLUSH;
      delete this->image;
   }
   
   if ( this->animImg ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->animImg" << std::endl << vprDEBUG_FLUSH;
      delete this->animImg;
   }
   
   if ( this->animStreamer ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->animStreamer" << std::endl << vprDEBUG_FLUSH;
      delete this->animStreamer;
   }
   
   if ( this->textOutput ) 
   {
      //vprDEBUG(vesDBG,2)  
      //<< "deleting this->textOutput" << std::endl << vprDEBUG_FLUSH;
      delete this->textOutput;
   }
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler& CreateVisObjectEventHandler::operator=(const CreateVisObjectEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CreateVisObjectEventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
/*   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< VE_Xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to AddVTKDataSetEventHandler::SetGlobalBaseObject!"<<std::endl;
   }*/
}
//////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   // Set the active dataset
   this->SetActiveDataSet( xmlObject );
   // set the active scalar and range
   this->SetActiveScalarAndRange( xmlObject );
   // set the active vector
   this->SetActiveVector( xmlObject );
   // Get the active object
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePairWeakPtr scalarDVP = command->GetDataValuePair( "Scalar Bar State" );
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Sub-Dialog Settings" );
   VE_XML::Command* objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );
   
   std::string direction;
   VE_XML::DataValuePairWeakPtr directionDVP = objectCommand->GetDataValuePair( "Direction" );
   if ( directionDVP )
   {
      directionDVP->GetData( direction );
   }
   
   std::string planes;
   VE_XML::DataValuePairWeakPtr planesDVP = objectCommand->GetDataValuePair( "Number of Planes" );
   if ( planesDVP )
   {
      planesDVP->GetData( planes );      
   }
   
   std::string advanced;
   VE_XML::DataValuePairWeakPtr advancedDVP = objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
   if ( advancedDVP )
   {
      VE_XML::Command* advancedCommand = dynamic_cast< VE_XML::Command* >( advancedDVP->GetDataXMLObject() );
      unsigned int warpOption = 0;
      advancedCommand->GetDataValuePair( "Warp Option" )->GetData( warpOption );
      if ( warpOption )
         advanced = "-warp";
   }
   
   //Create the key for a specific object
   std::pair< std::string, std::pair< std::string, std::string > > commandType;
   commandType = std::make_pair( std::string( objectCommand->GetCommandName() ), 
                                std::make_pair( direction, planes+advanced ) );

   // set the xml command to the cfdObject
   this->activeObject = visObjectMap[ commandType ];
   if ( activeObject == 0 )
   {
      std::cerr << "ERROR: selected vis option is not in the CreateVisObjectEventHandler. " << std::endl;
      return;  
   }

   unsigned int scalarBarState = 0;
   if ( scalarDVP )
   {
      scalarDVP->GetData( scalarBarState );
      cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->SetDataSetScalarState( scalarBarState );
   }

   // Check to see if any of the objectss need updated before we
   // create actors
   if ( cfdModelHandler::instance()->GetActiveModel() && activeObject)
   {
      activeObject->SetActiveDataSet( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
      activeObject->SetVECommand( cfdModelHandler::instance()->GetXMLCommand() );
      activeObject->UpdateCommand();
      cfdModelHandler::instance()->GetActiveModel()->GetModelCADHandler()->MakeCADRootTransparent();
   }
   
   // get the active vis object
   vprDEBUG(vesDBG,1) << " setting viz object " << activeObject->GetObjectType()
                        << " to _activeObject"
                        << std::endl << vprDEBUG_FLUSH;

   //SceneManager::instance()->GetRootNode()->AddChild( textOutput->add_text( "executing..." ) );

	osg::ref_ptr< VE_SceneGraph::DCS > activeDataSetDCS = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetDCS();

   // add active dataset DCS to scene graph if not already there...
   vprDEBUG(vesDBG,1) << " setting DCS to activeDCS = "
                        << activeDataSetDCS.get()
                        << std::endl << vprDEBUG_FLUSH;
   this->activeObject->SetActiveDataSet( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
   //this->activeObject->SetNormal( cfdEnvironmentHandler::instance()->GetNavigate()->GetDirection() );
   //this->activeObject->SetOrigin( cfdEnvironmentHandler::instance()->GetNavigate()->GetObjLocation() );
   this->activeObject->SetCursorType( cfdEnvironmentHandler::instance()->GetCursor()->GetCursorID() );
   activeObject->SetUpdateFlag( false );
   
   //call back over to ssvishandler to set the flags
   cfdSteadyStateVizHandler::instance()->SetActiveVisObject( activeObject );
   cfdSteadyStateVizHandler::instance()->SetComputeActorsAndGeodes( true );
   cfdSteadyStateVizHandler::instance()->SetActorsAreReady( true );
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveVector( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Active Vector" );
   std::string activeVector;
   activeModelDVP->GetData( activeVector );
   
   vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveVector Setting Active Vector = " << activeVector
      << std::endl << vprDEBUG_FLUSH;
   
   cfdModel* activeModel = cfdModelHandler::instance()->GetActiveModel();
   cfdDataSet* activeDataset = activeModel->GetActiveDataSet();
   // need to set the vector by name
   activeDataset->SetActiveVector( activeVector );
   //activeDataset->GetParent()->SetActiveVector( vectorIndex );
#ifdef _OSG
   /*cfdTextureDataSet* activeTDSet = activeModel->GetActiveTextureDataSet();
   if ( activeTDSet )
   {
      activeTDSet->SetActiveVector( activeVector );
   }*/
#endif
}
//////////////////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetActiveScalarAndRange( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );

   std::string activeScalarName;
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Active Scalar" );
   activeModelDVP->GetData( activeScalarName );   
   double scalarMin;
   activeModelDVP = command->GetDataValuePair( "Scalar Min" );
   activeModelDVP->GetData( scalarMin );
   double scalarMax;
   activeModelDVP = command->GetDataValuePair( "Scalar Max" );
   activeModelDVP->GetData( scalarMax );

   vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveScalarAndRange Set the scalar and range "
      << ", scalar = " << activeScalarName
      << ", min = " << scalarMin
      << ", max = " << scalarMax
      << std::endl << vprDEBUG_FLUSH;
   cfdDataSet* activeDataset = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
   //update active scalar texture if it exists
   
   activeDataset->SetActiveScalar( activeScalarName );
   activeDataset->GetParent()->SetActiveScalar( activeScalarName );
   
   activeDataset->ResetScalarBarRange( scalarMin, scalarMax );
   activeDataset->GetParent()->ResetScalarBarRange( scalarMin, scalarMax );
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveDataSet( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePairWeakPtr activeModelDVP = command->GetDataValuePair( "Active Dataset" );
   std::string dataSetName;
   activeModelDVP->GetData( dataSetName );

   //Need to set the active datasetname and get the position of the dataset
   cfdModel* activeModel = cfdModelHandler::instance()->GetActiveModel();
   unsigned int i = activeModel->GetIndexOfDataSet( dataSetName );
      vprDEBUG(vesDBG,1) 
         << "|\tCreateVisObjectEventHandler CHANGE_STEADYSTATE_DATASET " << i 
         << std::endl << vprDEBUG_FLUSH;
   //update active texture dataset if it exists
#ifdef _OSG
   unsigned int nTextureDataSets = activeModel->GetNumberOfTextureDataSets();
   if( (nTextureDataSets) && ( i < nTextureDataSets ) )
   {
      cfdTextureDataSet* activeTDSet = activeModel->GetTextureDataSet( i );
      activeModel->SetActiveTextureDataSet( activeTDSet );
   }
#endif
   if ( ( i < activeModel->GetNumberOfCfdDataSets() ) )
   {
      vprDEBUG(vesDBG,0) << "|\tCreateVisObjectEventHandler::SetActiveDataSet dataset = "
      << activeModel->GetCfdDataSet( i )->GetFileName()
      << ", dcs = " << activeModel->GetCfdDataSet( i )->GetDCS()
      << std::endl << vprDEBUG_FLUSH;
      
      int cfdType = activeModel->GetCfdDataSet( i )->GetType();
      vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveDataSet cfdType: " << cfdType
         << std::endl << vprDEBUG_FLUSH;
      
      // set the dataset as the appropriate dastaset type
      // (and the active dataset as well)
      cfdDataSet* activeDataset = activeModel->GetCfdDataSet( i );         
      
      std::string oldDatasetName = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetFileName();
      vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveDataSet last active dataset name = " 
         << oldDatasetName
         << std::endl << vprDEBUG_FLUSH;
      
      activeModel->SetActiveDataSet( activeDataset );         
      vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveDataSet Activating steady state file " 
         << activeDataset->GetFileName()
         << std::endl << vprDEBUG_FLUSH;
      
      // make sure that the user did not just hit same dataset button
      // (or change scalar since that is routed through here too)
      if ( oldDatasetName == activeDataset->GetFileName() )//if ( strcmp( oldDatasetName, activeDataset->GetFileName() ) )
      {
         vprDEBUG(vesDBG,1) << "|\tCreateVisObjectEventHandler::SetActiveDataSet  setting dataset as newly activated" 
         << std::endl << vprDEBUG_FLUSH;
         activeDataset->SetNewlyActivated();
         oldDatasetName.assign( activeDataset->GetFileName() );//strcpy( oldDatasetName, activeDataset->GetFileName() );
      }
      
      // Set the current active dataset for the scalar bar
      // so that it knows how to update itself
      //_scalarBar->SetActiveDataSet( activeDataset );
   }
   else
   {
      std::cerr << "ERROR: CreateVisObjectEventHandler::SetActiveDataSet  requested steady state dataset " 
      //<< commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
      << activeModel->GetNumberOfCfdDataSets()
      << std::endl;
   }
}
