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
 * File:          $RCSfile: cfdVEBaseClass.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdVEBaseClass.h"

#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdGroup.h"

#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdSoundHandler.h"
#include "VE_Xplorer/cfdObjects.h"
#include "VE_Xplorer/CADAddNodeEH.h"
#include "VE_Xplorer/AddVTKDataSetEventHandler.h"

#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"

#include <fstream>
#include <sstream>

#include "VE_Xplorer/cfdDebug.h"

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;
//////////////////////////////////////////////////////////////////      
// Constructor
cfdVEBaseClass::cfdVEBaseClass( void ) 
{
   xmlModel = 0;
   _onSceneGraph = false;
   _network.empty();
}
//////////////////////////////////////////////////////////////////      
// Destructor
cfdVEBaseClass::~cfdVEBaseClass( void )
{
   //delete this->dataRepresentation;
   if ( xmlModel )
   {
      delete xmlModel;
   }
}
//////////////////////////////////////////////////////////////////      
void cfdVEBaseClass::InitializeNode( VE_SceneGraph::cfdDCS* veworldDCS )
{
   //this->groupNode = new cfdGroup();
   this->_dcs = new VE_SceneGraph::cfdDCS(); 
   this->_dcs->SetName("cfdVEBaseClass");
   //this->dataRepresentation = new cfdObjects();
   //this->geometryNode = new cfdModuleGeometry( groupNode );
   this->worldDCS = veworldDCS;
   this->_model = new cfdModel( _dcs );
   //this->_readParam = new cfdReadParam();
}
//////////////////////////////////////////////////////////////////      
// Methods to do scene graph manipulations
// New methods may have to be added later
void cfdVEBaseClass::AddSelfToSG( void )
{
   _onSceneGraph = true;
   this->worldDCS->AddChild( this->_dcs );
}
//////////////////////////////////////////////////////////////////      
void cfdVEBaseClass::RemoveSelfFromSG( void )
{
   _onSceneGraph = false;
   this->worldDCS->RemoveChild( this->_dcs );
}
//////////////////////////////////////////////////////////////////      
// Change state information for geometric representation
void cfdVEBaseClass::MakeTransparent( void )
{
   ;
}
//////////////////////////////////////////////////////////////////      
void cfdVEBaseClass::SetColor( double* color )
{
   ;
}
//////////////////////////////////////////////////////////////////      
// transform object based 
void cfdVEBaseClass::SetTransforms( float* scale, float* rot, float* trans)
{
   this->_dcs->SetTranslationArray( trans );
   this->_dcs->SetScaleArray( scale );
   this->_dcs->SetRotationArray( rot );
}
//////////////////////////////////////////////////////////////////      
// Implement Gengxun's work by using socket
// stuff from vtk. This will be used in parallel
// with implementation of a unit connected to the 
// computational engine.
void cfdVEBaseClass::GetDataFromUnit( void )
{
   // Need to get Gengxun's work
   /*std::cout << "this->cfdId = " << geodeEnumToString(this->cfdId) << std::endl;
   this-> sock = vtkSocketCommunicator::New();
   this-> sock->WaitForConnection(33000);

   std::cout << "[DBG] VE_Xplorer is connected to the port 33000 "<< std::endl;
   

   vprDEBUG(vesDBG,1)
         <<" UPDATE_INTERACTIVE_DESIGN " << this->Interactive_state;
   
   vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
   
   if (!this->sock->Receive(ugrid,1,9))
   {
      std::cerr << " cfdCalculator side error :: Error receiving data." << std::endl;
      if (this->sock)
      {
         this->sock->CloseConnection();
         this->sock->Delete();
         this->sock = NULL;

      }

      ugrid->Delete();

   }

   std::cout << "[DBG] Receiving ugrid data..." << std::endl;
   
   
   
   if( this -> sock)
   {
      std::cout << "[DBG] testing if the sock is still connected" << std::endl;
      this->sock->CloseConnection();
      this->sock->Delete();
      this->sock = NULL;

   }*/
}

// Basically uses vtkActorToPF to create a geode and 
// add it to the scene graph. Probably use cfdObject.
void cfdVEBaseClass::MakeGeodeByUserRequest( int )
{
   //this->dataRepresentation->UpdatecfdGeode();
}

//This returns the name of the module
std::string cfdVEBaseClass::GetName( void )
{
   return this->_objectName;
}
/////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetObjectName( std::string input )
{
   _objectName = input;
}
//This returns the description of the module, This should be a short description
std::string cfdVEBaseClass::GetDesc( void )
{
   return this->_objectDescription;
}

// Set the pointer to the cursor class so that dynamic
// objects can do custom features with the wand input
void cfdVEBaseClass::SetCursor( cfdCursor* input )
{
   if ( input != NULL )
   {
      _cursor = input;
   }
   else
   {
      std::cerr << " ERROR : cfdVEBaseClass::SetCursor input is NULL "
                << std::endl;
   }
}

// Set the pointer to the navigate class so that dynamic
// objects can do custom features with the wand buttons
void cfdVEBaseClass::SetNavigate( cfdNavigate* input )
{
   if ( input != NULL )
   {
      _navigate = input;
   }
   else
   {
      std::cerr << " ERROR : cfdVEBaseClass::SetNavigate input is NULL "
                << std::endl;
   }
}
//////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetSoundHandler( cfdSoundHandler* input )
{
   if ( input )
   {
      soundHandler = input;
   }
   else
   {
      std::cerr << " ERROR : cfdVEBaseClass::SetSoundHandler input is NULL "
                << std::endl;
   }
}
//////////////////////////////////////////////////////////////////////
// Set the results for a particluar module so that we can use them for custom 
// viz features
void cfdVEBaseClass::SetModuleResults( const std::string network )
{
   if ( network.empty() )// == NULL )
   {
      std::cout << " No results for " << _objectName << std::endl;
   }
   else
   {
      if ( _network.empty() )// != NULL )
         //delete _network;
      //_network = new char[ strlen( network ) + 1 ];
      _network.assign( network );//strcpy( _network, network );
   }
  
   /*Package p;
   p.SetSysId("veresult.xml");
   p.Load(_network.c_str(), strlen(_network.c_str()));

   this->UnPackResult(&p.intfs[0]);
   //delete _network;
   _network.erase();// = NULL;*/
}
//////////////////////////////////////////////////////////////////////
// Viz feature for the devloper to define
// Can be anything that creates a geode
void cfdVEBaseClass::CreateCustomVizFeature( int input )
{
   // Do nothing
   // Implement for each module
}
// Set the id for a particular module
//////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetID(int id)
{
   _modID = id;
}
//////////////////////////////////////////////////////////////////////
cfdModel* cfdVEBaseClass::GetCFDModel( void )
{
   return _model;
}
//////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::CreateObjects( void )
{
   int numObjects;
   char text[ 256 ];
   char textLine[ 256 ];

   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,1) << " Number of Obejcts in Interactive Geometry : "
                          << numObjects << std::endl  << vprDEBUG_FLUSH;
   for ( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vesDBG,1) << "Id of object in Interactive Geometry : "
                             << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 9 ) // if it is an geom file
      {
         char fileName[100];
         float stlColor[3];
         int color;
         int transFlag;

         input >> transFlag;
         input.getline( textLine, 256 );   //skip past remainder of line
         vprDEBUG(vesDBG,0) << " geometry transparency flag = "
                                << transFlag
                                << std::endl << vprDEBUG_FLUSH;

         // read color flag
         input >> color;
         vprDEBUG(vesDBG,0) << " stl color flag = " << color
                                << std::endl << vprDEBUG_FLUSH;

         // read color if color flag = 1
         if( color == 1)
         {
            for(int i=0;i<3;i++)
            {
               input >> stlColor[ i ];
            }
            vprDEBUG(vesDBG,0) << "\tcolor: " << stlColor[ 0 ] << " : "
                                   << stlColor[ 1 ] << " : "
                                   << stlColor[ 2 ]
                                   << std::endl << vprDEBUG_FLUSH;
         }
         input.getline( textLine, 256 );   //skip past remainder of line

         vprDEBUG(vesDBG,0) << " geometry DCS parameters:" 
                                << std::endl << vprDEBUG_FLUSH;
         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         input >> fileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         int test1 = fileIO::isFileReadable( fileName );
         if ( test1 == 1 )
         { 
            vprDEBUG(vesDBG,0) << " geometry fileName = " << fileName
                                   << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable geometry file = " << fileName 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit(1);
         }

         vprDEBUG(vesDBG,0) << " scale = " << scale[0] << " : "
                                << scale[1] << " : " << scale[2]
                                << std::endl << vprDEBUG_FLUSH;

         _model->CreateGeomDataSet( fileName );
         _model->GetGeomDataSet( -1 )->GetDCS()->SetScaleArray( scale );
         _model->GetGeomDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
         _model->GetGeomDataSet( -1 )->GetDCS()->SetRotationArray( rotate );
         _model->GetGeomDataSet( -1 )->SetFILEProperties( color, transFlag, stlColor );
         _model->GetGeomDataSet( -1 )->setOpac( 1 );
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}
//////////////////////////////////////////////////////////////////   
VE_SceneGraph::cfdDCS* cfdVEBaseClass::GetWorldDCS()
{
   return this->worldDCS;
}
//////////////////////////////////////////////////////////////////   
void cfdVEBaseClass::SetXMLModel( VE_Model::Model* tempModel )
{
   xmlModel = tempModel;

   //Decompose model to be utilized by the event handlers
   VE_CAD::CADAssembly* cadNodeData = dynamic_cast< VE_CAD::CADAssembly* >( xmlModel->GetGeometry() );
   if ( cadNodeData )
   {
      VE_XML::DataValuePair* cadNode = new VE_XML::DataValuePair();
      cadNode->SetDataType( std::string("XMLOBJECT") );
      cadNode->SetData("New Node", cadNodeData );

      VE_XML::Command* cadCommand = new VE_XML::Command();
      cadCommand->AddDataValuePair( cadNode );
      std::string _commandName = "CAD_ADD_NODE";
      cadCommand->SetCommandName( _commandName );

      //Process the cad
      VE_EVENTS::CADAddNodeEventHandler newCADNode;
      newCADNode.SetGlobalBaseObject( _model );
      newCADNode.Execute( cadCommand );
      delete cadCommand;
   }

   //process the information blocks
   if ( xmlModel->GetNumberOfInformationPackets() > 0 )
   {
      VE_XML::DataValuePair* modelNode = new VE_XML::DataValuePair();
      modelNode->SetDataType( std::string("XMLOBJECT") );
      modelNode->SetData("CREATE_NEW_DATASETS", xmlModel );
      
      VE_XML::Command* dataCommand = new VE_XML::Command();
      dataCommand->AddDataValuePair( modelNode );
      dataCommand->SetCommandName(  "UPDATE_MODEL_DATASETS" );
      
      //Process the vtk data
      VE_EVENTS::AddVTKDataSetEventHandler addVTKEH;
      addVTKEH.SetGlobalBaseObject( _model );
      addVTKEH.Execute( dataCommand );
      delete dataCommand;
   }

   //process inputs
   if ( xmlModel->GetNumberOfInputs() > 0 )
   {
      //do something
   }
   
   //process results
   if ( xmlModel->GetNumberOfResults() > 0 )
   {
      //do something
   }
}
