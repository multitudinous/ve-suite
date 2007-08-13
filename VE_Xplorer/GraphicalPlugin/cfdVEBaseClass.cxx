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

#include "VE_Xplorer/GraphicalPlugin/cfdVEBaseClass.h"

#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdSoundHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"
#include "VE_Xplorer/XplorerHandlers/CADAddNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/AddVTKDataSetEventHandler.h"

#include "VE_Xplorer/Utilities/fileIO.h"

#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"

#include <fstream>
#include <sstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;
//////////////////////////////////////////////////////////////////      
// Constructor
cfdVEBaseClass::cfdVEBaseClass( void ):
xmlModel( 0 ),
_onSceneGraph( false )
{
   _network.empty();
}
//////////////////////////////////////////////////////////////////      
// Destructor
cfdVEBaseClass::~cfdVEBaseClass( void )
{
    std::cout << "cfdVEBaseClass deleted" <<std::endl;
   if ( xmlModel )
   {
      delete xmlModel;
   }
}
//////////////////////////////////////////////////////////////////      
void cfdVEBaseClass::InitializeNode( VE_SceneGraph::DCS* veworldDCS )
{
   //this->groupNode = new VE_SceneGraph::Group();
   this->_dcs = new VE_SceneGraph::DCS(); 
   this->_dcs->SetName("cfdVEBaseClass");
   //this->dataRepresentation = new cfdObjects();
   //this->geometryNode = new cfdModuleGeometry( groupNode );
   this->worldDCS = veworldDCS;
   this->_model = new cfdModel( _dcs.get() );
   //this->_readParam = new cfdReadParam();
}
//////////////////////////////////////////////////////////////////      
// Methods to do scene graph manipulations
// New methods may have to be added later
void cfdVEBaseClass::AddSelfToSG( void )
{
   _onSceneGraph = true;
   this->worldDCS->AddChild( this->_dcs.get() );
}
//////////////////////////////////////////////////////////////////      
void cfdVEBaseClass::RemoveSelfFromSG( void )
{
   _onSceneGraph = false;
   this->worldDCS->RemoveChild( this->_dcs.get() );
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
void cfdVEBaseClass::SetTransforms( double* scale, double* rot, double* trans)
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
   if ( network.empty() || network == "NULL" )
   {
      std::cout << "|\tNo results for " << _objectName << std::endl;
      return;
   }
   //add inputs to xml model
   //
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( network, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();
   
   if ( objectVector.empty() )
   {
      std::cerr << "|\tBad command sent to graphical plugin : " << network << std::endl;
      return;
   }

   VE_XML::Command* tempCommand = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
   size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
   for ( size_t i = 0; i < numDVP; ++i )
   {
      VE_XML::Command* command = xmlModel->GetResult( i );
      VE_XML::DataValuePairWeakPtr tempPair = tempCommand->GetDataValuePair( i );
      VE_XML::Command* copyCommand = dynamic_cast< VE_XML::Command* >( tempPair->GetDataXMLObject() );
      *command = *copyCommand;
   }
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

//////////////////////////////////////////////////////////////////   
/*VE_SceneGraph::DCS* cfdVEBaseClass::GetWorldDCS()
{
   return this->worldDCS.get();
}*/
//////////////////////////////////////////////////////////////////   
void cfdVEBaseClass::SetXMLModel( VE_XML::VE_Model::Model* tempModel )
{
   xmlModel = tempModel;

   //Decompose model to be utilized by the event handlers
   VE_XML::VE_CAD::CADAssembly* cadNodeData = dynamic_cast< VE_XML::VE_CAD::CADAssembly* >( xmlModel->GetGeometry() );
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
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetCurrentCommand( VE_XML::Command* command )
{
   if(command)
   {
      vprDEBUG(vesDBG,4) << command->GetCommandName() << std::endl << vprDEBUG_FLUSH;
   }
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, cfdVEBaseClass* > cfdVEBaseClass::GetCommandNameMap( void )
{
   return ehMap;
}
