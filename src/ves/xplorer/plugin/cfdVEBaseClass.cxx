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
// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/environment/cfdSoundHandler.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/event/viz/cfdObjects.h>
#include <ves/xplorer/event/cad/CADAddNodeEH.h>
#include <ves/xplorer/event/data/AddVTKDataSetEventHandler.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAL/SoundManager>
#endif

// --- vrJuggler Includes --- //
#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

// --- C/C++ Libraries
#include <fstream>
#include <sstream>

namespace ves
{
namespace xplorer
{
namespace plugin
{

////////////////////////////////////////////////////////////////////////////////
cfdVEBaseClass::cfdVEBaseClass():
xmlModel( 0 ),
_onSceneGraph( false ),
m_device( 0 ),
m_physicsSimulator( 0 )
#ifdef VE_SOUND
,
m_soundManager( 0 )
#endif
{
    _network.empty();
}
////////////////////////////////////////////////////////////////////////////////
cfdVEBaseClass::~cfdVEBaseClass()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    //groupNode = new ves::xplorer::scenegraph::Group();
    _dcs = new ves::xplorer::scenegraph::DCS(); 
    _dcs->SetName( "cfdVEBaseClass" );
    //dataRepresentation = new cfdObjects();
    //geometryNode = new cfdModuleGeometry( groupNode );
    worldDCS = veworldDCS;
    _model = new Model( _dcs.get() );
    //_readParam = new cfdReadParam();
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::AddSelfToSG()
{
    _onSceneGraph = true;
    worldDCS->AddChild( _dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::RemoveSelfFromSG()
{
    _onSceneGraph = false;
    worldDCS->RemoveChild( _dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::MakeTransparent()
{
    ;
}
//////////////////////////////////////////////////////////////////////////////// 
void cfdVEBaseClass::SetColor( double* color )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetTransforms( double* scale, double* rot, double* trans )
{
    _dcs->SetTranslationArray( trans );
    _dcs->SetScaleArray( scale );
    _dcs->SetRotationArray( rot );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::GetDataFromUnit()
{
    //Need to get Gengxun's work
    /*std::cout << "cfdId = " << geodeEnumToString( cfdId ) << std::endl;
    sock = vtkSocketCommunicator::New();
    sock->WaitForConnection( 33000 );

    std::cout << "[DBG] VE_Xplorer is connected to the port 33000 "<< std::endl;

    vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();

    if( !sock->Receive( ugrid, 1, 9 ) )
    {
        std::cerr << " cfdCalculator side error :: Error receiving data." << std::endl;
        if( sock )
        {
            sock->CloseConnection();
            sock->Delete();
            sock = NULL;
        }

        ugrid->Delete();

    }

    std::cout << "[DBG] Receiving ugrid data..." << std::endl;



    if( this -> sock )
    {
        std::cout << "[DBG] testing if the sock is still connected" << std::endl;
        sock->CloseConnection();
        sock->Delete();
        sock = NULL;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::MakeGeodeByUserRequest( int )
{
    //dataRepresentation->UpdatecfdGeode();
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdVEBaseClass::GetName()
{
    return _objectName;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetObjectName( std::string input )
{
    _objectName = input;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdVEBaseClass::GetDesc()
{
    return _objectDescription;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetCursor( cfdCursor* input )
{
    if( input != NULL )
    {
        _cursor = input;
    }
    else
    {
        std::cerr << " ERROR : cfdVEBaseClass::SetCursor input is NULL "
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetInteractionDevice( ves::xplorer::Device* device )
{
    if( device != NULL )
    {
        m_device = device;
    }
    else
    {
        std::cerr << " ERROR : cfdVEBaseClass::SetNavigate input is NULL "
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetSoundHandler( cfdSoundHandler* input )
{
    if( input )
    {
        soundHandler = input;
    }
    else
    {
        std::cerr << " ERROR : cfdVEBaseClass::SetSoundHandler input is NULL "
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetPhysicsSimulator( ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
{
    if( physicsSimulator )
    {
        m_physicsSimulator = physicsSimulator;
    }
    else
    {
        std::cerr << " ERROR : cfdVEBaseClass::SetPhysicsSimulator physicsSimulator is NULL "
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_SOUND
void cfdVEBaseClass::SetSoundManager( osgAL::SoundManager* soundManager )
{
    if( soundManager )
    {
        m_soundManager = soundManager;
    }
    else
    {
        std::cerr << " ERROR : cfdVEBaseClass::SetSoundManager soundManager is NULL "
                  << std::endl;
    }
}
#endif
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetModuleResults( const std::string network )
{
    if( network.empty() || network == "NULL" )
    {
        std::cout << "|\tNo results for " << _objectName << std::endl;

        return;
    }
    //add inputs to xml model
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( network, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

    if( objectVector.empty() )
    {
        std::cerr << "|\tBad command sent to graphical plugin : " << network << std::endl;
        return;
    }

    ves::open::xml::Command* tempCommand = dynamic_cast< ves::open::xml::Command* >( objectVector.at( 0 ) );
    size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVP; ++i )
    {
        ves::open::xml::Command* command = xmlModel->GetResult( i );
        ves::open::xml::DataValuePairWeakPtr tempPair = tempCommand->GetDataValuePair( i );
        ves::open::xml::Command* copyCommand = dynamic_cast< ves::open::xml::Command* >( tempPair->GetDataXMLObject() );
        *command = *copyCommand;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::CreateCustomVizFeature( int input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetID( int id )
{
    _modID = id;
}
////////////////////////////////////////////////////////////////////////////////
Model* cfdVEBaseClass::GetCFDModel()
{
    return _model;
}
////////////////////////////////////////////////////////////////////////////////   
/*
ves::xplorer::scenegraph::DCS* cfdVEBaseClass::GetWorldDCS()
{
    return worldDCS.get();
}
*/
////////////////////////////////////////////////////////////////////////////////   
void cfdVEBaseClass::SetXMLModel( ves::open::xml::model::ModelWeakPtr tempModel )
{
    xmlModel = tempModel;

    //Decompose model to be utilized by the event handlers
    ves::open::xml::cad::CADAssembly* cadNodeData = 
        dynamic_cast< ves::open::xml::cad::CADAssembly* >( xmlModel->GetGeometry() );
    if( cadNodeData )
    {
        ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
        cadNode->SetDataType( std::string( "XMLOBJECT" ) );
        cadNode->SetData( "New Node", cadNodeData );

        ves::open::xml::Command* cadCommand = new ves::open::xml::Command();
        cadCommand->AddDataValuePair( cadNode );
        std::string _commandName = "CAD_ADD_NODE";
        cadCommand->SetCommandName( _commandName );

        //Process the cad
        ves::xplorer::event::CADAddNodeEventHandler newCADNode;
        newCADNode.SetGlobalBaseObject( _model );
        newCADNode.Execute( cadCommand );
        delete cadCommand;
    }

    //process the information blocks
    if( xmlModel->GetNumberOfInformationPackets() > 0 )
    {
        ves::open::xml::DataValuePairPtr modelNode = new ves::open::xml::DataValuePair();
        modelNode->SetDataType( std::string( "XMLOBJECT" ) );
        modelNode->SetData( "CREATE_NEW_DATASETS", 
            new ves::open::xml::model::Model( *xmlModel ) );

        ves::open::xml::Command* dataCommand = new ves::open::xml::Command();
        dataCommand->AddDataValuePair( modelNode );
        dataCommand->SetCommandName( "UPDATE_MODEL_DATASETS" );

        //Add the active dataset name to the command
        ves::open::xml::ParameterBlock* parameterBlock = 
            xmlModel->GetInformationPacket( 0 );
        ves::open::xml::DataValuePairPtr dataSetName = 
        new ves::open::xml::DataValuePair();
        dataSetName->SetData( "VTK_DATASET_NAME", 
            parameterBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
        dataCommand->AddDataValuePair( dataSetName );

        //Process the vtk data
        ves::xplorer::event::AddVTKDataSetEventHandler addVTKEH;
        addVTKEH.SetGlobalBaseObject( _model );
        addVTKEH.Execute( dataCommand );
        delete dataCommand;
    }

    //process inputs
    if( xmlModel->GetNumberOfInputs() > 0 )
    {
        ;
    }

    //process results
    if( xmlModel->GetNumberOfResults() > 0 )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetCurrentCommand( ves::open::xml::Command* command )
{
    if( command )
    {
        vprDEBUG( vesDBG, 4 ) << command->GetCommandName() << std::endl << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, cfdVEBaseClass* > cfdVEBaseClass::GetCommandNameMap()
{
    return ehMap;
}
////////////////////////////////////////////////////////////////////////////////

} // end plugin
} // end xplorer
} // end ves