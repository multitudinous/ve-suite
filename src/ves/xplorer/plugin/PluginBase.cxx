/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

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
        m_onSceneGraph( false ),
        m_device( 0 ),
        m_physicsSimulator( 0 ),
        m_modID( -1 ),
        m_pos_x( 0 ),
        m_pos_y( 0 )
#ifdef VE_SOUND
        ,
        m_soundManager( 0 )
#endif
{
    m_xmlModel = ves::open::xml::model::ModelPtr();
    m_network.empty();
}
////////////////////////////////////////////////////////////////////////////////
cfdVEBaseClass::~cfdVEBaseClass()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    m_dcs = new ves::xplorer::scenegraph::DCS();
    m_dcs->SetName( "cfdVEBaseClass" );
    m_worldDCS = veworldDCS;
    m_model = new Model( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::AddSelfToSG()
{
    m_onSceneGraph = true;
    m_worldDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::RemoveSelfFromSG()
{
    m_onSceneGraph = false;
    m_worldDCS->RemoveChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetTransforms( double* scale, double* rot, double* trans )
{
    m_dcs->SetTranslationArray( trans );
    m_dcs->SetScaleArray( scale );
    m_dcs->SetRotationArray( rot );
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
const std::string& cfdVEBaseClass::GetName()
{
    return m_objectName;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetObjectName( const std::string& input )
{
    m_objectName = input;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& cfdVEBaseClass::GetDesc()
{
    return m_objectDescription;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetCursor( cfdCursor* input )
{
    if( input != NULL )
    {
        m_cursor = input;
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
        m_soundHandler = input;
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
void cfdVEBaseClass::SetModuleResults( const std::string& network )
{
    if( network.empty() || network == "NULL" )
    {
        std::cout << "|\tNo results for " << m_objectName << std::endl;

        return;
    }
    //add inputs to xml model
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( network, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    if( objectVector.empty() )
    {
        std::cerr << "|\tBad command sent to graphical plugin : " << network << std::endl;
        return;
    }

    ves::open::xml::CommandPtr tempCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVP; ++i )
    {
        ves::open::xml::CommandPtr command = m_xmlModel->GetResult( i );
        ves::open::xml::DataValuePairPtr tempPair = tempCommand->GetDataValuePair( i );
        ves::open::xml::CommandPtr copyCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>(  tempPair->GetDataXMLObject() );
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
    m_modID = id;
}
////////////////////////////////////////////////////////////////////////////////
Model* cfdVEBaseClass::GetCFDModel()
{
    return m_model;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetXMLModel( ves::open::xml::model::ModelPtr& tempModel )
{
    m_xmlModel = tempModel;

    //Decompose model to be utilized by the event handlers
    ves::open::xml::cad::CADAssemblyPtr cadNodeData = boost::dynamic_pointer_cast<ves::open::xml::cad::CADAssembly>( m_xmlModel->GetGeometry() );
    if( cadNodeData )
    {
        ves::open::xml::DataValuePairPtr cadNode( new ves::open::xml::DataValuePair() );
        cadNode->SetDataType( std::string( "XMLOBJECT" ) );
        cadNode->SetData( "New Node", cadNodeData );

        ves::open::xml::CommandPtr cadCommand( new ves::open::xml::Command() );
        cadCommand->AddDataValuePair( cadNode );
        std::string _commandName = "CAD_ADD_NODE";
        cadCommand->SetCommandName( _commandName );

        //Process the cad
        ves::xplorer::event::CADAddNodeEventHandler newCADNode;
        newCADNode.SetGlobalBaseObject( m_model );
        newCADNode.Execute( cadCommand );
    }

    //process the information blocks
    if( m_xmlModel->GetNumberOfInformationPackets() > 0 )
    {
        ves::open::xml::DataValuePairPtr modelNode( new ves::open::xml::DataValuePair() );
        modelNode->SetDataType( std::string( "XMLOBJECT" ) );
        modelNode->SetData( "CREATE_NEW_DATASETS",
                            ves::open::xml::model::ModelPtr( new ves::open::xml::model::Model( *m_xmlModel ) ) );

        ves::open::xml::CommandPtr dataCommand( new ves::open::xml::Command() );
        dataCommand->AddDataValuePair( modelNode );
        dataCommand->SetCommandName( "UPDATE_MODEL_DATASETS" );

        //Add the active dataset name to the command
        ves::open::xml::ParameterBlockPtr parameterBlock =
            m_xmlModel->GetInformationPacket( 0 );
        ves::open::xml::DataValuePairPtr dataSetName(
                     new ves::open::xml::DataValuePair() );
        dataSetName->SetData( "VTK_DATASET_NAME",
                              parameterBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
        dataCommand->AddDataValuePair( dataSetName );

        //Process the vtk data
        ves::xplorer::event::AddVTKDataSetEventHandler addVTKEH;
        addVTKEH.SetGlobalBaseObject( m_model );
        addVTKEH.Execute( dataCommand );
    }

    //process inputs
    if( m_xmlModel->GetNumberOfInputs() > 0 )
    {
        ;
    }

    //process results
    if( m_xmlModel->GetNumberOfResults() > 0 )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( command )
    {
        vprDEBUG( vesDBG, 4 ) << command->GetCommandName() << std::endl << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, cfdVEBaseClass* > cfdVEBaseClass::GetCommandNameMap()
{
    return m_ehMap;
}
////////////////////////////////////////////////////////////////////////////////

} // end plugin
} // end xplorer
} // end ves
