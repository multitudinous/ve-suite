/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/CommandHandler.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/event/cad/CADAddNodeEH.h>
#include <ves/xplorer/event/data/AddVTKDataSetEventHandler.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/cad/CADAssembly.h>

// --- vrJuggler Includes --- //
#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAL/SoundManager>
#endif

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
PluginBase::PluginBase():
        mOnSceneGraph( false ),
        mCursor( 0 ),
        mDevice( 0 ),
        mModel( 0 ),
        mPhysicsSimulator( 0 ),
        mEnvironmentHandler( 0 ),
        mSceneManager( 0 ),
        mResourceManager( 0 ),
        mCommandHandler( 0 ),
#ifdef VE_SOUND
        mSoundManager( 0 ),
#endif
        mDCS( 0 ),
        mWorldDCS( 0 ),
        mXmlModel( ves::open::xml::model::ModelPtr() )
{
    //This needs to match the name of the gui plugin.
    //It is used in cfdVEPluginLoader::CreateObject. The name used is the name
    //in the ves::open::xml::Model model name. This name comes from the gui
    //plugin.
    mObjectName = "Please set mObjectName in your plugin.";
}
////////////////////////////////////////////////////////////////////////////////
PluginBase::~PluginBase()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::InitializeNode( osg::Group* veworldDCS )
{
    mDCS = new ves::xplorer::scenegraph::DCS();
    mDCS->SetName( "PluginBase" );
    mWorldDCS = veworldDCS;
    AddSelfToSG();
    mModel = new Model( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::AddSelfToSG()
{
    if( mOnSceneGraph )
    {
        return;
    }
    
    mOnSceneGraph = true;
    mWorldDCS->addChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::RemoveSelfFromSG()
{
    mOnSceneGraph = false;
    mWorldDCS->removeChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::GetDataFromUnit()
{
    /*
    //Need to get Gengxun's work
    std::cout << "cfdId = " << geodeEnumToString( cfdId ) << std::endl;
    sock = vtkSocketCommunicator::New();
    sock->WaitForConnection( 33000 );

    std::cout << "[DBG] VE_Xplorer is connected to the port 33000 "
              << std::endl;

    vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();

    if( !sock->Receive( ugrid, 1, 9 ) )
    {
        std::cerr << " cfdCalculator side error :: Error receiving data."
                  << std::endl;
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
        std::cout << "[DBG] testing if the sock is still connected"
                  << std::endl;
        sock->CloseConnection();
        sock->Delete();
        sock = NULL;
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
const std::string& PluginBase::GetName()
{
    return mObjectName;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetObjectName( const std::string& input )
{
    mObjectName = input;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& PluginBase::GetDesc()
{
    return mObjectDescription;
}
////////////////////////////////////////////////////////////////////////////////
bool PluginBase::OnSceneGraph()
{
    return mOnSceneGraph;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetCursor( cfdCursor* cursor )
{
    if( cursor != NULL )
    {
        mCursor = cursor;
    }
    else
    {
        std::cerr << " ERROR : PluginBase::SetCursor cursor is NULL "
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetSceneManager(
    ves::xplorer::scenegraph::SceneManager* sceneManager )
{
    mSceneManager = sceneManager;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetEnvironmentHandler(
    ves::xplorer::EnvironmentHandler* environmentHandler )
{
    mEnvironmentHandler = environmentHandler;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetResourceManager(
    ves::xplorer::scenegraph::ResourceManager* resourceManager )
{
    mResourceManager = resourceManager;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetCommandHandler(
    ves::xplorer::CommandHandler* commandHandler )
{
    mCommandHandler = commandHandler;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetInteractionDevice( ves::xplorer::Device* device )
{
    mDevice = device;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetPhysicsSimulator(
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
{
    mPhysicsSimulator = physicsSimulator;
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_SOUND
void PluginBase::SetSoundManager( osgAL::SoundManager* soundManager )
{
    mSoundManager = soundManager;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetModuleResults( const std::string& network )
{
    if( network.empty() || network == "NULL" )
    {
        std::cout << "|\tNo results for " << mObjectName << std::endl;

        return;
    }
    //add inputs to xml model
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( network, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    if( objectVector.empty() )
    {
        std::cerr << "|\tBad command sent to graphical plugin : "
                  << network << std::endl;
        return;
    }

    ves::open::xml::CommandPtr tempCommand = 
        boost::dynamic_pointer_cast< ves::open::xml::Command >( 
            objectVector.at( 0 ) );
    size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVP; ++i )
    {
        ves::open::xml::DataValuePairPtr tempPair =
            tempCommand->GetDataValuePair( i );
        ves::open::xml::CommandPtr copyCommand =
            boost::dynamic_pointer_cast< ves::open::xml::Command >(
                tempPair->GetDataXMLObject() );
        mXmlModel->SetResult( copyCommand );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::CreateCustomVizFeature( int input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SelectedPreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Model* PluginBase::GetCFDModel()
{
    return mModel;
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetXMLModel( ves::open::xml::model::ModelPtr& tempModel )
{
    mXmlModel = tempModel;

    {
        bool hasID = false;
        osg::Node::DescriptionList descriptList;
        descriptList = mDCS->getDescriptions();
        for( size_t i = 0; i < descriptList.size(); ++i )
        {
            if( descriptList.at( i ) == "VE_XPLORER_PLUGIN_ID" )
            {
                //descriptList.at( i + 1 ) = mXmlModel->GetID();
                //mDCS->setDescriptions( descriptList );
                hasID = true;
                break;
            }
        }
        
        if( !hasID )
        {
            //Add this descriptor so that the whole "plugin" 
            //can be turned "off" easily
            mDCS->addDescription( "VE_XPLORER_PLUGIN_ID" );   
            mDCS->addDescription( mXmlModel->GetID() );   
        }
    }

    //Decompose model to be utilized by the event handlers
    ves::open::xml::cad::CADAssemblyPtr cadNodeData =
        boost::dynamic_pointer_cast< ves::open::xml::cad::CADAssembly >(
            mXmlModel->GetGeometry() );
    if( cadNodeData )
    {
        ves::open::xml::DataValuePairPtr cadNode( 
            new ves::open::xml::DataValuePair() );
        cadNode->SetDataType( std::string( "XMLOBJECT" ) );
        cadNode->SetData( "New Node", cadNodeData );

        ves::open::xml::CommandPtr cadCommand( new ves::open::xml::Command() );
        cadCommand->AddDataValuePair( cadNode );
        std::string _commandName = "CAD_ADD_NODE";
        cadCommand->SetCommandName( _commandName );

        //Process the cad
        ves::xplorer::event::CADAddNodeEventHandler newCADNode;
        newCADNode.SetGlobalBaseObject( mModel );
        newCADNode.Execute( cadCommand );
    }

    //process the information blocks
    if( mXmlModel->GetNumberOfInformationPackets() > 0 )
    {
        ves::open::xml::DataValuePairPtr modelNode( 
            new ves::open::xml::DataValuePair() );
        modelNode->SetDataType( std::string( "XMLOBJECT" ) );
        modelNode->SetData( "CREATE_NEW_DATASETS",
                            ves::open::xml::model::ModelPtr( 
                            new ves::open::xml::model::Model( *mXmlModel ) ) );

        ves::open::xml::CommandPtr dataCommand( new ves::open::xml::Command() );
        dataCommand->AddDataValuePair( modelNode );
        dataCommand->SetCommandName( "UPDATE_MODEL_DATASETS" );

        //Add the active dataset name to the command
        ves::open::xml::ParameterBlockPtr parameterBlock =
            mXmlModel->GetInformationPacket( 0 );
        ves::open::xml::DataValuePairPtr dataSetName(
                     new ves::open::xml::DataValuePair() );
        dataSetName->SetData( "VTK_DATASET_NAME",
            parameterBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
        dataCommand->AddDataValuePair( dataSetName );

        //Process the vtk data
        ves::xplorer::event::AddVTKDataSetEventHandler addVTKEH;
        addVTKEH.SetGlobalBaseObject( mModel );
        addVTKEH.Execute( dataCommand );
    }

    //process inputs
    /*if( mXmlModel->GetNumberOfInputs() > 0 )
    {
        ;
    }

    //process results
    if( mXmlModel->GetNumberOfResults() > 0 )
    {
        ;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( command )
    {
        vprDEBUG( vesDBG, 4 ) << command->GetCommandName() << std::endl
                              << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginBase::ProcessOnSubmitJob()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, PluginBase* > PluginBase::GetCommandNameMap()
{
    return mEventHandlerMap;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* PluginBase::GetPluginDCS()
{
    return mDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
} // end plugin
} // end xplorer
} // end ves
