/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/event/viz/VisFeatureMakerBase.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/data/DatasetPropertySet.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/event/viz/cfdObjects.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <iostream>

#include <boost/concept_check.hpp>

using namespace ves::xplorer::command;
using namespace ves::xplorer;
using namespace ves::xplorer::volume;

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::VisFeatureMakerBase()
    :
    m_logger( Poco::Logger::get("xplorer.VisFeatureMakerBase") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::VisFeatureMakerBase( const VisFeatureMakerBase& orig )
    :
    m_logger( Poco::Logger::get("xplorer.VisFeatureMakerBase") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    boost::ignore_unused_variable_warning( orig );
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::~VisFeatureMakerBase()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::Update( const std::string& recordUUID )
{
    boost::ignore_unused_variable_warning( recordUUID );
    // Does nothing, but don't want pure virtual f'n so that this class *can*
    // be instantiated alone.
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::UpdateAdvancedSettings( ves::xplorer::data::PropertySet& set )
{
    boost::ignore_unused_variable_warning( set );
    // Does nothing, but don't want pure virtual f'n so that this class *can*
    // be instantiated alone.
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::UpdateBaseInformation( xplorer::data::PropertySet& set )
{
    m_vistabBaseInformation.clear();
    m_commandName.clear();
    ///This is the default. Other dialogs actions will set the command name to the specific value if they are launched
    m_commandName = "VISUALIZATION_SETTINGS";

    ves::open::xml::DataValuePairPtr activeScalar( new ves::open::xml::DataValuePair() );
    activeScalar->SetDataType( "STRING" );
    activeScalar->SetDataName( std::string( "Active Scalar" ) );
    activeScalar->SetDataString( boost::any_cast<std::string >
    ( set.GetPropertyAttribute( "DataSet_ScalarData", "enumCurrentString" ) ) );
    m_vistabBaseInformation.push_back( activeScalar );

    ves::open::xml::DataValuePairPtr activeVector( new ves::open::xml::DataValuePair() );
    activeVector->SetDataType( "STRING" );
    activeVector->SetDataName( std::string( "Active Vector" ) );
    activeVector->SetDataString( boost::any_cast<std::string >
    ( set.GetPropertyAttribute( "DataSet_VectorData", "enumCurrentString" ) ) );
    m_vistabBaseInformation.push_back( activeVector );

    ves::open::xml::DataValuePairPtr activeDataset( new ves::open::xml::DataValuePair() );
    activeDataset->SetDataType( "STRING" );
    activeDataset->SetDataName( std::string( "Active Dataset" ) );
    activeDataset->SetDataString( boost::any_cast<std::string >
    ( set.GetPropertyAttribute( "DataSet", "enumCurrentString" ) ) );
    m_vistabBaseInformation.push_back( activeDataset );
    
    ves::open::xml::DataValuePairPtr scalarMin( new ves::open::xml::DataValuePair() );
    double minimumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Min" ) );
    scalarMin->SetData( "Scalar Min", minimumValue );
    m_vistabBaseInformation.push_back( scalarMin );

    ves::open::xml::DataValuePairPtr scalarMax( new ves::open::xml::DataValuePair() );
    double maximumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Max" ) );
    scalarMax->SetData( "Scalar Max", maximumValue );
    m_vistabBaseInformation.push_back( scalarMax );

    // Load instance of selected DataSet from database and get relevant properties
    // from it.
    xplorer::data::DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", boost::any_cast<std::string >
                       ( set.GetPropertyAttribute
                       ( "DataSet", "enumCurrentString" )
                       ) );

    //Bounding box display value
    ves::open::xml::DataValuePairPtr bbox( new ves::open::xml::DataValuePair() );
    bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > (
                   boost::any_cast<bool>( dataset.GetPropertyValue( "BoundingBox" ) )
                   ) );
    //bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( bbox );

    //Surface wrap display value
    ves::open::xml::DataValuePairPtr wireMesh( new ves::open::xml::DataValuePair() );
    wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > (
                       boost::any_cast<bool>( dataset.GetPropertyValue( "SurfaceWrap" ) )
                       ) );
    //wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( wireMesh );

    //set scalar bar state
    ves::open::xml::DataValuePairPtr scalarBarDVP( new ves::open::xml::DataValuePair() );
    scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > (
                           boost::any_cast<bool>( dataset.GetPropertyValue( "ScalarBar" ) )
                           ) );
    //scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( scalarBarDVP );
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand,
                                                        xplorer::data::PropertySet& set )
{
    UpdateBaseInformation( set );
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );

    for( size_t i = 0; i < m_vistabBaseInformation.size(); ++i )
    {
        newCommand->AddDataValuePair( m_vistabBaseInformation.at( i ) );
    }
    if( subDialogCommand )
    {
        ves::open::xml::DataValuePairPtr subDialogSettings( new ves::open::xml::DataValuePair() );
        subDialogSettings->SetData( "Sub-Dialog Settings", subDialogCommand );
        newCommand->AddDataValuePair( subDialogSettings );
    }
    newCommand->SetCommandName( m_commandName );
    newCommand->SetID( set.GetUUIDAsString() );
    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( newCommand );
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::Execute( xplorer::data::PropertySetPtr set )
{
    //Sleep here while we wait for the ss viz handler to finish computing the
    //last request from the user
    //while(  SteadyStateVizHandler::instance()->TransientGeodesIsBusy() )
    //{
    //    vpr::System::msleep( 500 );  // half-second delay
    //}
    
    // Set the active datasetm_propertySet->
    if( !SetActiveDataSet( set ) )
    {
        return;
    }
    // set the active scalar and range
    SetActiveScalarAndRange( set );
    // set the active vector
    SetActiveVector( set );
    // Get the active object
    
    std::string direction;
    if( set->PropertyExists( "Direction" ) )
    {
        direction = 
            boost::any_cast< std::string >( set->GetPropertyAttribute( "Direction", "enumCurrentString" ) );
    }
        
    std::string planes;
    if( set->PropertyExists( "Mode" ) )
    {
        int mode = boost::any_cast<int>( set->GetPropertyValue( "Mode" ) );
        if( mode == 0 )
        {
            planes = "Single";
        }
        else if( mode == 1 )
        {
            planes = "Multiple";
        }
    }
    
    std::string advanced;
    if( set->PropertyExists( "Advanced_WarpOption" ) )
    {
        if( boost::any_cast<bool>( set->GetPropertyValue( "Advanced_WarpOption" ) ) )
        {
            advanced = "-warp";
        }
    }
    
    if( set->PropertyExists( "Advanced_Greyscale" ) )
    {
        ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->
            SetGreyscaleFlag( boost::any_cast<bool>( 
            set->GetPropertyValue( "Advanced_Greyscale" ) ) );
    }

    //Create the key for a specific object
    std::pair< std::string, std::pair< std::string, std::string > > commandType;
    commandType = std::make_pair( std::string( m_commandName ),
                                 std::make_pair( direction, planes + advanced ) );
    ves::xplorer::cfdObjects* activeObject = SteadyStateVizHandler::instance()->GetVizObject( commandType );
    if( activeObject == 0 )
    {
        LOG_WARNING( "Selected vis option is not in the VisFeatureMakerBase." );
        return;
    }

    ///Setup the surface name if needed
    if( direction == "By Surface" )
    {
        const std::string dataSetName = 
            boost::any_cast< std::string >( set->GetPropertyAttribute( "Direction_Surface", "enumCurrentString" ) );
        activeObject->SetDataMapSurfaceName( dataSetName );
    }
    
    //Need to wire this up to the UI
    //ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->
    //    SetDataSetScalarState( boost::any_cast<bool>( set->GetPropertyValue( "ScalarBar" ) ) );
    // Load instance of selected DataSet from database and get relevant properties
    // from it.
    xplorer::data::DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", boost::any_cast<std::string >
                       ( set->GetPropertyAttribute
                       ( "DataSet", "enumCurrentString" )
                       ) );
    ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->
        SetDataSetScalarState( boost::any_cast<bool>( dataset.GetPropertyValue( "ScalarBar" ) ) );
    
    // Check to see if any of the objectss need updated before we
    // create actors
    {
        activeObject->SetActiveDataSet( ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
        
        //activeObject->SetVECommand( CommandManager::instance()->GetXMLCommand() );
        //activeObject->UpdateCommand();
        //xplorer::data::PropertySetWeakPtr tempPtr = xplorer::data::PropertySetWeakPtr( &set );
        activeObject->SetPropertySet( set );
        activeObject->UpdateCommand();

        activeObject->SetUUID( set->GetUUIDAsString() );
        
        ModelHandler::instance()->GetActiveModel()->GetModelCADHandler()->MakeCADRootTransparent();
    }

    // get the active vis object
    LOG_INFO( "Setting viz object " 
                << activeObject->GetObjectType()
                << " to _activeObject." );
    
    //SceneManager::instance()->GetRootNode()->AddChild( textOutput->add_text( "executing..." ) );
    
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeDataSetDCS = 
        ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetDCS();
    
    // add active dataset DCS to scene graph if not already there...
    LOG_INFO( "Setting DCS to activeDCS = "
             << activeDataSetDCS.get() );
    
    //this->activeObject->SetActiveDataSet( ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
    //this->activeObject->SetNormal( EnvironmentHandler::instance()->GetNavigate()->GetDirection() );
    //this->activeObject->SetOrigin( EnvironmentHandler::instance()->GetNavigate()->GetObjLocation() );
    activeObject->SetCursorType( NONE );//EnvironmentHandler::instance()->GetCursor()->GetCursorID() );
    activeObject->SetUpdateFlag( false );
    //call back over to ssvishandler to set the flags
    SteadyStateVizHandler::instance()->SetActiveVisObject( activeObject );
    SteadyStateVizHandler::instance()->SetComputeActorsAndGeodes( true );
    //SteadyStateVizHandler::instance()->SetActorsAreReady( true );
}
//////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::SetActiveVector( xplorer::data::PropertySetPtr set )
{
    const std::string activeVector = 
        boost::any_cast<std::string >( set->
        GetPropertyAttribute( "DataSet_VectorData", "enumCurrentString" ) );
    
    if( !activeVector.empty() )
    {
        LOG_INFO( "VisFeatureMakerBase::SetActiveVector Setting Active Vector = " << activeVector );
        
        Model* activeModel = ModelHandler::instance()->GetActiveModel();
        DataSet* activeDataset = activeModel->GetActiveDataSet();
        // need to set the vector by name
        activeDataset->SetActiveVector( activeVector );
        //activeDataset->GetParent()->SetActiveVector( vectorIndex );
        
        /*cfdTextureDataSet* activeTDSet = activeModel->GetActiveTextureDataSet();
         if(activeTDSet )
         {
         activeTDSet->SetActiveVector( activeVector );
         }*/
    }
}
//////////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::SetActiveScalarAndRange( xplorer::data::PropertySetPtr set )
{    
    std::string activeScalarName = boost::any_cast<std::string >( set->GetPropertyAttribute( "DataSet_ScalarData", "enumCurrentString" ) );
        
    double scalarMin = boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Min" ) );
    double scalarMax = boost::any_cast<double>( set->GetPropertyValue( "DataSet_ScalarRange_Max" ) );
    
    vprDEBUG( vesDBG, 1 ) << "|\tVisFeatureMakerBase::SetActiveScalarAndRange Set the scalar and range "
    << ", scalar = " << activeScalarName
    << ", min = " << scalarMin
    << ", max = " << scalarMax
    << std::endl << vprDEBUG_FLUSH;
    DataSet* activeDataset = ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
    //update active scalar texture if it exists
    
    activeDataset->SetActiveScalar( activeScalarName );
    activeDataset->GetParent()->SetActiveScalar( activeScalarName );
    
    activeDataset->ResetScalarBarRange( scalarMin, scalarMax );
    activeDataset->GetParent()->ResetScalarBarRange( scalarMin, scalarMax );
}
//////////////////////////////////////////////////////////////////
bool VisFeatureMakerBase::SetActiveDataSet( xplorer::data::PropertySetPtr set )
{    
    std::string dataSetName = boost::any_cast<std::string >( set->GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    xplorer::data::DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", dataSetName );
    //const std::string& longFilename = boost::any_cast< std::string >( dataset.GetPropertyValue( "LongFilename" ) );
    
    //Need to set the active datasetname and get the position of the dataset
    Model* activeModel = ModelHandler::instance()->GetActiveModel();
    unsigned int i = activeModel->GetIndexOfDataSet( dataSetName );
    vprDEBUG( vesDBG, 1 )
    << "|\tVisFeatureMakerBase CHANGE_STEADYSTATE_DATASET " << i
    << std::endl << vprDEBUG_FLUSH;
    //update active texture dataset if it exists
    unsigned int nTextureDataSets = activeModel->GetNumberOfTextureDataSets();
    if( ( nTextureDataSets ) && ( i < nTextureDataSets ) )
    {
        cfdTextureDataSet* activeTDSet = activeModel->GetTextureDataSet( i );
        activeModel->SetActiveTextureDataSet( activeTDSet );
    }
    if( i < activeModel->GetNumberOfCfdDataSets() )
    {
        vprDEBUG( vesDBG, 0 ) << "|\tVisFeatureMakerBase::SetActiveDataSet dataset = "
        << activeModel->GetCfdDataSet( i )->GetFileName()
        << ", dcs = " << activeModel->GetCfdDataSet( i )->GetDCS()
        << std::endl << vprDEBUG_FLUSH;
        
        int cfdType = activeModel->GetCfdDataSet( i )->GetType();
        vprDEBUG( vesDBG, 1 ) << "|\tVisFeatureMakerBase::SetActiveDataSet cfdType: " << cfdType
        << std::endl << vprDEBUG_FLUSH;
        
        // set the dataset as the appropriate dastaset type
        // (and the active dataset as well)
        DataSet* activeDataset = activeModel->GetCfdDataSet( i );
        
        // Get the previous active dataset
        std::string oldDatasetName;
        if( activeModel->GetActiveDataSet() )
        {
            oldDatasetName = activeModel->GetActiveDataSet()->GetFileName();
            vprDEBUG( vesDBG, 1 ) 
            << "|\tVisFeatureMakerBase::SetActiveDataSet last active dataset name = "
            << oldDatasetName
            << std::endl << vprDEBUG_FLUSH;
        }
        
        activeModel->SetActiveDataSet( activeDataset );
        vprDEBUG( vesDBG, 1 ) << "|\tVisFeatureMakerBase::SetActiveDataSet Activating steady state file "
        << activeDataset->GetFileName() << " == "
        << activeModel->GetActiveDataSet()->GetFileName()
        << std::endl << vprDEBUG_FLUSH;
        
        // make sure that the user did not just hit same dataset button
        // (or change scalar since that is routed through here too)
        if( oldDatasetName != activeDataset->GetFileName() )
        {
            vprDEBUG( vesDBG, 1 ) 
            << "|\tVisFeatureMakerBase::SetActiveDataSet  setting dataset as newly activated"
            << std::endl << vprDEBUG_FLUSH;
            activeDataset->SetNewlyActivated();
        }
        return true;
    }
    else
    {
        std::cerr << "ERROR: VisFeatureMakerBase::SetActiveDataSet  requested steady state dataset "
        << activeModel->GetNumberOfCfdDataSets()
        << std::endl;
        return false;
    }
}
//////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
