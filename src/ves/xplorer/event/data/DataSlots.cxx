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

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/data/DataSlots.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/OneDStringArray.h>

#include <string>
#include <vector>
#include <fstream>

#include <latticefx/core/DataSet.h>

#include <latticefx/core/vtk/DataSet.h>
#include <latticefx/utils/vtk/fileIO.h>
#include <latticefx/utils/vtk/VTKFileHandler.h>

#include <osgwTools/Orientation.h>
#include <osgwTools/Quat.h>

#include <boost/filesystem.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/version.hpp>

#include <switchwire/SingleShotSignal.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace data
{
////////////////////////////////////////////////////////////////////////////////
lfx::core::vtk::DataSetPtr GetSelectedDataset( std::string const& uuid )
{
    ves::xplorer::Model* activeModel =
        ModelHandler::instance()->GetActiveModel();

    ves::xplorer::data::DatasetPropertySet set;
    set.SetUUID( uuid );
    set.Load();
    const std::string& datasetName =
        boost::any_cast<std::string>( set.GetPropertyValue( "Filename" ) );

    lfx::core::vtk::DataSetPtr dataSet = activeModel->GetCfdDataSet(
                           activeModel->GetIndexOfDataSet( datasetName ) );
    return dataSet;
}
////////////////////////////////////////////////////////////////////////////////
void SetContourPlaneGreyscale( std::string const& uuid, std::vector< bool > const& greyscaleflag )
{
    vprDEBUG( vesDBG, 2 )
            << "|\tDataSlots::SetContourPlaneGreyscale : uuid " << uuid
            << std::endl << vprDEBUG_FLUSH;

    if( ModelHandler::instance()->GetActiveModel() == NULL )
    {
        return;
    }

    if( ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() == NULL )
    {
        return;
    }

    ModelHandler::instance()->GetActiveModel()->
    GetActiveDataSet()->SetGreyscaleFlag( greyscaleflag[0] );
}
////////////////////////////////////////////////////////////////////////////////
void TransformDatasetNode( const std::string& uuid, const std::vector< double >& transform )
{
    ves::xplorer::Model* activeModel = ModelHandler::instance()->GetActiveModel();

    if( activeModel == NULL )
    {
        return;
    }

    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( uuid );

    osg::ref_ptr< osg::PositionAttitudeTransform > dcs = dataSet->GetDCS();

    if( dcs.valid() )
    {
        // Entire transform is packed into a single vector. Unpack into
        // separate translation, rotation, and scale pieces.
        std::vector<double>::const_iterator start = transform.begin();
        std::vector<double>::const_iterator stop = transform.begin() + 3;
        std::vector<double> translation( start, stop );
        std::vector<double> rotation( start + 3, stop + 3 );
        std::vector<double> scale( start + 6, stop + 6 );

        dcs->setScale( osg::Vec3d( scale[ 0 ], scale[ 1 ], scale[ 2 ] ) );
        dcs->setPosition( osg::Vec3d( translation[ 0 ], translation[ 1 ], translation[ 2 ] ) );
        {
            //The rotation values are returned in ypr format. Not x, y, z.
            osg::ref_ptr< osgwTools::Orientation > tempQuat = new osgwTools::Orientation();
            dcs->setAttitude( tempQuat->getQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );

            //dcs->setAttitude( osgwTools::makeHPRQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SetDatasetSurfaceWrap( std::string const& uuid, bool const& surfaceWrap )
{
    vprDEBUG( vesDBG, 2 )
            << "|\tDataSlots::SetDatasetSurfaceWrap : uuid " << uuid
            << std::endl << vprDEBUG_FLUSH;

    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( uuid );

    if( !dataSet )
    {
        return;
    }

    dataSet->SetWireframeState( surfaceWrap );
}
////////////////////////////////////////////////////////////////////////////////
void AddTextureDataset( std::string const&, std::string const& dirName )
{
    ves::xplorer::Model* activeModel =
        ModelHandler::instance()->GetActiveModel();
    activeModel->CreateTextureDataSet();

    activeModel->AddDataSetToTextureDataSet( 0, dirName );
    //std::ostringstream textId;
    //textId << "VTK_SURFACE_DIR_PATH_" << j;
    //DataSet* dataSet = GetSelectedDataset( uuid );
    //dataSet->SetUUID( textId.str(),
    //  tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" )->GetID() );
}
////////////////////////////////////////////////////////////////////////////////
void ToggleCADNode( const std::string& nodeID, bool const& visible )
{
    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( nodeID );

    if( !dataSet )
    {
        return;
    }

    dataSet->GetDCS()->setNodeMask( visible );
}
////////////////////////////////////////////////////////////////////////////////
void DeleteDataSet( const std::string& dataFilename )
{
    ves::xplorer::Model* activeModel =
        ModelHandler::instance()->GetActiveModel();

    size_t numDataSets = activeModel->GetModelData()->GetNumberOfInformationPackets();
    for( size_t i = 0; i < numDataSets; ++i )
    {
        std::string xmlFileName = activeModel->GetModelData()->GetInformationPacket( i )->
            GetProperty( "VTK_DATA_FILE" )->GetDataString();
        if( dataFilename == xmlFileName )
        {
            activeModel->GetModelData()->RemoveInformationPacket( i );
            break;
        }
    }
    activeModel->DeleteDataSet( dataFilename );
    activeModel->SetActiveDataSet( lfx::core::vtk::DataSetPtr() );

    //The underlying property set is being removed in the TreeTab remove dataset methods.
    //There is no need to do that here.
    /*ves::xplorer::data::DatasetPropertySet set;
    bool success = set.LoadByKey( "Filename", dataFilename );
    if( !success )
    {
        std::cout << "dataset not found "  << dataFilename << std::endl;
    }
    success = set.Remove();
    if( !success )
    {
        std::cout << "dataset not found "  << dataFilename << std::endl;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void ShowBBox( const std::string& uuid, const bool& show )
{
    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( uuid );;
    if( dataSet )
    {
        dataSet->SetBoundingBoxState( show );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UpdateDimensions( const std::string& uuid, const std::vector< int >& allDimensions )
{
    boost::ignore_unused_variable_warning( uuid );
    ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->
    SetDimensions( allDimensions[0],
                  allDimensions[1],
                  allDimensions[2] );
}
////////////////////////////////////////////////////////////////////////////////
void UpdateAllBounds( const std::vector< double >& bounds )
{
    double databounds[6] = {0, 0, 0, 0, 0, 0};
    //_activeModel->GetActiveDataSet()->GetDataSet()->GetWholeBoundingBox(databounds);
    ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetBounds( databounds );
    double newValue[6] = {0, 0, 0, 0, 0, 0};
    newValue[0] = databounds[0] + bounds.at( 0 ) * ( databounds[1] - databounds[0] );
    newValue[1] = databounds[0] + bounds.at( 1 ) * ( databounds[1] - databounds[0] );
    newValue[2] = databounds[2] + bounds.at( 2 ) * ( databounds[3] - databounds[2] );
    newValue[3] = databounds[2] + bounds.at( 3 ) * ( databounds[3] - databounds[2] );
    newValue[4] = databounds[4] + bounds.at( 4 ) * ( databounds[5] - databounds[4] );
    newValue[5] = databounds[4] + bounds.at( 5 ) * ( databounds[5] - databounds[4] );
    
    ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->
    SetBounds( newValue[0],
              newValue[1],
              newValue[2],
              newValue[3],
              newValue[4],
              newValue[5] );
}
////////////////////////////////////////////////////////////////////////////////
void ActivateSeedPoints( const std::string& dataSetName, const bool seedPointDisplay )
{
    //make the CAD transparent
    ves::xplorer::Model* tempModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
    tempModel->GetModelCADHandler()->MakeCADRootTransparent();
    
    //check to see if the seed points exist
    if( !ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()->containsNode( ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS() ) )
    {
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()->
            addChild( ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS() );
    }
    
    //this seems to be a bad sequence of calls but we need to set the
    //active dataset otherwise this set of calls goes in every seed pointEH
    //as well as all the commands have to lug this extra info.
    tempModel->SetActiveDataSet( tempModel->GetCfdDataSet( tempModel->GetIndexOfDataSet( dataSetName ) ) );
    osg::ref_ptr< osg::PositionAttitudeTransform > tempDCS = tempModel->GetActiveDataSet()->GetDCS();
    ves::xplorer::scenegraph::DCS* seedPointDCS = ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS();
    seedPointDCS->setScale( tempDCS->getScale() );
    seedPointDCS->setPosition( tempDCS->getPosition() );
    seedPointDCS->setAttitude( tempDCS->getAttitude() );

    ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->Toggle( seedPointDisplay );
}
////////////////////////////////////////////////////////////////////////////////
void ShowScalarBar( const std::string& uuid, const bool& show )
{
    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( uuid );;
    if( dataSet )
    {
        dataSet->SetDataSetScalarState( show );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WriteDatabaseEntry( lfx::core::vtk::DataSetPtr dataSet, ves::open::xml::ParameterBlockPtr tempInfoPacket )
{
    xplorer::data::DatasetPropertySet set;

    //boost::filesystem::path tempPath( fileName );
    //std::string shortName = tempPath.filename().string();

    //set.LoadByKey( "Filename", shortName );
    bool tempLoad = set.LoadByKey( "Filename", dataSet->GetFileName() );
    const std::string& dataSetUUID = dataSet->GetUUID( "VTK_DATA_FILE" );

    if( !tempLoad && !dataSetUUID.empty() )
    {
        set.SetUUID( dataSetUUID );
    }

    //When there are child datasets we do not want to overwrite the
    //scenegraph with uuid data for the child datasets because the ui
    //will use this information to look up the db entries
    if( dataSet->GetParent() == dataSet )
    {
        osg::Node::DescriptionList descriptorsList;
        descriptorsList.push_back( "VE_DATA_NODE" );
        descriptorsList.push_back( set.GetUUIDAsString() );
        dataSet->GetDCS()->setDescriptions( descriptorsList );
    }
    
    set.SetPropertyValue( "Filename", dataSet->GetFileName() );
    set.SetPropertyValue( "StepLength", dataSet->GetStepLength() );
    set.SetPropertyValue( "MaxTime", dataSet->GetMaxTime() );
    set.SetPropertyValue( "TimeStep", dataSet->GetTimeStep() );
    set.SetPropertyValue( "Type", dataSet->GetType() );
    set.SetPropertyValue( "PrecomputedDataSliceDir", dataSet->GetPrecomputedDataSliceDir() );
    set.SetPropertyValue( "PrecomputedSurfaceDir", dataSet->GetPrecomputedSurfaceDir() );
    set.SetPropertyValue( "ScalarNames", dataSet->GetScalarNames() );
    set.SetPropertyValue( "VectorNames", dataSet->GetVectorNames() );

    std::vector< double > ScalarMins;
    std::vector< double > ScalarMaxes;
    for( int index = 0; index < dataSet->GetNumberOfScalars(); ++index )
    {
        double* range = dataSet->GetActualScalarRange( index );
        ScalarMins.push_back( range[0] );
        ScalarMaxes.push_back( range[1] );
    }
    set.SetPropertyValue( "ScalarMins", ScalarMins );
    set.SetPropertyValue( "ScalarMaxes", ScalarMaxes );

    if( tempInfoPacket )
    {
        ves::open::xml::TransformPtr dataTransform = tempInfoPacket->GetTransform();

        ves::open::xml::FloatArrayPtr nodeTranslation =
            dataTransform->GetTranslationArray();
        ves::open::xml::FloatArrayPtr nodeRotation =
            dataTransform->GetRotationArray();
        ves::open::xml::FloatArrayPtr nodeScale =
            dataTransform->GetScaleArray();
        
        set.SetPropertyValue( "Transform_Translation_X",
                                nodeTranslation->GetElement( 0 ) );
        set.SetPropertyValue( "Transform_Translation_Y",
                                nodeTranslation->GetElement( 1 ) );
        set.SetPropertyValue( "Transform_Translation_Z",
                                nodeTranslation->GetElement( 2 ) );
        set.SetPropertyValue( "Transform_Rotation_X",
                                nodeRotation->GetElement( 1 ) );
        set.SetPropertyValue( "Transform_Rotation_Y",
                                nodeRotation->GetElement( 2 ) );
        set.SetPropertyValue( "Transform_Rotation_Z",
                                nodeRotation->GetElement( 0 ) );
        
        if( ( nodeScale->GetElement( 0 ) == nodeScale->GetElement( 1 ) ) &&
           ( nodeScale->GetElement( 1 ) == nodeScale->GetElement( 2 ) ) )
        {
            set.SetPropertyValue( "Transform_Scale_Uniform", true );
        }
        else
        {
            set.SetPropertyValue( "Transform_Scale_Uniform", false );
        }
        
        set.SetPropertyValue( "Transform_Scale_X",
                                nodeScale->GetElement( 0 ) );
        set.SetPropertyValue( "Transform_Scale_Y",
                                nodeScale->GetElement( 1 ) );
        set.SetPropertyValue( "Transform_Scale_Z",
                                nodeScale->GetElement( 2 ) );
    }
    set.Save();
}
////////////////////////////////////////////////////////////////////////////////
void LoadTransientData( const std::string& uuid, const bool& load )
{
    if( !load )
    {
        return;
    }
    
    lfx::core::vtk::DataSetPtr dataSet = GetSelectedDataset( uuid );;

    boost::filesystem::path tempPath( dataSet->GetFileName() );
    std::string tempFilename = dataSet->GetFileName();
    std::string dataDir = tempPath.parent_path().string();
    if( dataDir.empty() )
    {
        dataDir = "./";
        tempFilename = dataDir + tempFilename;
    }
    const std::string fileExt = tempPath.extension().string();
    dataSet->LoadTransientData( dataDir, fileExt );
    std::vector< lfx::core::vtk::DataSetPtr > dataSetVector =
        dataSet->GetTransientDataSets();

    for( size_t i = 0; i < dataSetVector.size(); ++i )
    {
        if( tempFilename != dataSetVector[ i ]->GetFileName() )
        {
            ves::xplorer::event::data::WriteDatabaseEntry( dataSetVector[ i ], ves::open::xml::ParameterBlockPtr() );
        }
    }

    // Notify about scenegraph changes
    reinterpret_cast< ves::util::VoidSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "ScenegraphChanged" ) )
    ->signal();
}
////////////////////////////////////////////////////////////////////////////////
void LoadTransientTimeSteps( const std::string& filename )
{
    ves::xplorer::data::DatasetPropertySet set;
    bool loaded = set.LoadByKey( "Filename", filename );
    if( !loaded )
    {
        return;
    }
    std::string uuidString = set.GetUUIDAsString();
    //std::cout << " load data " << filename << " " << loaded << " " << uuidString << std::endl;

    loaded = boost::any_cast< bool >( set.GetPropertyValue( "TimeDataset" ) );
    //std::cout << loaded << std::endl;
    if( loaded )
    {
        LoadTransientData( uuidString, true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void LoadDatasetFromFile( const std::string& filename )
{
    // TODO: Much of this method could use a good cleaning. Right now, we use
    // a VTKFileHandler to pull info from a file, pack it up into DVPs,
    // then later on unpack the very same data from the DVPs. Why? Because this
    // method is a mash-up of two previous methods that talked to each other
    // using the DVP mechanism. A lot of it could potentially be eliminated.
    ves::xplorer::Model* activeModel =
        ModelHandler::instance()->GetActiveModel();
    if( !activeModel )
    {
        return;
    }

/// This block has been moved from conductor/qt/MainWindow.cxx::LoadDataFile to
/// here.

        ves::open::xml::ParameterBlockPtr mParamBlock;
        ves::open::xml::model::ModelPtr veModel(
            ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelData() );
        mParamBlock = veModel->GetInformationPacket( -1 );
        mParamBlock->SetName( filename );

        //Load a vtk file
        ves::open::xml::DataValuePairPtr tempDVP =
            mParamBlock->GetProperty( "VTK_DATA_FILE" );
        if( !tempDVP )
        {
            tempDVP = mParamBlock->GetProperty( -1 );
        }
        tempDVP->SetData( "VTK_DATA_FILE", filename );

        lfx::vtk_utils::VTKFileHandler tempHandler;
        std::vector< std::string > dataArrayList =
            tempHandler.GetDataSetArraysFromFile( filename );

        if( !dataArrayList.empty() )
        {
            ves::open::xml::DataValuePairPtr arraysDVP =
                mParamBlock->GetProperty( "VTK_ACTIVE_DATA_ARRAYS" );
            if( !arraysDVP )
            {
                arraysDVP = mParamBlock->GetProperty( -1 );
            }
            ves::open::xml::OneDStringArrayPtr
            stringArray( new ves::open::xml::OneDStringArray() );
            stringArray->SetArray( dataArrayList );
            arraysDVP->SetData( "VTK_ACTIVE_DATA_ARRAYS", stringArray );
        }

 ///


    //CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
    /*
    std::string dataSetName =
        command->GetDataValuePair( "VTK_DATASET_NAME" )->GetDataString();
    if( !dataSetName.compare( "NULL" ) )
    {
        std::cerr << "AddVTKDataSetEventHandler::Execute : No base dataset is specified." << std::endl;
        return;
    }*/

    /*DataValuePairPtr veModelDVP =
        command->GetDataValuePair( "CREATE_NEW_DATASETS" );
    xml::model::ModelPtr veModel =
        boost::dynamic_pointer_cast<xml::model::Model>(
            veModelDVP->GetDataXMLObject() );*/

    size_t numInfoPackets = veModel->GetNumberOfInformationPackets();
    for( size_t i = 0; i < numInfoPackets; ++i )
    {
        ves::open::xml::ParameterBlockPtr tempInfoPacket = veModel->GetInformationPacket( i );

        if( !tempInfoPacket->GetProperty( "VTK_DATA_FILE" ) )
        {
            continue;
        }
        /*
        // Assume only one model for now
        // Flexibilty to have multiply models
        if( !_activeModel )
        {
            return;
        }*/
        size_t currentNumberOfDataSets = activeModel->GetNumberOfCfdDataSets();
        bool foundDataSet = false;

        //check to see if dataset is already on this particular model
        for( size_t j = 0; j < currentNumberOfDataSets; ++j )
        {
            if( tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetID() ==
                    activeModel->GetCfdDataSet( j )->GetUUID( "VTK_DATA_FILE" ) )
            {
                foundDataSet = true;
                break;
            }
        }

        if( foundDataSet )
        {
            std::cout << "|\tSkipping load of dataset: "
                      << tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetID()
                      << std::endl;
            continue;
        }

        //////////////////////////////////////////////////////////////
        // get vtk data set name...
        std::string vtk_filein =
            tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetDataString();
        if( !lfx::vtk_utils::fileIO::isFileReadable( vtk_filein ) )
        {
            std::cerr << "ERROR: unreadable vtk file = " << vtk_filein
                      << ".  You may need to correct your ves file."
                      << std::endl;
            continue;
        }

        ///////////////////////////////////////////////////////////////
        //If not already there lets create a new dataset
        activeModel->CreateCfdDataSet();
        // Pass in -1 to GetCfdDataSet to get the last dataset added
        lfx::core::vtk::DataSetPtr lastDataAdded = activeModel->GetCfdDataSet( -1 );
        vprDEBUG( vesDBG, 0 )
                << "|\t*************Now starting to load new data************ "
                << std::endl << vprDEBUG_FLUSH;

        //Setup the transform for the dataset
        {
            ves::open::xml::TransformPtr dataTransform = tempInfoPacket->GetTransform();
            std::vector< double > translation = dataTransform->GetTranslationArray()->GetArray();
            std::vector< double > rotation = dataTransform->GetRotationArray()->GetArray();
            std::vector< double > scale = dataTransform->GetScaleArray()->GetArray();

            osg::ref_ptr< osg::PositionAttitudeTransform > transform = lastDataAdded->GetDCS();
            transform->setScale( osg::Vec3d( scale[ 0 ], scale[ 1 ], scale[ 2 ] ) );
            transform->setPosition( osg::Vec3d( translation[ 0 ], translation[ 1 ], translation[ 2 ] ) );
            {
                osg::ref_ptr< osgwTools::Orientation > tempQuat = new osgwTools::Orientation();
                transform->setAttitude( tempQuat->getQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );
            }
        }

        vprDEBUG( vesDBG, 0 ) << "|\tvtk file = " << vtk_filein
                              << std::endl << vprDEBUG_FLUSH;
        lastDataAdded->SetFileName( vtk_filein );
        lastDataAdded->SetUUID( "VTK_DATA_FILE",
                                tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetID() );
        ves::open::xml::DataValuePairPtr stringDVP =
            tempInfoPacket->GetProperty( "VTK_ACTIVE_DATA_ARRAYS" );
        std::vector< std::string > vecStringArray;
        if( stringDVP )
        {
            ves::open::xml::OneDStringArrayPtr stringArray =
                boost::dynamic_pointer_cast <
                ves::open::xml::OneDStringArray > (
                    stringDVP->GetDataXMLObject() );
            vecStringArray = stringArray->GetArray();
            lastDataAdded->SetActiveDataArrays( vecStringArray );
        }

        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" ) )
        {
            std::string precomputedDataSliceDir =
                tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->
                GetDataString();
            lastDataAdded->
                    SetPrecomputedDataSliceDir( precomputedDataSliceDir );
            lastDataAdded->SetUUID( "VTK_PRECOMPUTED_DIR_PATH",
                                    tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->
                                    GetID() );
        }
        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" ) )
        {
            std::string precomputedSurfaceDir =
                tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->
                GetDataString();
            lastDataAdded->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
            lastDataAdded->SetUUID( "VTK_SURFACE_DIR_PATH",
                                    tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->
                                    GetID() );
        }

        ves::xplorer::event::data::LoadSurfaceFiles( lastDataAdded->GetPrecomputedSurfaceDir() );
        //////////////////////////////////////////////////////////////
        //Load texture datasets
        if( tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" ) )
        {
            vprDEBUG( vesDBG, 0 ) << "|\tCreating texture dataset."
                                  << std::endl << vprDEBUG_FLUSH;
            activeModel->CreateTextureDataSet();
            size_t numProperties = tempInfoPacket->GetNumberOfProperties();
            for( size_t j = 0; j < numProperties; ++j )
            {
                if( tempInfoPacket->GetProperty( j )->GetDataName() ==
                        std::string( "VTK_TEXTURE_DIR_PATH" ) )
                {
                    activeModel->AddDataSetToTextureDataSet( 0,
                            tempInfoPacket->GetProperty( j )->GetDataString() );
                    std::ostringstream textId;
                    textId << "VTK_SURFACE_DIR_PATH_" << j;
                    lastDataAdded->SetUUID( textId.str(), tempInfoPacket->
                                            GetProperty( "VTK_TEXTURE_DIR_PATH" )->GetID() );
                }
            }
        }

        //Now load up the dataset
        {
            const std::string tempDataSetFilename =
                lastDataAdded->GetFileName();
            std::cout << "|\tLoading data for file "
                      << tempDataSetFilename
                      << std::endl;
            lastDataAdded->SetArrow(
                ves::xplorer::ModelHandler::instance()->GetArrow() );
            //Check and see if the data is part of a transient series
            if( tempInfoPacket->GetProperty( "VTK_TRANSIENT_SERIES" ) )
            {
                std::string precomputedSurfaceDir =
                    tempInfoPacket->GetProperty( "VTK_TRANSIENT_SERIES" )->
                    GetDataString();
                lastDataAdded->LoadTransientData( precomputedSurfaceDir );
                std::vector< lfx::core::vtk::DataSetPtr > dataSetVector = lastDataAdded->GetTransientDataSets();
                for( size_t i = 0; i < dataSetVector.size(); ++i )
                {
                    ves::xplorer::event::data::WriteDatabaseEntry( dataSetVector[ i ], tempInfoPacket );
                }
            }
            else
            {
                lastDataAdded->LoadData();
                std::vector< lfx::core::vtk::DataSetPtr > dataSetVector = lastDataAdded->GetChildDataSets();
                for( size_t i = 0; i < dataSetVector.size(); ++i )
                {
                    ves::xplorer::event::data::WriteDatabaseEntry( dataSetVector[ i ], tempInfoPacket );
                }
                ves::xplorer::event::data::WriteDatabaseEntry( lastDataAdded, tempInfoPacket );
            }
            //If the data load failed
            if( !lastDataAdded->GetDataSet() )
            {
                std::cout << "|\tData failed to load." << std::endl;
                activeModel->DeleteDataSet( tempDataSetFilename );
            }
            else
            {
                std::cout << "|\tData is loaded for file "
                          << tempDataSetFilename
                          << std::endl;
                if( lastDataAdded->GetParent() == lastDataAdded )
                {
                    activeModel->GetDCS()->
                        addChild( lastDataAdded->GetDCS() );
                    activeModel->SetActiveDataSet( lfx::core::vtk::DataSetPtr() );
                }
                switchwire::SingleShotSignal< void, const std::string& >( "LoadDatasetFromFile.DatafileLoaded", tempDataSetFilename );
            }
        }
        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "Create Surface Wrap" ) )
        {
            unsigned int surfaceToggle = 0;
            tempInfoPacket->GetProperty( "Create Surface Wrap" )->
                GetData( surfaceToggle );
            if( surfaceToggle )
            {
                //Create surface
                lastDataAdded->CreateSurfaceWrap();
            }
        }
    }
}

bool LoadLfxDataSet( lfx::core::DataSetPtr dsp, const std::string& dbFile, const std::string& diskPath, const std::string& recipeFile )
{
    if( !( dbFile.empty() ) )
    {
        if( !dsp->loadDsFromCrunchstore( dbFile ) ) return false;
    }
    else if( !( diskPath.empty() ) )
    {
        if( !dsp->loadDsFromFolder( diskPath ) )  return false;
    }
	else
	{
		return false;
	}

	if( !recipeFile.empty() )
	{

		std::string err;
		if( !dsp->loadPipeline( recipeFile, &err ) )
		{
			return false;
		}

		return true;
	}

	return true;
}
////////////////////////////////////////////////////////////////////////////////
bool ProcessLfxConfigFile( const std::string& filename, std::string *pathDbFile, std::string *pathDbFolder, std::string *pathRecipe )
{
	std::ifstream in( filename.c_str(), std::ios::in );

	if( !in.is_open() ) return false;
	
	if( in.eof() ) return false;

	std::string line;
	std::getline( in, line );

	boost::filesystem::path path( line );
	if( boost::filesystem::is_directory( path ) )
	{
		*pathDbFolder = line;
	}
	else if( boost::filesystem::exists( path ) )
	{
		std::string ext = path.extension().string();
		if( ext != ".lfxtd" ) return false;
		
		*pathDbFile = line;
	}
	else
	{
		return false;
	}

	// is there a second line with a recipe file
	if( in.eof() ) return true;

	std::getline( in, line );
	path = line;
	if( boost::filesystem::exists( path ) && !boost::filesystem::is_directory( path ) )
	{
		std::string ext = path.extension().string();
		if( ext == ".lfxrf" )
		{
			*pathRecipe = line;
		}
	}
	
	return true;
}

////////////////////////////////////////////////////////////////////////////////
bool ProcessLfxFileType( const std::string& filename, std::string *pathDbFile, std::string *pathDbFolder, std::string *pathRecipe )
{
	// lets determine what type of file it is..
	boost::filesystem::path path( filename );
	std::string ext = path.extension().string();
	if( ext == ".lfxtd" )
	{
		*pathDbFile = filename;
		return true;
	}
	else if( ext == ".lfxcf" )
	{
		return ProcessLfxConfigFile( filename, pathDbFile, pathDbFolder, pathRecipe );
	}
	
	vprDEBUG( vesDBG, 0 ) << "|\tlfx file = " << filename << " has an unrecognized extension." << std::endl << vprDEBUG_FLUSH;
	return false;
}

////////////////////////////////////////////////////////////////////////////////
void LoadLfxDataFromFile( const std::string& filename )
{
	ves::xplorer::Model* activeModel = ModelHandler::instance()->GetActiveModel();
    if( !activeModel )
    {
        return;
    }

	std::string pathDbFile, pathDbFolder, pathRecipe;
	if( !ProcessLfxFileType( filename, &pathDbFile, &pathDbFolder, &pathRecipe ) )
	{
		return;
	}

	std::string dataName = "LFX_DATA_FILE";

	ves::open::xml::ParameterBlockPtr mParamBlock;
    ves::open::xml::model::ModelPtr veModel( ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelData() );
	mParamBlock = veModel->GetInformationPacket( -1 );
    mParamBlock->SetName( filename );

	// Load a vtk file
	ves::open::xml::DataValuePairPtr tempDVP = mParamBlock->GetProperty( dataName );
	if( !tempDVP )
    {
		tempDVP = mParamBlock->GetProperty( -1 );
	}
    tempDVP->SetData( dataName, filename );



	size_t numInfoPackets = veModel->GetNumberOfInformationPackets();
    for( size_t i = 0; i < numInfoPackets; ++i )
    {
        ves::open::xml::ParameterBlockPtr tempInfoPacket = veModel->GetInformationPacket( i );

        if( !tempInfoPacket->GetProperty( dataName ) )
        {
            continue;
        }
      
        size_t currentNumberOfDataSets = activeModel->GetNumberOfLfxDataSets();
        bool foundDataSet = false;

        //check to see if dataset is already on this particular model
        for( size_t j = 0; j < currentNumberOfDataSets; ++j )
        {
            if( tempInfoPacket->GetProperty( dataName )->GetID() ==
                    activeModel->GetLfxDataSet( j )->getUUID( dataName ) )
            {
                foundDataSet = true;
                break;
            }
        }

        if( foundDataSet )
        {
            std::cout << "|\tSkipping load of dataset: "
                      << tempInfoPacket->GetProperty( dataName )->GetID()
                      << std::endl;
            continue;
        }

        //////////////////////////////////////////////////////////////
        // get data set name...
        std::string lfx_filein = tempInfoPacket->GetProperty( dataName )->GetDataString();

        ///////////////////////////////////////////////////////////////
        //If not already there lets create a new dataset
        activeModel->CreateLfxDataSet();
        // Pass in -1 to GetLfxDataSet to get the last dataset added
        lfx::core::DataSetPtr lastDataAdded = activeModel->GetLfxDataSet( -1 );
        vprDEBUG( vesDBG, 0 )
                << "|\t*************Now starting to load new data************ "
                << std::endl << vprDEBUG_FLUSH;

		// TODO:
		/*
        //Setup the transform for the dataset
        {
            ves::open::xml::TransformPtr dataTransform = tempInfoPacket->GetTransform();
            std::vector< double > translation = dataTransform->GetTranslationArray()->GetArray();
            std::vector< double > rotation = dataTransform->GetRotationArray()->GetArray();
            std::vector< double > scale = dataTransform->GetScaleArray()->GetArray();

            osg::ref_ptr< osg::PositionAttitudeTransform > transform = lastDataAdded->GetDCS();
            transform->setScale( osg::Vec3d( scale[ 0 ], scale[ 1 ], scale[ 2 ] ) );
            transform->setPosition( osg::Vec3d( translation[ 0 ], translation[ 1 ], translation[ 2 ] ) );
            {
                osg::ref_ptr< osgwTools::Orientation > tempQuat = new osgwTools::Orientation();
                transform->setAttitude( tempQuat->getQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );
            }
        }
		*/

        vprDEBUG( vesDBG, 0 ) << "|\tlfx file = " << lfx_filein
                              << std::endl << vprDEBUG_FLUSH;
        lastDataAdded->setName( lfx_filein );
        lastDataAdded->setUUID( dataName, tempInfoPacket->GetProperty( dataName )->GetID() );

		if( !LoadLfxDataSet( lastDataAdded, pathDbFile, pathDbFolder, pathRecipe ) )
		{
			 activeModel->DeleteLfxDataSet(  lfx_filein );

			 vprDEBUG( vesDBG, 0 )
                << "|\tFailed to load lfxDataSet " << lfx_filein
                << std::endl << vprDEBUG_FLUSH;
			 return;
		}

		/*
        ves::open::xml::DataValuePairPtr stringDVP =
            tempInfoPacket->GetProperty( "VTK_ACTIVE_DATA_ARRAYS" );
        std::vector< std::string > vecStringArray;
        if( stringDVP )
        {
            ves::open::xml::OneDStringArrayPtr stringArray =
                boost::dynamic_pointer_cast <
                ves::open::xml::OneDStringArray > (
                    stringDVP->GetDataXMLObject() );
            vecStringArray = stringArray->GetArray();
            lastDataAdded->SetActiveDataArrays( vecStringArray );
        }

        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" ) )
        {
            std::string precomputedDataSliceDir =
                tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->
                GetDataString();
            lastDataAdded->
                    SetPrecomputedDataSliceDir( precomputedDataSliceDir );
            lastDataAdded->SetUUID( "VTK_PRECOMPUTED_DIR_PATH",
                                    tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->
                                    GetID() );
        }
        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" ) )
        {
            std::string precomputedSurfaceDir =
                tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->
                GetDataString();
            lastDataAdded->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
            lastDataAdded->SetUUID( "VTK_SURFACE_DIR_PATH",
                                    tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->
                                    GetID() );
        }

        ves::xplorer::event::data::LoadSurfaceFiles( lastDataAdded->GetPrecomputedSurfaceDir() );
        //////////////////////////////////////////////////////////////
        //Load texture datasets
        if( tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" ) )
        {
            vprDEBUG( vesDBG, 0 ) << "|\tCreating texture dataset."
                                  << std::endl << vprDEBUG_FLUSH;
            activeModel->CreateTextureDataSet();
            size_t numProperties = tempInfoPacket->GetNumberOfProperties();
            for( size_t j = 0; j < numProperties; ++j )
            {
                if( tempInfoPacket->GetProperty( j )->GetDataName() ==
                        std::string( "VTK_TEXTURE_DIR_PATH" ) )
                {
                    activeModel->AddDataSetToTextureDataSet( 0,
                            tempInfoPacket->GetProperty( j )->GetDataString() );
                    std::ostringstream textId;
                    textId << "VTK_SURFACE_DIR_PATH_" << j;
                    lastDataAdded->SetUUID( textId.str(), tempInfoPacket->
                                            GetProperty( "VTK_TEXTURE_DIR_PATH" )->GetID() );
                }
            }
        }
		*/

		/*
        //Now load up the dataset
        {
            const std::string tempDataSetFilename =
                lastDataAdded->GetFileName();
            std::cout << "|\tLoading data for file "
                      << tempDataSetFilename
                      << std::endl;
            lastDataAdded->SetArrow(
                ves::xplorer::ModelHandler::instance()->GetArrow() );
            //Check and see if the data is part of a transient series
            if( tempInfoPacket->GetProperty( "VTK_TRANSIENT_SERIES" ) )
            {
                std::string precomputedSurfaceDir =
                    tempInfoPacket->GetProperty( "VTK_TRANSIENT_SERIES" )->
                    GetDataString();
                lastDataAdded->LoadTransientData( precomputedSurfaceDir );
                std::vector< lfx::core::vtk::DataSetPtr > dataSetVector = lastDataAdded->GetTransientDataSets();
                for( size_t i = 0; i < dataSetVector.size(); ++i )
                {
                    ves::xplorer::event::data::WriteDatabaseEntry( dataSetVector[ i ], tempInfoPacket );
                }
            }
            else
            {
                lastDataAdded->LoadData();
                std::vector< lfx::core::vtk::DataSetPtr > dataSetVector = lastDataAdded->GetChildDataSets();
                for( size_t i = 0; i < dataSetVector.size(); ++i )
                {
                    ves::xplorer::event::data::WriteDatabaseEntry( dataSetVector[ i ], tempInfoPacket );
                }
                ves::xplorer::event::data::WriteDatabaseEntry( lastDataAdded, tempInfoPacket );
            }
            //If the data load failed
            if( !lastDataAdded->GetDataSet() )
            {
                std::cout << "|\tData failed to load." << std::endl;
                activeModel->DeleteDataSet( tempDataSetFilename );
            }
            else
            {
                std::cout << "|\tData is loaded for file "
                          << tempDataSetFilename
                          << std::endl;
                if( lastDataAdded->GetParent() == lastDataAdded )
                {
                    activeModel->GetDCS()->
                        addChild( lastDataAdded->GetDCS() );
                    activeModel->SetActiveDataSet( lfx::core::vtk::DataSetPtr() );
                }
                switchwire::SingleShotSignal< void, const std::string& >( "LoadDatasetFromFile.DatafileLoaded", tempDataSetFilename );
            }
        }
        //////////////////////////////////////////////////////////////
        if( tempInfoPacket->GetProperty( "Create Surface Wrap" ) )
        {
            unsigned int surfaceToggle = 0;
            tempInfoPacket->GetProperty( "Create Surface Wrap" )->
                GetData( surfaceToggle );
            if( surfaceToggle )
            {
                //Create surface
                lastDataAdded->CreateSurfaceWrap();
            }
        }
			*/
    }


}
////////////////////////////////////////////////////////////////////////////////
void LoadSurfaceFiles( std::string precomputedSurfaceDir )
{
    if( precomputedSurfaceDir.empty() )
    {
        return;
    }

    ves::xplorer::Model* _activeModel =
        ModelHandler::instance()->GetActiveModel();

    vprDEBUG( vesDBG, 1 ) << "|\tLoading surface files from "
                          << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;

#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    boost::filesystem::path dir_path( precomputedSurfaceDir );
#else
    boost::filesystem::path dir_path( precomputedSurfaceDir, boost::filesystem::no_check );
#endif
    if( boost::filesystem::is_directory( dir_path ) )
    {
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
        std::cout << "|\tIn directory: "
                  << dir_path.string() << "\n";
#else
        std::cout << "|\tIn directory: "
                  << dir_path.native_directory_string() << "\n";
#endif
        boost::filesystem::directory_iterator end_iter;
        for( boost::filesystem::directory_iterator dir_itr( dir_path );
                dir_itr != end_iter; ++dir_itr )
        {
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
            const std::string filenameString = dir_itr->path().string();
#else
            const std::string filenameString = dir_itr->leaf();
#endif
            try
            {
                if( boost::filesystem::is_directory( *dir_itr ) )
                {
                    std::cout << "|\tIs a sub directory " << filenameString << " [directory]\n";
                }
                else
                {
                    std::cout << filenameString << "\n";
                    if( strstr( filenameString.c_str(), ".vtk" ) )
                    {
                        std::string pathAndFileName;
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
                        pathAndFileName.assign( filenameString );
#else
                        pathAndFileName.assign( dir_path.leaf().c_str() );
                        pathAndFileName.append( "/" );
                        pathAndFileName.append( filenameString.c_str() );
#endif
                        vprDEBUG( vesDBG, 0 ) << "|\tsurface file = " << pathAndFileName
                                              << std::endl << vprDEBUG_FLUSH;

                        _activeModel->CreateCfdDataSet();
                        ///This code needs updated BADLY
                        ///There should be much better logic here.
                        unsigned int numDataSets = _activeModel->GetNumberOfCfdDataSets();
                        // subtract 1 because this number was 1 base not 0 base
                        numDataSets -= 1;
                        _activeModel->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

                        // set the dcs matrix the same as the last file
                        _activeModel->GetCfdDataSet( -1 )->SetDCS(
                            _activeModel->GetCfdDataSet( ( int )( numDataSets - 1 ) )->GetDCS() );

                        // precomputed data that descends from a flowdata.vtk should
                        // automatically have the same color mapping as the "parent"
                        _activeModel->GetCfdDataSet( -1 )->SetParent(
                            _activeModel->GetCfdDataSet( ( int )( numDataSets - 1 ) )->GetParent() );
                    }
                }
            }
            catch( const std::exception& ex )
            {
                std::cout << filenameString << " " << ex.what() << std::endl;
            }
        }
    }
    else // must be a file
    {
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
        std::cout << "\nFound: " << dir_path.string() << "\n";
#else
        std::cout << "\nFound: " << dir_path.native_file_string() << "\n";
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
}}}} // namespace
