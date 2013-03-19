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

#include <ves/xplorer/data/DatasetPropertySet.h>

#include <string>
#include <vector>

#include <latticefx/core/vtk/DataSet.h>

#include <osgwTools/Quat.h>

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
        dcs->setAttitude( osgwTools::makeHPRQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );
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

    activeModel->DeleteDataSet( dataFilename );
    activeModel->SetActiveDataSet( lfx::core::vtk::DataSetPtr() );
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
void WriteDatabaseEntry( lfx::core::vtk::DataSetPtr dataSet )
{
    xplorer::data::DatasetPropertySet set;

    //boost::filesystem::path tempPath( fileName );
    //std::string shortName = tempPath.filename().string();

    //set.LoadByKey( "Filename", shortName );
    set.LoadByKey( "Filename", dataSet->GetFileName() );

    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_DATA_NODE" );
    descriptorsList.push_back( set.GetUUIDAsString() );
    dataSet->GetDCS()->setDescriptions( descriptorsList );

    //set.SetPropertyValue( "Filename", shortName );
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

    set.Save();
}
////////////////////////////////////////////////////////////////////////////////

}
}
}
}
