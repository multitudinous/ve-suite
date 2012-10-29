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

#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/util/RescaleTextureVisitor.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/event/viz/cfdVectorBase.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/ActiveModelEventHandler.h>

#include <ves/xplorer/event/cad/CADAddNodeEH.h>
#include <ves/xplorer/event/cad/CADAnimationEH.h>
#include <ves/xplorer/event/cad/CADAddAttributeEH.h>
#include <ves/xplorer/event/cad/CADDeleteNodeEH.h>
#include <ves/xplorer/event/cad/CADInitializePhysicsEventHandler.h>
#include <ves/xplorer/event/cad/CADMoveNodeEventHandler.h>
#include <ves/xplorer/event/cad/CADPhysicsMeshEventHandler.h>
#include <ves/xplorer/event/cad/CADPhysicsPropertiesEventHandler.h>
#include <ves/xplorer/event/cad/CADRemoveAttributeEH.h>
#include <ves/xplorer/event/cad/CADSetActiveAttributeEH.h>
#include <ves/xplorer/event/cad/CADSetNameEH.h>
#include <ves/xplorer/event/cad/CADSetRootNodeEH.h>
#include <ves/xplorer/event/cad/CADToggleEH.h>
#include <ves/xplorer/event/cad/CADTransformEH.h>
#include <ves/xplorer/event/cad/MaterialUpdateEH.h>
#include <ves/xplorer/event/cad/MaterialModeUpdateEH.h>
#include <ves/xplorer/event/cad/CADSetOpacityEventHandler.h>
#include <ves/xplorer/event/cad/TogglePluginsEventHandler.h>
#include <ves/xplorer/event/cad/NavigateToEventHandler.h>
#include <ves/xplorer/event/cad/OcclusionSettingsEventHandler.h>
#include <ves/xplorer/event/cad/CADSlotInitializer.h>

#include <ves/xplorer/event/data/AddVTKDataSetEventHandler.h>
#include <ves/xplorer/event/data/AxesEventHandler.h>
#include <ves/xplorer/event/data/AxesLabelsEventHandler.h>
#include <ves/xplorer/event/data/BBoxEventHandler.h>
#include <ves/xplorer/event/data/DataTransformEH.h>
#include <ves/xplorer/event/data/ScalarBarEventHandler.h>
#include <ves/xplorer/event/data/WireframeEventHandler.h>
#include <ves/xplorer/event/data/ActiveDataSetEventHandler.h>
#include <ves/xplorer/event/data/DataSlots.h>

#include <ves/xplorer/event/environment/SoundActivateEH.h>
#include <ves/xplorer/event/environment/SoundAddNewEH.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/Command.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>
#include <ves/xplorer/volume/cfdTextureManager.h>
using namespace ves::xplorer::volume;

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

//#include <vtkPolyDataWriter.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyData.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <fstream>
#include <sstream>
#include <algorithm>

#include <osgDB/Registry>

vprSingletonImpLifetime( ves::xplorer::ModelHandler, 1 );
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::util;

ModelHandler::ModelHandler()
    :
    activeDataset( 0 ),
    activeCommand( ves::open::xml::CommandPtr() ),
    _activeModel( 0 ),
    _activeTDSet( 0 ),
    arrow( 0 ),
    m_rescaleCADEntityTextures( false ),
    m_CADSlotInitializer( new ves::xplorer::event::cad::CADSlotInitializer )
{
    vprDEBUG( vesDBG, 2 ) << "ModelHandler constructor"
                          << std::endl << vprDEBUG_FLUSH;

    _eventHandlers[ std::string( "SET_ROOT_CAD_NODE" )] =
        new ves::xplorer::event::CADSetRootNodeEventHandler();
    _eventHandlers[ std::string( "CAD_TRANSFORM_UPDATE" )] =
        new ves::xplorer::event::CADTransformEventHandler();
    _eventHandlers[ std::string( "CAD_ADD_ANIMATION_TO_NODE" )] =
        new ves::xplorer::event::CADAnimationEventHandler();
    _eventHandlers[ std::string( "CAD_ADD_NODE" )] =
        new ves::xplorer::event::CADAddNodeEventHandler();
    _eventHandlers[ std::string( "CAD_DELETE_NODE" )] =
        new ves::xplorer::event::CADDeleteNodeEventHandler();
    _eventHandlers[ std::string( "CAD_ADD_ATTRIBUTE_TO_NODE" )] =
        new ves::xplorer::event::CADAddAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_SET_ACTIVE_ATTRIBUTE_ON_NODE" )] =
        new ves::xplorer::event::CADSetActiveAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_SET_NODE_NAME" )] =
        new ves::xplorer::event::CADSetNameEventHandler();
    _eventHandlers[ std::string( "CAD_TOGGLE_NODE" )] =
        new ves::xplorer::event::CADToggleEventHandler();
    _eventHandlers[ std::string( "CHANGE_ACTIVE_MODEL" )] =
        new ves::xplorer::event::ActiveModelEventHandler();
    _eventHandlers[ std::string( "CHANGE_ACTIVE_DATASET" )] =
        new ves::xplorer::event::data::ActiveDataSetEventHandler();
    _eventHandlers[ std::string( "Optimize CAD" )] =
        new ves::xplorer::event::ActiveModelEventHandler();
    _eventHandlers[ std::string( "CAD_ATTRIBUTE_MATERIAL_UPDATE" )] =
        new ves::xplorer::event::MaterialUpdateEventHandler();
    _eventHandlers[ std::string( "CAD_ATTRIBUTE_MATERIAL_MODE" )] =
        new ves::xplorer::event::MaterialModeUpdateEventHandler();
    _eventHandlers[ std::string( "CAD_REMOVE_ATTRIBUTE" )] =
        new ves::xplorer::event::CADRemoveAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_MOVE_NODE" )] =
        new ves::xplorer::event::CADMoveNodeEventHandler();
    _eventHandlers[ std::string( "UPDATE_MODEL_DATASETS" )] =
        new ves::xplorer::event::AddVTKDataSetEventHandler();
    _eventHandlers[ std::string( "Change Bounding Box State" )] =
        new ves::xplorer::event::BBoxEventHandler();
    _eventHandlers[ std::string( "Change Wire Frame State" )] =
        new ves::xplorer::event::WireframeEventHandler();
    _eventHandlers[ std::string( "Change Axes State" )] =
        new ves::xplorer::event::AxesEventHandler();
    _eventHandlers[ std::string( "Change Axes Labels" )] =
        new ves::xplorer::event::AxesLabelsEventHandler();
    _eventHandlers[ std::string( "Change Scalar Bar State" )] =
        new ves::xplorer::event::ScalarBarEventHandler();
    _eventHandlers[ std::string( "DATA_TRANSFORM_UPDATE" )] =
        new ves::xplorer::event::DataTransformEventHandler();
    _eventHandlers[ std::string( "Enable/Disable Sound" )] =
        new ves::xplorer::event::SoundActivateEventHandler();
    _eventHandlers[ std::string( "Add New Sound" )] =
        new ves::xplorer::event::SoundAddNewEventHandler();
    _eventHandlers[ std::string( "INITIALIZE_PHYSICS" )] =
        new ves::xplorer::event::CADInitializePhysicsEventHandler();
    _eventHandlers[ std::string( "PHYSICS_MESH" )] =
        new ves::xplorer::event::CADPhysicsMeshEventHandler();
    _eventHandlers[ std::string( "PHYSICS_PROPERTIES" )] =
        new ves::xplorer::event::CADPhysicsPropertiesEventHandler();
    _eventHandlers[ std::string( "CAD_OPACITY_UPDATE" )] =
        new ves::xplorer::event::CADSetOpacityEventHandler();
    _eventHandlers[ std::string( "Xplorer Toggle Plugin Events" )] =
        new ves::xplorer::event::cad::TogglePluginsEventHandler();
    _eventHandlers[ std::string( "Move to cad" )] =
        new ves::xplorer::event::cad::NavigateToEventHandler();
    _eventHandlers[ std::string( "Culling Settings" )] =
        new ves::xplorer::event::cad::OcclusionSettingsEventHandler();

    // Register signal(s) with EventManager
    switchwire::EventManager::instance()->RegisterSignal(
        ( &mActiveModelChangedSignal ),
        "ModelHandler.ActiveModelChangedSignal" );

    CONNECTSIGNALS_STATIC( "%SetContourPlaneGreyscale", void( std::string const&, std::vector< bool > const& ),
                           &ves::xplorer::event::data::SetContourPlaneGreyscale,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TransformDataNode", void( std::string const&, std::vector< double > const& ),
                           &ves::xplorer::event::data::TransformDatasetNode,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%SetDatasetSurfaceWrap", void( std::string const&, bool const& ),
                           &ves::xplorer::event::data::SetDatasetSurfaceWrap,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%TBETAddScalarSignal",
                           void( std::string const&, std::string const& ),
                           &ves::xplorer::event::data::AddTextureDataset,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%ToggleDataNode",
                           void( std::string const&, bool const& ),
                           &ves::xplorer::event::data::ToggleCADNode,
                           m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%DeleteDataSet",
                          void( std::string const& ),
                          &ves::xplorer::event::data::DeleteDataSet,
                          m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNAL_1( "ChangeActiveModel",
                     void( std::string const& ),
                     &ModelHandler::SetActiveModel,
                     m_connections, normal_Priority );

    CONNECTSIGNALS_1( "%dbPresent",
                      void( bool const& ),
                      &ModelHandler::SetDBPresent,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
ModelHandler::~ModelHandler()
{
    m_filenameToCADMap.clear();
    m_animationCADMap.clear();

    for( size_t i = 0; i < _modelList.size(); ++i )
    {
        delete _modelList.at( i );
    }
    _modelList.clear();

    if( this->arrow )
    {
        this->arrow->Delete();
        arrow = 0;
    }

    for( std::map< std::string , ves::xplorer::event::EventHandler* >::iterator
            itr = _eventHandlers.begin(); itr != _eventHandlers.end(); ++itr )
    {
        delete itr->second;
        itr->second = 0;
    }
    _eventHandlers.clear();

    osgDB::Registry::instance( true );
}
///////////////////////
// Helper functions
///////////////////////
////////////////////////////////////////////////////////////////////////////////
cfdTextureDataSet* ModelHandler::GetActiveTextureDataSet()
{
    return _activeTDSet;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::SetActiveModel( std::string const& modelNumber )
{
    _activeModel = 0;

    for( size_t i = 0; i < _modelList.size(); i++ )
    {
        if( modelNumber == _modelList.at( i )->GetID() )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tModelHandler::SetActiveModel : "
                                  << modelNumber
                                  << " is set." << std::endl << vprDEBUG_FLUSH;
            _activeModel = _modelList.at( i );
            mActiveModelChangedSignal.signal( modelNumber );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
Model* ModelHandler::GetModel( int i )
{
    if( _modelList.empty() )
    {
        return NULL;
    }

    return _modelList.at( i );
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::AddModel( Model* const input )
{
    _modelList.push_back( input );
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::RemoveModel( Model* const modelToBeRemoved )
{
    for( std::vector< Model* >::iterator iter = _modelList.begin();
            iter != _modelList.end(); )
    {
        if( *iter == modelToBeRemoved )
        {
            _modelList.erase( iter++ );
            delete modelToBeRemoved;
            vprDEBUG( vesDBG, 1 ) << "|\tModelHandler::RemoveModel "
                                  << " Model Removal Successful" << std::endl << vprDEBUG_FLUSH;
            return;
        }
        else
        {
            ++iter;
        }
        // The above code is from : The C++ Standard Library by:Josuttis
    }
    vprDEBUG( vesDBG, 1 ) << "|\tModelHandler::RemoveModel "
                          << " Model Removal Failed" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
Model* ModelHandler::GetActiveModel()
{
    return _activeModel;
}
////////////////////////////////////////////////////////////////////////////////
int ModelHandler::GetNumberOfModels()
{
    return static_cast< int >( _modelList.size() );
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* ModelHandler::_GetArrowPolyData()
{
    //ripped from cfdArrow in the Utilities/arrowCreator directory
    float shaftAngleIncrement = ( 3.14159265 / 1.5 );
    float tipAngleIncrement = ( 3.14159265 / 1.5 );
    int tipResolution = 3;
    int shaftResolution = 3;
    float shaftRadius = 0.03;
    float tipRadius = 0.10;
    float tipLength = 0.35;
    int numCells = 6;

    float vertex[3];
    vtkIdType vertexList[4];     // make large enough for rectangles
    vtkPolyData* arrow = vtkPolyData::New();

    int memSize = numCells;
    arrow->Allocate( numCells, memSize );


    // Work on the arrow head
    int i;
    vtkPoints* points = vtkPoints::New();
    for( i = 0; i < tipResolution; i++ )
    {
        float angle = tipAngleIncrement * float( i );

        //generate and store points for each triangle of the arrow head
        vertex[0] = -tipLength;
        vertex[1] = tipRadius * sin( angle );
        vertex[2] = tipRadius * cos( angle );
        vertexList[0] = points->InsertNextPoint( vertex );

        vertex[0] = -tipLength;
        vertex[1] = tipRadius * sin( angle + tipAngleIncrement );
        vertex[2] = tipRadius * cos( angle + tipAngleIncrement );
        vertexList[1] = points->InsertNextPoint( vertex );

        vertex[0] = 0.0;
        vertex[1] = 0.0;
        vertex[2] = 0.0;
        vertexList[2] = points->InsertNextPoint( vertex );

        arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );

        if( tipResolution == 2 ) // generate crossed polygon and exit...
        {
            vertex[0] = -tipLength;
            vertex[1] = tipRadius;
            vertex[2] = 0.0;
            vertexList[0] = points->InsertNextPoint( vertex );

            vertex[0] = -tipLength;
            vertex[1] = -tipRadius;
            vertex[2] = 0.0;
            vertexList[1] = points->InsertNextPoint( vertex );

            vertex[0] = 0.0;
            vertex[1] = 0.0;
            vertex[2] = 0.0;
            vertexList[2] = points->InsertNextPoint( vertex );

            arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );
            break;
        }
    }

    // Work on the shaft
    for( i = 0; i < shaftResolution; i++ )
    {
        float angle = shaftAngleIncrement * float( i );

        // generate and store points for each rectangle of the shaft
        vertex[0] = -1.0;
        vertex[1] = shaftRadius * sin( angle );
        vertex[2] = shaftRadius * cos( angle );
        vertexList[0] = points->InsertNextPoint( vertex );

        vertex[0] = -tipLength;
        vertex[1] = shaftRadius * sin( angle );
        vertex[2] = shaftRadius * cos( angle );
        vertexList[1] = points->InsertNextPoint( vertex );

        vertex[0] = -tipLength;
        vertex[1] = shaftRadius * sin( angle + shaftAngleIncrement );
        vertex[2] = shaftRadius * cos( angle + shaftAngleIncrement );
        vertexList[2] = points->InsertNextPoint( vertex );

        vertex[0] = -1.0;
        vertex[1] = shaftRadius * sin( angle + shaftAngleIncrement );
        vertex[2] = shaftRadius * cos( angle + shaftAngleIncrement );
        vertexList[3] = points->InsertNextPoint( vertex );

        arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );

        if( shaftResolution == 2 ) // generate crossed polygon and exit...
        {
            vertex[0] = -1.0;
            vertex[1] = shaftRadius;
            vertex[2] = 0.0;
            vertexList[0] = points->InsertNextPoint( vertex );

            vertex[0] = -tipLength;
            vertex[1] = shaftRadius;
            vertex[2] = 0.0;
            vertexList[1] = points->InsertNextPoint( vertex );

            vertex[0] = -tipLength;
            vertex[1] = -shaftRadius;
            vertex[2] = 0.0;
            vertexList[2] = points->InsertNextPoint( vertex );

            vertex[0] = -1.0;
            vertex[1] = -shaftRadius;
            vertex[2] = 0.0;
            vertexList[3] = points->InsertNextPoint( vertex );

            arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );
            break;
        }
    }

    arrow->SetPoints( points );

    //the normals
    vtkPolyDataNormals* arrowPolysWithNormals = vtkPolyDataNormals::New();
    arrowPolysWithNormals->SetInput( arrow );
    //Specify the angle that defines a sharp edge. If the difference in angle across neighboring
    //polygons is greater than this value, the shared edge is considered "sharp".
    arrowPolysWithNormals->SetFeatureAngle( 60 );
    arrowPolysWithNormals->Update();

    vtkPolyData* arrowPolys = vtkPolyData::New();
    arrowPolys->DeepCopy( arrowPolysWithNormals->GetOutput() );

    arrowPolysWithNormals->Delete();
    points->Delete();
    arrow->Delete();

    return arrowPolys;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::InitScene()
{

    this->arrow = _GetArrowPolyData();

    if( !arrow )
    {
        std::cerr << "Error: ModelHandler::InitScene()" << std::endl;
        std::cerr << "Couldn't create arrow polydata!!" << std::endl;
        exit( 1 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::PreFrameUpdate()
{
    activeCommand =
        ves::xplorer::command::CommandManager::instance()->GetXMLCommand();
    if( activeCommand )
    {
        std::map<std::string, ves::xplorer::event::EventHandler* >::const_iterator
        currentEventHandler =
            _eventHandlers.find( activeCommand->GetCommandName() );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tModelHandler::PreFrameUpdate Executing: "
                                  << activeCommand->GetCommandName() << std::endl << vprDEBUG_FLUSH;

            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( activeCommand );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* ModelHandler::GetArrow()
{
    return this->arrow;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::RegisterAnimatedCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity )
{
    m_animationCADMap.insert(
        std::pair< std::string, ves::xplorer::scenegraph::CADEntity* const >(
            tempEntity->GetFilename(), tempEntity ) );
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::RegisterCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity )
{
    m_filenameToCADMap.insert(
        std::pair< std::string, ves::xplorer::scenegraph::CADEntity* const >(
            tempEntity->GetFilename(), tempEntity ) );
    m_rescaleCADEntityTextures = true;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntity* ModelHandler::IsCADFileLoaded( std::string const& filename )
{
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::const_iterator
    iter = m_filenameToCADMap.find( filename );
    if( iter != m_filenameToCADMap.end() )
    {
        return iter->second;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::UnregisterCADFile( ves::xplorer::scenegraph::CADEntity* const tempEntity )
{
    for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::iterator
            iter = m_filenameToCADMap.begin(); iter != m_filenameToCADMap.end(); ++iter )
    {
        if( iter->second == tempEntity )
        {
            m_filenameToCADMap.erase( iter );
            break;
        }
    }

    for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::iterator
            iter = m_animationCADMap.begin(); iter != m_animationCADMap.end(); ++iter )
    {
        if( iter->second == tempEntity )
        {
            m_animationCADMap.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::ContextPreDrawUpdate()
{
    if( m_rescaleCADEntityTextures )
    {
        vpr::Guard<vpr::Mutex> val_guard( mValueLock );

        for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::const_iterator
                iter = m_filenameToCADMap.begin(); iter != m_filenameToCADMap.end(); ++iter )
        {
            ves::xplorer::scenegraph::util::RescaleTextureVisitor
            textureVisitor( iter->second->GetDCS() );
        }
        m_rescaleCADEntityTextures = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::PauseCADAnimations()
{
    for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::iterator
            iter = m_animationCADMap.begin(); iter != m_animationCADMap.end(); ++iter )
    {
        osg::AnimationPathCallback* tempPath = dynamic_cast< osg::AnimationPathCallback* >( iter->second->GetDCS()->getUpdateCallback() );
        if( tempPath )
        {
            tempPath->setPause( true );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::PlayCADAnimations()
{
    for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::iterator
            iter = m_animationCADMap.begin(); iter != m_animationCADMap.end(); ++iter )
    {
        osg::AnimationPathCallback* tempPath = dynamic_cast< osg::AnimationPathCallback* >( iter->second->GetDCS()->getUpdateCallback() );
        if( tempPath )
        {
            tempPath->setPause( false );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::ResetCADAnimations()
{
    for( std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* const >::iterator
            iter = m_animationCADMap.begin(); iter != m_animationCADMap.end(); ++iter )
    {
        osg::AnimationPathCallback* tempPath = dynamic_cast< osg::AnimationPathCallback* >( iter->second->GetDCS()->getUpdateCallback() );
        if( tempPath )
        {
            tempPath->reset();
            tempPath->setPause( true );
            tempPath->update( *( static_cast< osg::Node* >( iter->second->GetDCS() ) ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::SetDBPresent( bool const& dbPresent )
{
    m_dbPresent = dbPresent;
}
////////////////////////////////////////////////////////////////////////////////
bool ModelHandler::GetDBPresent()
{
    return m_dbPresent;
}
////////////////////////////////////////////////////////////////////////////////
