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

#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/event/viz/cfdVectorBase.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/ActiveModelEventHandler.h>

#include <ves/xplorer/event/cad/CADAddNodeEH.h>
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

#include <ves/xplorer/event/data/AddVTKDataSetEventHandler.h>
#include <ves/xplorer/event/data/AxesEventHandler.h>
#include <ves/xplorer/event/data/AxesLabelsEventHandler.h>
#include <ves/xplorer/event/data/BBoxEventHandler.h>
#include <ves/xplorer/event/data/DataTransformEH.h>
#include <ves/xplorer/event/data/ScalarBarEventHandler.h>
#include <ves/xplorer/event/data/WireframeEventHandler.h>

#include <ves/xplorer/event/environment/SoundActivateEH.h>
#include <ves/xplorer/event/environment/SoundAddNewEH.h>

#include <ves/open/xml/Command.h>

#ifdef _OSG
#include <ves/xplorer/volume/cfdTextureDataSet.h>
#include <ves/xplorer/volume/cfdTextureManager.h>
using namespace ves::xplorer::volume;
#endif

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


#ifndef WIN32
// Needed for irix
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#endif

#ifdef WIN32
#include <direct.h>
#endif

vprSingletonImpLifetime( ves::xplorer::ModelHandler, 11 );
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::util;

ModelHandler::ModelHandler( void )
{
    vprDEBUG( vesDBG, 2 ) << "ModelHandler constructor"
    << std::endl << vprDEBUG_FLUSH;
    _param.erase();//_param = 0;

    activeDataset  = 0;
    //_scalarBar     = 0;
    arrow          = 0;
    //_readParam     = 0;
    _activeModel   = 0;
    activeCommand  = ves::open::xml::CommandPtr();

    //create null command to be returned when a command is not active
    nullCommand = ves::open::xml::CommandPtr( new ves::open::xml::Command() );
    nullCommand->SetCommandName( "NULL" );

    tbased = false;
    _eventHandlers[ std::string( "SET_ROOT_CAD_NODE" )] = new ves::xplorer::event::CADSetRootNodeEventHandler();
    _eventHandlers[ std::string( "CAD_TRANSFORM_UPDATE" )] = new ves::xplorer::event::CADTransformEventHandler();
    _eventHandlers[ std::string( "CAD_ADD_NODE" )] = new ves::xplorer::event::CADAddNodeEventHandler();
    _eventHandlers[ std::string( "CAD_DELETE_NODE" )] = new ves::xplorer::event::CADDeleteNodeEventHandler();
    _eventHandlers[ std::string( "CAD_ADD_ATTRIBUTE_TO_NODE" )] = new ves::xplorer::event::CADAddAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_SET_ACTIVE_ATTRIBUTE_ON_NODE" )] = new ves::xplorer::event::CADSetActiveAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_SET_NODE_NAME" )] = new ves::xplorer::event::CADSetNameEventHandler();
    _eventHandlers[ std::string( "CAD_TOGGLE_NODE" )] = new ves::xplorer::event::CADToggleEventHandler();
    _eventHandlers[ std::string( "CHANGE_ACTIVE_MODEL" )] = new ves::xplorer::event::ActiveModelEventHandler();
    _eventHandlers[ std::string( "CAD_ATTRIBUTE_MATERIAL_UPDATE" )] = new ves::xplorer::event::MaterialUpdateEventHandler();
    _eventHandlers[ std::string( "CAD_ATTRIBUTE_MATERIAL_MODE" )] = new ves::xplorer::event::MaterialModeUpdateEventHandler();
    _eventHandlers[ std::string( "CAD_REMOVE_ATTRIBUTE" )] = new ves::xplorer::event::CADRemoveAttributeEventHandler();
    _eventHandlers[ std::string( "CAD_MOVE_NODE" )] = new ves::xplorer::event::CADMoveNodeEventHandler();
    _eventHandlers[ std::string( "UPDATE_MODEL_DATASETS" )] = new ves::xplorer::event::AddVTKDataSetEventHandler();
    _eventHandlers[ std::string( "Change Bounding Box State" )] = new ves::xplorer::event::BBoxEventHandler();
    _eventHandlers[ std::string( "Change Wire Frame State" )] = new ves::xplorer::event::WireframeEventHandler();
    _eventHandlers[ std::string( "Change Axes State" )] = new ves::xplorer::event::AxesEventHandler();
    _eventHandlers[ std::string( "Change Axes Labels" )] = new ves::xplorer::event::AxesLabelsEventHandler();
    _eventHandlers[ std::string( "Change Scalar Bar State" )] = new ves::xplorer::event::ScalarBarEventHandler();
    _eventHandlers[ std::string( "DATA_TRANSFORM_UPDATE" )] = new ves::xplorer::event::DataTransformEventHandler();
    _eventHandlers[ std::string( "Enable/Disable Sound" )] = new ves::xplorer::event::SoundActivateEventHandler();
    _eventHandlers[ std::string( "Add New Sound" )] = new ves::xplorer::event::SoundAddNewEventHandler();
    _eventHandlers[ std::string( "INITIALIZE_PHYSICS" )] = new ves::xplorer::event::CADInitializePhysicsEventHandler();
    _eventHandlers[ std::string( "PHYSICS_MESH" )] = new ves::xplorer::event::CADPhysicsMeshEventHandler();
    _eventHandlers[ std::string( "PHYSICS_PROPERTIES" )] = new ves::xplorer::event::CADPhysicsPropertiesEventHandler();
    _eventHandlers[ std::string( "CAD_OPACITY_UPDATE" )] = new ves::xplorer::event::CADSetOpacityEventHandler();
    _eventHandlers[ std::string( "Xplorer Toggle Plugin Events" )] = new ves::xplorer::event::cad::TogglePluginsEventHandler();
    _eventHandlers[ std::string( "Move to cad" )] = new ves::xplorer::event::cad::NavigateToEventHandler();

#ifdef _OSG
    _activeTDSet = 0;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::Initialize( std::string param )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ModelHandler::~ModelHandler( void )
{
    //vprDEBUG(vesDBG,2) << "ModelHandler destructor"
    //                       << std::endl << vprDEBUG_FLUSH;

    m_filenameToCADMap.clear();

    for( size_t i = 0; i < _modelList.size(); ++i )
    {
        delete _modelList.at( i );
    }
    _modelList.clear();

    //if ( _scalarBar )
    //{
    //   delete _scalarBar;
    //vprDEBUG(vesDBG,2) << "delete _scalarBar"
    //   << std::endl << vprDEBUG_FLUSH;
    //}

    if( this->arrow )
    {
        this->arrow->Delete();
        arrow = 0;
        //vprDEBUG(vesDBG,2) << "this->arrow->Delete()"
        //   << std::endl << vprDEBUG_FLUSH;
    }

    for( std::map<std::string , ves::xplorer::event::EventHandler*>::iterator itr = _eventHandlers.begin();
            itr != _eventHandlers.end(); itr++ )
    {
        delete itr->second;
        itr->second = 0;
    }
    _eventHandlers.clear();
    //vprDEBUG(vesDBG,2) << "ModelHandler end destructor"
    //   << std::endl << vprDEBUG_FLUSH;
}

///////////////////////
// Helper functions
///////////////////////
/////////////////////////////////////////////////////////////
void ModelHandler::SetXMLCommand( const ves::open::xml::CommandPtr& inputCommand )
{
    activeCommand = inputCommand;
}
/////////////////////////////////////////////////////////////
const ves::open::xml::CommandPtr& ModelHandler::GetXMLCommand( void )
{
    return activeCommand;
}
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
#ifdef _OSG
cfdTextureDataSet* ModelHandler::GetActiveTextureDataSet()
{
    return _activeTDSet;
}
#endif
/////////////////////////////////////////////////////
//cfdScalarBarActor* ModelHandler::GetScalarBar(void)
//{
//   return _scalarBar;
//}
/////////////////////////////////////////////////////
void ModelHandler::SetActiveModel( int modelNumber )
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
            break;
        }
    }
}
/////////////////////////////////////////////
Model* ModelHandler::GetModel( int i )
{
    if( _modelList.empty() )
        return NULL;
    else
        return _modelList.at( i );
}
/////////////////////////////////////////////////
void ModelHandler::AddModel( Model* input )
{
    _modelList.push_back( input );
}
///////////////////////////////////////////////////////////////
void ModelHandler::RemoveModel( Model* modelToBeRemoved )
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
/////////////////////////////////////////////////
Model* ModelHandler::GetActiveModel( void )
{
    return _activeModel;
}
//////////////////////////////////////////////
int ModelHandler::GetNumberOfModels( void )
{
    return static_cast< int >( _modelList.size() );
}
///////////////////////////////////////////////////
vtkPolyData* ModelHandler::_GetArrowPolyData( )
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
    vtkPolyData * arrow = vtkPolyData::New();

    int memSize = numCells;
    arrow->Allocate( numCells, memSize );


    // Work on the arrow head
    int i;
    vtkPoints * points = vtkPoints::New();
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
    vtkPolyDataNormals * arrowPolysWithNormals = vtkPolyDataNormals::New();
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
///////////////////////////////////////
void ModelHandler::InitScene( void )
{

    this->arrow = _GetArrowPolyData();

    if( !arrow )
    {
        std::cerr << "Error: ModelHandler::InitScene()" << std::endl;
        std::cerr << "Couldn't create arrow polydata!!" << std::endl;
        exit( 1 );
    }
    //this->arrow->ShallowCopy( tempArrow->GetPolyData());

    for( unsigned int j = 0; j < _modelList.size(); j++ )
    {
        for( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++ )
        {
            std::cout << "|\tLoading data for file "
                << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
                << std::endl;
            _modelList.at( j )->GetCfdDataSet( i )->LoadData();
            _modelList.at( j )->GetCfdDataSet( i )->SetArrow( this->arrow );
            if( _modelList.at( j )->GetCfdDataSet( i )->GetParent() == _modelList.at( j )->GetCfdDataSet( i ) )
                ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->
                AddChild( _modelList.at( j )->GetCfdDataSet( i )->GetDCS() );
        }
    }

    // set default active dataset to be the meshed volume
    if( !_modelList.empty() )
    {
        _activeModel = _modelList.at( 0 );
        if( _modelList.at( 0 )->GetNumberOfCfdDataSets() > 0 )
        {
            activeDataset = _modelList.at( 0 )->GetCfdDataSet( 0 );
            _activeModel->SetActiveDataSet( activeDataset );
        }
#ifdef _OSG
        if( _modelList.at( 0 )->GetNumberOfTextureDataSets() > 0 )
        {
            _activeTDSet = _modelList.at( 0 )->GetTextureDataSet( 0 );
            _activeModel->SetActiveTextureDataSet( _activeTDSet );
        }
#endif
    }

    if( activeDataset != NULL )
    {
        // Fix this later - we need to check and see if this is already
        // done in DataSet upon initialization
        // set first scalar active
        activeDataset->SetActiveScalar( 0 );

        oldDatasetName.assign( activeDataset->GetFileName() );//strcpy( oldDatasetName, activeDataset->GetFileName() );
        vprDEBUG( vesDBG, 1 ) << "ModelHandler: Setting active dataset to "
        << activeDataset->GetFileName() << " , "
        << oldDatasetName << std::endl << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////
// PreFrameUpdate - Be sure to set the commandArray before calling this
// function.
/////////////////////////////////
void ModelHandler::PreFrameUpdate( void )
{
    if( activeCommand )
    {
        vprDEBUG( vesDBG, 3 ) << "|\tModelHandler::PreFrameUpdate Command Name : "
            << activeCommand->GetCommandName() << std::endl << vprDEBUG_FLUSH;
    
        std::map<std::string, ves::xplorer::event::EventHandler*>::iterator 
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
bool ModelHandler::GetVisOption()
{
    return tbased;
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* ModelHandler::GetArrow( void )
{
    return this->arrow;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::RegisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity )
{
    m_filenameToCADMap.insert(
        std::pair< std::string, ves::xplorer::scenegraph::CADEntity* >(
            tempEntity->GetFilename(), tempEntity ) );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntity* ModelHandler::IsCADFileLoaded( std::string filename )
{
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* >::iterator iter;
    iter = m_filenameToCADMap.find( filename );
    if( iter != m_filenameToCADMap.end() )
    {
        return iter->second;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void ModelHandler::UnregisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity )
{
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* >::iterator iter;
    for( iter = m_filenameToCADMap.begin(); iter != m_filenameToCADMap.end(); ++iter )
    {
        if( iter->second == tempEntity )
        {
            m_filenameToCADMap.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
