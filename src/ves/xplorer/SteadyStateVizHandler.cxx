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
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/device/cfdCursor.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/xplorer/event/viz/cfdPresetMomentum.h>
#include <ves/xplorer/event/viz/cfdPolyData.h>
#include <ves/xplorer/event/viz/cfdIsosurface.h>
#include <ves/xplorer/event/viz/cfdPresetContour.h>
#include <ves/xplorer/event/viz/cfdContours.h>
#include <ves/xplorer/event/viz/cfdMomentum.h>
#include <ves/xplorer/event/viz/cfdMomentums.h>
#include <ves/xplorer/event/viz/cfdVector.h>
#include <ves/xplorer/event/viz/cfdPresetVector.h>
#include <ves/xplorer/event/viz/cfdVectors.h>
#include <ves/xplorer/event/viz/cfdStreamers.h>
#include <ves/xplorer/event/viz/cfdPolyData.h>
#include <ves/xplorer/event/viz/cfdImage.h>
#include <ves/xplorer/event/viz/cfdAnimatedImage.h>
#include <ves/xplorer/event/viz/cfdAnimatedStreamlineCone.h>
#include <ves/xplorer/event/viz/cfdContour.h>
#include <ves/xplorer/event/viz/cfdObjects.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/event/viz/cfdGraphicsObject.h>
#include <ves/xplorer/event/viz/ParticleAnimation.h>

#include <ves/xplorer/event/viz/CreateVisObjectEventHandler.h>
#include <ves/xplorer/event/viz/ClearVisObjectsEventHandler.h>
#include <ves/xplorer/event/viz/StreamLineEventHandler.h>
#include <ves/xplorer/event/viz/PolydataSurfaceEventHandler.h>
#include <ves/xplorer/event/viz/VectorEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- Juggler Includes --- //
#include <vpr/vpr.h>
#include <vpr/System.h>

#include <boost/bind.hpp>
#include <boost/algorithm/string/case_conv.hpp>

// --- VTK Includes --- //
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkAlgorithm.h>


#include <ves/xplorer/event/viz/VisFeatureMakerBase.h>

#include <ves/xplorer/event/viz/ContourFeatureMaker.h>
#include <ves/xplorer/event/viz/VectorFeatureMaker.h>
#include <ves/xplorer/event/viz/StreamlineFeatureMaker.h>
#include <ves/xplorer/event/viz/IsosurfaceFeatureMaker.h>
#include <ves/xplorer/event/viz/PolydataFeatureMaker.h>
#include <ves/xplorer/event/viz/VolumeVisFeatureMaker.h>

vprSingletonImpLifetime( ves::xplorer::SteadyStateVizHandler, 1 );

using namespace ves::xplorer::command;
using namespace ves::xplorer::event::viz;

namespace ves
{
namespace xplorer
{

////////////////////////////////////////////////////////////////////////////////
SteadyStateVizHandler::SteadyStateVizHandler()
    :
    m_vizThread( 0 ),
    actorsAreReady( false ),
    computeActorsAndGeodes( false ),
    texturesActive( false ),
    lastSource( 0 ),
    cursor( 0 ),
    useLastSource( false ),
    transientActors( true ),
    m_logger( Poco::Logger::get( "xplorer.SteadyStateVizHandler" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    vtkAlgorithm::SetDefaultExecutivePrototype( prototype );
    prototype->Delete();

    _eventHandlers[ std::string( "VISUALIZATION_SETTINGS" )] =
        new ves::xplorer::event::CreateVisObjectEventHandler();
    _eventHandlers[ std::string( "CLEAR_VIS_OBJECTS" )] =
        new ves::xplorer::event::ClearVisObjectsEventHandler();
    _eventHandlers[ std::string( "DELETE_OBJECT_FROM_NETWORK" )] =
        new ves::xplorer::event::ClearVisObjectsEventHandler();
    _eventHandlers[ std::string( "LIVE_STREAMLINE_UPDATE" )] =
        new ves::xplorer::event::StreamLineEventHandler();
    _eventHandlers[ std::string( "LIVE_POLYDATA_UPDATE" )] =
        new ves::xplorer::event::PolydataSurfaceEventHandler();
    _eventHandlers[ std::string( "LIVE_VECTOR_UPDATE" )] =
        new ves::xplorer::event::VectorEventHandler();

    ///Create ciz object factory
    CreateVizObjectMap();

    ///Setup slots
    CONNECTSIGNALS_1( "%DeleteVizFeature",
                      void( std::string const & activModelID ),
                      &SteadyStateVizHandler::DeleteVizFeature,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_2( "%AddVizFeature",
                      void( std::string const & activModelID, std::string const & tableName ),
                      &SteadyStateVizHandler::AddVizFeature,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_2( "%HideVizFeature",
                      void( std::string const&, std::vector< bool > const& ),
                      &SteadyStateVizHandler::HideVizFeature,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
SteadyStateVizHandler::~SteadyStateVizHandler()
{
    for( VisObjectConstIter iter = m_visObjectMap.begin();
            iter != m_visObjectMap.end(); ++iter )
    {
        delete iter->second;
    }
    m_visObjectMap.clear();

    runIntraParallelThread = false;

    std::map< std::string, ves::xplorer::event::EventHandler* >::iterator pos;
    for( pos = _eventHandlers.begin(); pos != _eventHandlers.end(); )
    {
        delete pos->second;
        _eventHandlers.erase( pos++ );
    }

    try
    {
        m_vizThread->join();
    }
    catch( ... )
    {
        ;//do nothing
    }
    delete m_vizThread;

    vtkAlgorithm::SetDefaultExecutivePrototype( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::HideVizFeature( const std::string& uuid, const std::vector< bool >& onOff )
{
    LOG_DEBUG( "HideVizFeature = " << uuid );
    graphics_objects_map::iterator hashIter = m_graphicsObjectMap.find( vpr::GUID( uuid ) );
    if( hashIter != m_graphicsObjectMap.end() )
    {
        std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > >& geode =
            hashIter->second->GetGeodes();
        geode.back().get()->setNodeMask( !onOff.at( 0 ) );
    }
    else
    {
        LOG_WARNING( "HideVizFeature: Unable to find relevant viz feature." );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::DeleteVizFeature( std::string const& featureUUID )
{
    LOG_DEBUG( "DeleteVizFeature = " << featureUUID );
    graphics_objects_map::iterator hashIter = m_graphicsObjectMap.find( vpr::GUID( featureUUID ) );
    if( hashIter != m_graphicsObjectMap.end() )
    {
        m_graphicsObjectMap.erase( hashIter );
    }
    else
    {
        LOG_WARNING( "DeleteVizFeature: Unable to find relevant viz feature." );
    }

    for( std::multimap< int, cfdGraphicsObject* >::iterator
            itr = graphicsObjects.begin();
            itr != graphicsObjects.end(); ++itr )
    {
        if( itr->second->GetUUID() == featureUUID )
        {
            itr->second->RemoveGeodeFromDCS();
            delete itr->second;
            graphicsObjects.erase( itr );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::AddVizFeature( std::string const& featureUUID, std::string const& tableName )
{
    LOG_INFO( "AddVizFeature = " << featureUUID << " " << tableName );

    using namespace ves::conductor;
    VisFeatureMakerBasePtr feature;
    if( tableName == "ContourPlane" )
    {
        LOG_INFO( "UpdateFeature: Updating ContourFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new ContourFeatureMaker() );
    }
    else if( tableName == "VectorPlane" )
    {
        LOG_INFO( "UpdateFeature: Updating VectorFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new VectorFeatureMaker() );
    }
    else if( tableName == "Streamline" )
    {
        LOG_INFO( "UpdateFeature: Updating StreamlineFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new StreamlineFeatureMaker() );
    }
    else if( tableName == "Isosurface" )
    {
        LOG_INFO( "UpdateFeature: Updating IsosurfaceFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new IsosurfaceFeatureMaker() );
    }
    else if( tableName == "Texture-based" )
    {
        LOG_INFO( "UpdateFeature: Updating TextureBasedFeatureMaker" );
    }
    else if( tableName == "Polydata" )
    {
        LOG_INFO( "UpdateFeature: Updating PolydataFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new PolydataFeatureMaker() );
    }
    else if( tableName == "VolumeVis" )
    {
        LOG_INFO( "UpdateFeature: Updating VolumeVisFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new VolumeVisFeatureMaker() );
    }

    feature->Update( featureUUID );
}
////////////////////////////////////////////////////////////////////////////////
bool SteadyStateVizHandler::TransientGeodesIsBusy()
{
    //return transientBusy;
    return computeActorsAndGeodes;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< cfdGraphicsObject* > SteadyStateVizHandler::GetGraphicsObjectsOfType( cfdGeodeEnum type )
{
    std::vector< cfdGraphicsObject* > cfdGraphicsObjects;

    //Search map for other object types with the same type as this one
    std::multimap< int, cfdGraphicsObject* >::iterator itr;
    for( itr = graphicsObjects.lower_bound( type );
            itr != graphicsObjects.upper_bound( type ); ++itr )
    {
        if( itr->second )
        {
            cfdGraphicsObjects.push_back( itr->second );
        }
    }

    return cfdGraphicsObjects;
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::SetActiveVisObject( cfdObjects* tempObject )
{
    m_visObjectQueue.push( tempObject );
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::SetComputeActorsAndGeodes( bool actorsAndGeodes )
{
    computeActorsAndGeodes = actorsAndGeodes;
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::SetActorsAreReady( bool actorsReady )
{
    actorsAreReady = actorsReady;
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::ClearVisObjects()
{
    vprDEBUG( vesDBG, 2 ) << "|\tClear All Graphics Objects From Scene Graph"
                          << std::endl << vprDEBUG_FLUSH;

    for( std::multimap< int, cfdGraphicsObject* >::iterator
            pos = graphicsObjects.begin(); pos != graphicsObjects.end(); ++pos )
    {
        pos->second->RemoveGeodeFromDCS();
        delete pos->second;
    }
    graphicsObjects.clear();
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::InitScene()
{
    //This set of thread stuff needs to be in ssvizhandler and transvizhandler
    std::cout << "| Initializing............................................. Threads |" << std::endl;
    runIntraParallelThread = true;
    //static_cast<void (App::*)( MyEvent& )>(&App::OnEvent)
    m_vizThread = new vpr::Thread( boost::bind( static_cast< void ( SteadyStateVizHandler::* )() >( &SteadyStateVizHandler::CreateActorThread ), boost::ref( *this ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::PreFrameUpdate()
{
    //Process the current command form the gui
    if( CommandManager::instance()->GetXMLCommand() )
    {
        std::map< std::string, ves::xplorer::event::EventHandler* >::iterator currentEventHandler;
        const ves::open::xml::CommandPtr tempCommand =
            CommandManager::instance()->GetXMLCommand();
        currentEventHandler = _eventHandlers.find( tempCommand->GetCommandName() );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tExecuting: "
                                  << tempCommand->GetCommandName()
                                  << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
    }

    //Check any virtual objects need to be updated
    if( actorsAreReady && transientActors )
    {
        vprDEBUG( vesDBG, 3 ) << "|\tUpdating Viz Feature"
                              << std::endl << vprDEBUG_FLUSH;

        cfdObjects* tempVisObject = m_visObjectSGQueue.front();
        m_visObjectSGQueue.pop();
        if( tempVisObject->GetUpdateFlag() )
        {
            //Set the update flag in steadystate viz handler
            //now update the vis object
            ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->SetVal( 0 );
            vprDEBUG( vesDBG, 2 ) << "|\tCreating Viz Feature"
                                  << std::endl << vprDEBUG_FLUSH;
            // if object needs updated then already have a graphics object
            cfdGraphicsObject* const temp = new cfdGraphicsObject();
            temp->SetTypeOfViz( cfdGraphicsObject::CLASSIC );
            temp->SetParentNode( tempVisObject->GetActiveDataSet()->GetDCS() );
            temp->SetDataSet( tempVisObject->GetActiveDataSet() );
            temp->SetActiveModel( ModelHandler::instance()->GetActiveModel() );
            temp->SetWorldNode( ModelHandler::instance()->GetActiveModel()->GetDCS() );
            temp->SetGeodes( tempVisObject );
            temp->AddGraphicsObjectToSceneGraph();

            //Search map for other object types with the same type as this one
            /*for( std::multimap< int, cfdGraphicsObject* >::iterator
                pos = graphicsObjects.lower_bound( tempVisObject->GetObjectType() );
                    pos != graphicsObjects.upper_bound( tempVisObject->GetObjectType() ); )
            {
                //and see if they have the same parent node
                //the parent node is unique becaue each dataset has a dcs
                if( pos->second->GetParentNode() == temp->GetParentNode() )
                {
                    pos->second->RemoveGeodeFromDCS();
                    delete pos->second;

                    graphicsObjects.erase( pos++ );
                }
                else
                {
                    ++pos;
                }
            }*/

            //First check to see if we are updating a feature or if this is
            //a brand new feature
            DeleteVizFeature( temp->GetUUID() );

            graphicsObjects.insert( std::make_pair( tempVisObject->GetObjectType(), temp ) );
            graphics_objects_map::value_type p = std::make_pair( vpr::GUID( temp->GetUUID() ), temp );
            m_graphicsObjectMap.insert( p );
            /*std::cout << temp->GetUUID() << " " << p.first.toString() << std::endl;
            std::string vprUUID = p.first.toString();
            boost::algorithm::to_lower( vprUUID );
            if( temp->GetUUID() != vprUUID )
            {
                std::cout << "We have a UUID mismatch " << std::endl;
            }*/

            // Resetting these variables is very important
            tempVisObject->SetUpdateFlag( false );
            tempVisObject->ClearGeodes();
            vprDEBUG( vesDBG, 2 ) << "|\tDone Creating Objects"
                                  << std::endl << vprDEBUG_FLUSH;

        }
        delete tempVisObject;
        tempVisObject = 0;
        //This means the update flag was not set to true so the update failed
        //on the viz object. This should really be handled with an exception
        //in the viz object or a status enum flag in the viz objects.
        /*if( !tempVisObject )
        {
            delete tempVisObject;
            tempVisObject = 0;
        }*/

        if( m_visObjectSGQueue.empty() )
        {
            actorsAreReady = false;
        }

        /*else if( !computeActorsAndGeodes )
        {
            m_visObjectQueue.pop();
            delete tempVisObject;
            if( m_visObjectQueue.empty() )
            {
                actorsAreReady = false;
            }
        }*/
    }
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::CreateActorThread()
{
    //DO NOT put scene graph manipulation code in this function
    //This thread is purely for creation of geodes
    while( runIntraParallelThread )
    {
        while( !computeActorsAndGeodes && runIntraParallelThread )
        {
            vpr::System::msleep( 100 );
        }

        if( !runIntraParallelThread )
        {
            return;
        }

        // Basically waiting for work here
        // This is a guard
        // Sample every half second
        if( !m_visObjectQueue.empty() )
        {
            cfdObjects* const tempVisObject = m_visObjectQueue.front();
            m_visObjectQueue.pop();

            //if( !tempVisObject->GetUpdateFlag() )
            {
                vprDEBUG( vesDBG, 0 ) << "|\tUpdating Graphics Data..."
                                      << std::endl << vprDEBUG_FLUSH;
                tempVisObject->Update();
                m_visObjectSGQueue.push( tempVisObject );
                SetActorsAreReady( true );
                vprDEBUG( vesDBG, 0 ) << "|\tDone updating Graphics Data"
                                      << std::endl << std::endl << vprDEBUG_FLUSH;
            }
        }

        if( m_visObjectQueue.empty() )
        {
            computeActorsAndGeodes = false;
        }
    } // End of While loop
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::CreateVizObjectMap()
{
    // Initialize all the vis objects from ssvishandler
    //
    std::cout << "| Initializing Viz Methods......................................... |" << std::endl;

    // Initiate the isosurface.
    //
    std::pair< std::string, std::pair< std::string, std::string > > objectType;
    objectType = std::make_pair(
                     std::string( "UPDATE_ISOSURFACE_SETTINGS" ), std::make_pair( "", "" ) );
    cfdIsosurface* isosurface = new cfdIsosurface( 10 );
    isosurface->SetObjectType( ISOSURFACE );
    m_visObjectMap[ objectType ] = isosurface;

    //
    // Initiate the interactive contour.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "wand" );
    objectType.second.second = std::string( "Single" );
    cfdContour* contour = new cfdContour();
    contour->SetObjectType( CONTOUR );
    m_visObjectMap[ objectType ] = contour;

    //
    // Initiate the interactive momentum.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "wand" );
    objectType.second.second = std::string( "Single-warp" );
    cfdMomentum* momentum = new cfdMomentum();
    momentum->SetObjectType( MOMENTUM );
    m_visObjectMap[ objectType ] = momentum;

    //
    // Initiate the interactive vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "wand" );
    objectType.second.second = std::string( "Single" );
    cfdVector* vector = new cfdVector();
    vector->SetObjectType( VECTOR );
    m_visObjectMap[ objectType ] = vector;

    //
    // Initiate the preset x contour.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Single" );
    cfdPresetContour* x_contour = new cfdPresetContour( 0, 10 );
    x_contour->SetObjectType( X_CONTOUR );
    m_visObjectMap[ objectType ] = x_contour;

    //
    // Initiate the preset y contour.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Single" );
    cfdPresetContour* y_contour = new cfdPresetContour( 1, 10 );
    y_contour->SetObjectType( Y_CONTOUR );
    m_visObjectMap[ objectType ] = y_contour;

    //
    // Initiate the preset z contour.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Single" );
    cfdPresetContour* z_contour = new cfdPresetContour( 2, 10 );
    z_contour->SetObjectType( Z_CONTOUR );
    m_visObjectMap[ objectType ] = z_contour;

    //
    // Initiate the surface contour.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "By Surface" );
    objectType.second.second = std::string( "Single" );
    ves::xplorer::cfdPresetContour* surface_contour =
        new cfdPresetContour( 2, 10 );
    surface_contour->SetObjectType( BY_SURFACE );
    m_visObjectMap[ objectType ] = surface_contour;

    //
    // Initiate the preset x momentum.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Single-warp" );
    // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
    cfdPresetMomentum* x_momentum = new cfdPresetMomentum( 0, 10 );
    x_momentum->SetObjectType( X_MOMENTUM );
    m_visObjectMap[ objectType ] = x_momentum;

    //
    // Initiate the preset y momentum.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Single-warp" );
    // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
    cfdPresetMomentum* y_momentum = new cfdPresetMomentum( 1, 10 );
    y_momentum->SetObjectType( Y_MOMENTUM );
    m_visObjectMap[ objectType ] = y_momentum;

    //
    // Initiate the preset z momentum.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Single-warp" );
    // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
    cfdPresetMomentum* z_momentum = new cfdPresetMomentum( 2, 10 );
    z_momentum->SetObjectType( Z_MOMENTUM );
    m_visObjectMap[ objectType ] = z_momentum;

    //
    // Initiate the preset x vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Single" );
    cfdPresetVector* x_vector = new cfdPresetVector( 0, 10 );
    x_vector->SetObjectType( X_VECTOR );
    m_visObjectMap[ objectType ] = x_vector;

    //
    // Initiate the preset y vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Single" );
    cfdPresetVector* y_vector = new cfdPresetVector( 1, 10 );
    y_vector->SetObjectType( Y_VECTOR );
    m_visObjectMap[ objectType ] = y_vector;

    //
    // Initiate the preset z vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Single" );
    cfdPresetVector* tempVector = new cfdPresetVector( 2, 10 );
    tempVector->SetObjectType( Z_VECTOR );
    m_visObjectMap[ objectType ] = tempVector;

    //
    // Initiate the preset z vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "All" );
    objectType.second.second = std::string( "Single" );
    cfdPresetVector* z_vector = new cfdPresetVector( 3, 10 );
    z_vector->SetObjectType( Z_VECTOR );
    m_visObjectMap[ objectType ] = z_vector;

    //
    // Initiate the surface vector.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "By Surface" );
    objectType.second.second = std::string( "Single" );
    ves::xplorer::cfdPresetVector* surface_vector =
        new cfdPresetVector( 2, 10 );
    surface_vector->SetObjectType( BY_SURFACE );
    m_visObjectMap[ objectType ] = surface_vector;

    //
    // Initiate the preset x contour lines.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Multiple" );
    cfdContours* x_contours = new cfdContours( 0 );
    x_contours->SetObjectType( X_CONTOURS );
    m_visObjectMap[ objectType ] = x_contours;

    //
    // Initiate the preset y contour lines.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Multiple" );
    cfdContours* y_contours = new cfdContours( 1 );
    y_contours->SetObjectType( Y_CONTOURS );
    m_visObjectMap[ objectType ] = y_contours;

    //
    // Initiate the preset z contour lines.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Multiple" );
    cfdContours* z_contours = new cfdContours( 2 );
    z_contours->SetObjectType( Z_CONTOURS );
    m_visObjectMap[ objectType ] = z_contours;

    //
    // Initiate the preset x momentums.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Multiple-warp" );
    cfdMomentums* x_momentums = new cfdMomentums( 0 );
    x_momentums->SetObjectType( X_MOMENTUMS );
    m_visObjectMap[ objectType ] = x_momentums;

    //
    // Initiate the preset y momentums.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Multiple-warp" );
    cfdMomentums* y_momentums = new cfdMomentums( 1 );
    y_momentums->SetObjectType( Y_MOMENTUMS );
    m_visObjectMap[ objectType ] = y_momentums;

    //
    // Initiate the preset z momentums.
    //
    objectType.first = std::string( "UPDATE_SCALAR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Multiple-warp" );
    cfdMomentums* z_momentums = new cfdMomentums( 2 );
    z_momentums->SetObjectType( Z_MOMENTUMS );
    m_visObjectMap[ objectType ] = z_momentums;

    //
    // Initiate the preset x vectors.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "x" );
    objectType.second.second = std::string( "Multiple" );
    cfdVectors* x_vectors = new cfdVectors( 0 );
    x_vectors->SetObjectType( X_VECTORS );
    m_visObjectMap[ objectType ] = x_vectors;

    //
    // Initiate the preset y vectors.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "y" );
    objectType.second.second = std::string( "Multiple" );
    cfdVectors* y_vectors = new cfdVectors( 1 );
    y_vectors->SetObjectType( Y_VECTORS );
    m_visObjectMap[ objectType ] = y_vectors;

    //
    // Initiate the preset z vectors.
    //
    objectType.first = std::string( "UPDATE_VECTOR_SETTINGS" );
    objectType.second.first = std::string( "z" );
    objectType.second.second = std::string( "Multiple" );
    cfdVectors* z_vectors = new cfdVectors( 2 );
    z_vectors->SetObjectType( Z_VECTORS );
    m_visObjectMap[ objectType ] = z_vectors;

    //
    // Initiate the streamlines.
    //
    objectType.first = std::string( "UPDATE_STREAMLINE_SETTINGS" );
    objectType.second.first = std::string( "" );
    objectType.second.second = std::string( "" );
    cfdStreamers* streamlines = new cfdStreamers();
    streamlines->SetObjectType( STREAMLINES );
    m_visObjectMap[ objectType ] = streamlines;

    //
    // Initiate the animated streamers.
    //
    objectType.first = std::string( "UPDATE_STREAMLINE_SETTINGS" );
    objectType.second.first = std::string( "animated" );
    objectType.second.second = std::string( "" );
    cfdAnimatedStreamlineCone* animStreamer = new cfdAnimatedStreamlineCone();
    animStreamer->SetObjectType( ANIMATED_STREAMLINES );
    animStreamer->SetStreamlineSource( streamlines );
    m_visObjectMap[ objectType ] = animStreamer;

    //
    // Initiate the animated Images.
    //
    //std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
    //animImg = new cfdAnimatedImage( _param.c_str() );
    //animImg->SetObjectType( ANIMATED_IMAGES );
    //dataList.push_back( animImg);

    //
    // Initiate the PolyData File
    //
    /*objectType.first = std::string( "UPDATE_PARTICLE_SETTINGS" );
     objectType.second.first = std::string( "" );
     objectType.second.second = std::string( "" );
     cfdPolyData* particles = new cfdPolyData();
     particles->SetObjectType( PARTICLES );
     m_visObjectMap[ objectType ] = particles;*/

    objectType.first = std::string( "UPDATE_POLYDATA_SETTINGS" );
    objectType.second.first = std::string( "" );
    objectType.second.second = std::string( "" );
    cfdPolyData* surface = new cfdPolyData();
    surface->SetObjectType( POLYDATA );
    m_visObjectMap[ objectType ] = surface;


    objectType.first = std::string( "UPDATE_POLYDATA_SETTINGS" );
    objectType.second.first = std::string( "PARTICLE_VIZ" );
    objectType.second.second = std::string( "" );
    ParticleAnimation* particleAnim = new ParticleAnimation();
    particleAnim->SetObjectType( PARTICLE_TRANSIENT );
    m_visObjectMap[ objectType ] = particleAnim;

    //
    // Initiate PIV data from INEL
    //
    //std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
    /*cfdImage* image = new cfdImage( _param );
     image->SetObjectType( IMAGE_EX );
     dataList.push_back( image ); */

    std::cout << "| Finished Initializing Viz Methods................................ |" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
cfdObjects* SteadyStateVizHandler::GetVizObject( VizKeyPair const& vizKey )
{
    VisObjectConstIter iter = m_visObjectMap.find( vizKey );
    if( iter == m_visObjectMap.end() )
    {
        LOG_WARNING( "Selected vis option is not in the GetVizObject." );
        return 0;
    }
    return iter->second->CreateCopy();
}
////////////////////////////////////////////////////////////////////////////////
} // end xplorer
} // end ves
