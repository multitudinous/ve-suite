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
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>

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
#include <ves/xplorer/event/viz/CreateVisObjectEventHandler.h>
#include <ves/xplorer/event/viz/ClearVisObjectsEventHandler.h>
#include <ves/xplorer/event/viz/StreamLineEventHandler.h>
#include <ves/xplorer/event/viz/PolydataSurfaceEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- Juggler Includes --- //
#include <vpr/vpr.h>
#include <vpr/System.h>

#include <boost/bind.hpp>

// --- VTK Includes --- //
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkAlgorithm.h>

vprSingletonImpLifetime( ves::xplorer::SteadyStateVizHandler, 10 );

namespace ves
{
namespace xplorer
{

////////////////////////////////////////////////////////////////////////////////
SteadyStateVizHandler::SteadyStateVizHandler()
    :
    _activeObject( 0 ),
    lastSource( 0 ),
    computeActorsAndGeodes( false ),
    actorsAreReady( false ),
    useLastSource( false ),
    texturesActive( false ),
    transientActors( true )
{
    vjTh[ 0 ] = 0;

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
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::Initialize( std::string param )
{
    _param = param;
    //cursor = EnvironmentHandler::instance()->GetCursor();
}
////////////////////////////////////////////////////////////////////////////////
SteadyStateVizHandler::~SteadyStateVizHandler()
{
    runIntraParallelThread = false;

    std::map< std::string, ves::xplorer::event::EventHandler* >::iterator pos;
    for( pos = _eventHandlers.begin(); pos != _eventHandlers.end(); )
    {
        delete pos->second;
        _eventHandlers.erase( pos++ );
    }

    try
    {
        vjTh[ 0 ]->join();
    }
    catch ( ... )
    {
        ;//do nothing
    }
    delete vjTh[ 0 ];
    
    vtkAlgorithm::SetDefaultExecutivePrototype( 0 );
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
    _activeObject = tempObject;
    m_visObjectQueue.push( _activeObject );
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

    std::multimap< int, cfdGraphicsObject* >::iterator pos;
    for( pos = graphicsObjects.begin(); pos != graphicsObjects.end(); )
    {
        pos->second->RemoveGeodeFromDCS();
        delete pos->second;

        graphicsObjects.erase( pos++ );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::InitScene()
{
    //This set of thread stuff needs to be in ssvizhandler and transvizhandler
    std::cout << "|  9. Initializing......................................... Threads |" << std::endl;
    runIntraParallelThread = true;
    vjTh[ 0 ] = new vpr::Thread( boost::bind( &SteadyStateVizHandler::CreateActorThread, this ) );
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::PreFrameUpdate()
{
    //Process the current command form the gui
    if( ModelHandler::instance()->GetXMLCommand()->GetCommandName().compare( "wait" ) )
    {
        std::map< std::string, ves::xplorer::event::EventHandler* >::iterator currentEventHandler;
        ves::open::xml::CommandPtr tempCommand = ModelHandler::instance()->GetXMLCommand();
        currentEventHandler = _eventHandlers.find( tempCommand->GetCommandName() );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 0 ) << "|\tExecuting: " 
                << tempCommand->GetCommandName()
                << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
    }

    //Check any virtual objects need to be updated
    if( actorsAreReady && transientActors )
    {
        vprDEBUG( vesDBG, 3 ) << "|\tUpdating Objects"
            << std::endl << vprDEBUG_FLUSH;

        cfdObjects* tempVisObject = m_visObjectQueue.front();
        if( tempVisObject->GetUpdateFlag() )
        {
            //Set the update flag in steadystate viz handler
            //now update the vis object
            ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->SetVal( 0 );
            vprDEBUG( vesDBG, 2 ) << "|\tCreating Objects"
                << std::endl << vprDEBUG_FLUSH;
            // if object needs updated then already have a graphics object
            cfdGraphicsObject* temp = new cfdGraphicsObject();
            temp->SetTypeOfViz( cfdGraphicsObject::CLASSIC );
            temp->SetParentNode( tempVisObject->GetActiveDataSet()->GetDCS() );
            temp->SetDataSet( tempVisObject->GetActiveDataSet() );
            temp->SetActiveModel( ModelHandler::instance()->GetActiveModel() );
            temp->SetWorldNode( ModelHandler::instance()->GetActiveModel()->GetDCS() );
            temp->SetGeodes( tempVisObject );
            temp->AddGraphicsObjectToSceneGraph();

            //Search map for other object types with the same type as this one
            std::multimap< int, cfdGraphicsObject* >::iterator pos;
            for( pos = graphicsObjects.lower_bound( tempVisObject->GetObjectType() );
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
            }
            graphicsObjects.insert( std::make_pair( tempVisObject->GetObjectType(), temp ) );

            // Resetting these variables is very important
            tempVisObject->SetUpdateFlag( false );
            tempVisObject->ClearGeodes();
            vprDEBUG( vesDBG, 2 ) << "|\tDone Creating Objects"
                << std::endl << vprDEBUG_FLUSH;

            m_visObjectQueue.pop();
            if( m_visObjectQueue.empty() )
            {
                actorsAreReady = false;
            }
        }
        //This means the update flag was not set to true so the update failed
        //on the viz object. This should really be handled with an exception
        //in the viz object or a status enum flag in the viz objects.
        else if( !computeActorsAndGeodes )
        {
            m_visObjectQueue.pop();
            if( m_visObjectQueue.empty() )
            {
                actorsAreReady = false;
            }            
        }
    }

    /*
    if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == USE_LAST_STREAMLINE_SEEDPOINTS )
    {
        useLastSource = ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )== 0 ) ? false : true;
    }
    */
    /*
    else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_DURATION )
    {
        std::multimap< int, cfdGraphicsObject* >::iterator pos;
        for(pos=graphicsObjects.begin(); pos!=graphicsObjects.end(); ++pos )
        {
            if( pos->second->GetAnimation() )
            {
                pos->second->GetAnimation()->SetDuration( 
                    commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
            }
        }
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::CreateActorThread()
{
    //DO NOT put scene graph manipulation code in this function
    //This thread is purely for creation of geodes
    while( runIntraParallelThread )
    {
        vpr::System::msleep( 100 );  // thenth-second delay

        // Basically waiting for work here
        // This is a guard
        // Sample every half second
        if( computeActorsAndGeodes )
        {
            if( _activeObject != NULL )
            {
                vprDEBUG( vesDBG, 0 ) << "|\tUpdating cfdObject..."
                    << std::endl << vprDEBUG_FLUSH;

                {
                    // For everything except for the interactive and transient stuff
                    vprDEBUG( vesDBG, 1 ) << "|\tNon-interactive object." << std::endl << vprDEBUG_FLUSH;

                    _activeObject->Update();
                }

                _activeObject = NULL;
                computeActorsAndGeodes = false;
                vprDEBUG( vesDBG, 0 ) << "|\tDone updating cfdObject"
                    << std::endl << std::endl << vprDEBUG_FLUSH;
            }
        }
    } // End of While loop
}
////////////////////////////////////////////////////////////////////////////////
/*void SteadyStateVizHandler::streamers()
{
    vprDEBUG( vesDBG, 1 ) << "In streamers" << std::endl << vprDEBUG_FLUSH;
    if( cursor->GetCursorID() == NONE )
    {
        _activeObject = NULL;
        return;
    }

    if( cursor->GetCursorID() == CUBE )
    {
        _activeObject->SetBoxSize( cur_box );
    }

    if( !useLastSource )
    {
        vprDEBUG( vesDBG, 1 ) << "creating fresh streamlines"
        << std::endl << vprDEBUG_FLUSH;
        if( lastSource != NULL )
        {
            lastSource->Delete();
        }

        lastSource = vtkPolyData::New();
        lastSource->DeepCopy( cursor->GetSourcePoints() );

        _activeObject->SetSourcePoints( lastSource );
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "using transformed last source"
        << std::endl << vprDEBUG_FLUSH;

        _activeObject->SetSourcePoints( lastSource );
    }

    _activeObject->Update();
    //_activeObject = NULL;
}*/
////////////////////////////////////////////////////////////////////////////////

} // end xplorer
} // end ves
