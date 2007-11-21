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

vprSingletonImpLifetime( ves::xplorer::SteadyStateVizHandler, 10 );

namespace ves
{
namespace xplorer
{

////////////////////////////////////////////////////////////////////////////////
SteadyStateVizHandler::SteadyStateVizHandler()
{
    //_activeDataSetDCS = 0;
    _activeObject = 0;
    lastSource = 0;
    //_activeTempAnimation = 0;

    computeActorsAndGeodes = false;
    actorsAreReady = false;
    useLastSource = false;
    texturesActive = false;
    transientActors = true;
    vjTh[ 0 ] = 0;
    _param.erase();

    _eventHandlers[ std::string( "VISUALIZATION_SETTINGS" ) ] = 
        new ves::xplorer::event::CreateVisObjectEventHandler();
    _eventHandlers[ std::string( "CLEAR_VIS_OBJECTS" ) ] = 
        new ves::xplorer::event::ClearVisObjectsEventHandler();
    _eventHandlers[ std::string( "DELETE_OBJECT_FROM_NETWORK" ) ] = 
        new ves::xplorer::event::ClearVisObjectsEventHandler();
    _eventHandlers[ std::string( "LIVE_STREAMLINE_UPDATE" ) ] = 
        new ves::xplorer::event::StreamLineEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::Initialize( std::string param )
{
    _param = param;
    cursor = EnvironmentHandler::instance()->GetCursor();
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

    if( vjTh[ 0 ] )
    {
        vjTh[ 0 ]->join();
        delete vjTh[ 0 ];
    }
}
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
        cfdGraphicsObjects.push_back( itr->second );
    }

    return cfdGraphicsObjects;
}
////////////////////////////////////////////////////////////////////////////////
/*
ves::xplorer::scenegraph::cfdTempAnimation* SteadyStateVizHandler::GetActiveAnimation()
{
    return _activeTempAnimation;
}
*/
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::SetActiveVisObject( cfdObjects* tempObject )
{
    _activeObject = tempObject;
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
    vprDEBUG( vesDBG,2 ) << "|\tClear All Graphics Objects From Scene Graph"
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
#if __VJ_version > 2000003
    vjTh[ 0 ] = new vpr::Thread( boost::bind( &SteadyStateVizHandler::CreateActorThread, this ) );
#elif __VJ_version == 2000003
    vjTh[ 0 ] = new vpr::Thread( new vpr::ThreadMemberFunctor< SteadyStateVizHandler >( 
        this, &SteadyStateVizHandler::CreateActorThread ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::PreFrameUpdate()
{
    //Process the current command form the gui
    //if( ModelHandler::instance()->GetActiveModel() )
    //{
    if( ModelHandler::instance()->GetXMLCommand()->GetCommandName().compare( "wait" ) )
    {
        std::map< std::string, ves::xplorer::event::EventHandler* >::iterator currentEventHandler;
        ves::open::xml::Command* tempCommand = ModelHandler::instance()->GetXMLCommand();
        currentEventHandler = _eventHandlers.find( tempCommand->GetCommandName() );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 0 ) << "|\tExecuting: "<< tempCommand->GetCommandName() 
                                  << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
    }
    //}

    /*
    if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_ACTIVE )
    {
        transientActors = (commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE )== 1)?true:false;
    }
    */

    //Check any virtual objects need to be updated
    if( actorsAreReady && transientActors )
    {
        vprDEBUG( vesDBG, 3 ) << "|\tUpdating Objects"
                              << std::endl << vprDEBUG_FLUSH;
        bool alreadyRemoved = false;
        //for ( unsigned int i = 0; i < dataList.size(); i++ )
        //{
        if( _activeObject->GetUpdateFlag() )// || 
            //dataList.at( i )->GetTransientGeodeFlag() )
        {  
            //Set the update flag in steadystate viz handler
            //now update the vis object
            ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->SetVal(0);
            vprDEBUG( vesDBG, 2 ) << "|\tCreating Objects"
                                  << std::endl << vprDEBUG_FLUSH;
            // if object needs updated then already have a graphics object
            cfdGraphicsObject* temp = new cfdGraphicsObject();
            temp->SetTypeOfViz( cfdGraphicsObject::CLASSIC );
            temp->SetParentNode( _activeObject->GetActiveDataSet()->GetDCS() );
            temp->SetActiveModel( ModelHandler::instance()->GetActiveModel() );
            temp->SetWorldNode( ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() );
            temp->SetGeodes( _activeObject );
            temp->AddGraphicsObjectToSceneGraph();

            //Search map for other object types with the same type as this one
            std::multimap< int, cfdGraphicsObject* >::iterator pos;
            for( pos = graphicsObjects.lower_bound( _activeObject->GetObjectType() ); 
                 pos != graphicsObjects.upper_bound( _activeObject->GetObjectType() ); )
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
            graphicsObjects.insert( std::make_pair( _activeObject->GetObjectType(), temp ) );

            // Resetting these variables is very important
            _activeObject->SetUpdateFlag( false );
            actorsAreReady = false;
            _activeObject->ClearGeodes();
            vprDEBUG( vesDBG, 2 ) << "|\tDone Creating Objects"
                                  << std::endl << vprDEBUG_FLUSH;

            /*if( ModelHandler::instance()->GetActiveModel()->GetMirrorDataFlag() )
            {
                //we mirror the dataset in two places
                //once here for data viz and once in modelhandler for geom
                ves::xplorer::scenegraph::Group* temp = ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->GetChild( 0 );
                ModelHandler::instance()->GetActiveModel()->SetMirrorNode( temp );
            }*/

            _activeObject = NULL;
        }


        //If we have selected a viz feature and it is complete the remove the text
        if( !computeActorsAndGeodes && !alreadyRemoved )
        {
            alreadyRemoved = true;
            //SceneManager::instance()->GetRootNode()->RemoveChild( textOutput->add_text( "executing..." ) );
        }
        //}
    }

    /*
    if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == USE_LAST_STREAMLINE_SEEDPOINTS )
    {
        useLastSource = ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )== 0 ) ? false : true;
    }
    */
    /*else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_DURATION )
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
    }*/
}
////////////////////////////////////////////////////////////////////////////////
#if __VJ_version > 2000003
void SteadyStateVizHandler::CreateActorThread()
#elif __VJ_version == 2000003
void SteadyStateVizHandler::CreateActorThread( void* )
#endif
{
    //DO NOT put scene graph manipulation code in this function
    //This thread is purely for creation of geodes
    while( runIntraParallelThread )
    {
        vpr::System::msleep( 500 );  // half-second delay

        // Basically waiting for work here
        // This is a guard 
        // Sample every half second
        if( computeActorsAndGeodes )
        {      
            if( _activeObject != NULL )
            {
                cfdContour* contourTest = 
                    dynamic_cast< cfdContour* >( _activeObject );
                cfdVector* vectorTest = 
                    dynamic_cast< cfdVector* >( _activeObject );
                cfdMomentum* momentumTest = 
                    dynamic_cast< cfdMomentum* >( _activeObject );
                cfdStreamers* streamersTest = 
                    dynamic_cast< cfdStreamers* >( _activeObject );
                cfdAnimatedStreamlineCone* animStreamerTest = 
                    dynamic_cast< cfdAnimatedStreamlineCone* >( _activeObject );
                cfdAnimatedImage* animImgTest = 
                    dynamic_cast< cfdAnimatedImage* >( _activeObject );

                vprDEBUG( vesDBG, 0 ) << " Updating cfdObject..." 
                                      << std::endl << vprDEBUG_FLUSH;

                // May replace later , fix a later date
                //vprDEBUG(vesDBG,2) << " Memory used before update ( bytes ) : "
                //                   << pfMemory::getArenaBytesUsed() << std::endl << vprDEBUG_FLUSH;

                //tt = GetTimeClock();
                if( contourTest == NULL && 
                    vectorTest == NULL &&
                    momentumTest == NULL &&   
                    //streamersTest == NULL &&
                    animStreamerTest == NULL &&
                    animImgTest == NULL )
                {
                    // For everything except for the interactive and transient stuff
                    vprDEBUG( vesDBG, 1 ) << "non-interactive object." << std::endl << vprDEBUG_FLUSH; 

                    _activeObject->Update();      
                    //_activeObject->SetSequence( 0 );
                }
                /*else if ( streamersTest != NULL )
                {
                vprDEBUG(vesDBG,1) << "interactive object." 
                   << std::endl << vprDEBUG_FLUSH;
                // if we are not already computing streamlines
                streamers();  
                }*/
                else if( animStreamerTest != NULL )
                {
                    // if we are not already computing animatedStreamlines
                    animStreamer->SetPolyDataSource( streamlines->GetStreamersOutput() );
                    animStreamer->Update();
                }
                else if( animImgTest != NULL )
                {
                    // if we are not already computing animatedImages
                    animImg->Update();
                }

                // May fix later, not a crucial part
                //vprDEBUG(vesDBG,1) << " Time: " << GetTimeClock()-tt
                //                       << std::endl << vprDEBUG_FLUSH;
                //vprDEBUG(vesDBG,2) <<" Memory used after update ( bytes ) : "
                //                       << pfMemory::getArenaBytesUsed() 
                //                       << std::endl << vprDEBUG_FLUSH;

                //_activeObject = NULL;
                computeActorsAndGeodes = false;   
                vprDEBUG( vesDBG, 0 ) << "|\tDone updating cfdObject" 
                                      << std::endl << std::endl << vprDEBUG_FLUSH; 

            }
        }
    } // End of While loop
}
////////////////////////////////////////////////////////////////////////////////
void SteadyStateVizHandler::streamers()
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
}
////////////////////////////////////////////////////////////////////////////////

} // end xplorer
} // end ves
