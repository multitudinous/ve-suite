/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef STEADY_STATE_VIZ_HANDLER_H
#define STEADY_STATE_VIZ_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/SteadyStateVizHandlerPtr.h>

#include <ves/xplorer/GlobalBasePtr.h>

#include <ves/xplorer/event/EventHandlerPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/CommandPtr.h>

// --- Juggler Includes --- //
#include <vrj/vrjParam.h>

#include <vpr/Thread/Thread.h>
#include <vpr/Util/Singleton.h>
#include <vpr/Util/GUID.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- VTK Includes --- //
class vtkPolyData;

// --- C/C++ Libraries --- //
#include <vector>
#include <queue>
#include <map>

#if defined(__GNUC__) && __GNUC__ >= 4
#  include <tr1/unordered_map>
#elif defined(_MSC_VER) && _MSC_VER >= 1500
#  include <unordered_map>
#endif

namespace ves
{
namespace xplorer
{

class cfdPolyData;
class cfdIsosurface;
class cfdPresetContour;
class cfdContours;
class cfdMomentum;
class cfdPresetMomentum;
class cfdMomentums;
class cfdVector;
class cfdPresetVector;
class cfdVectors;
class cfdStreamers;
class cfdPolyData;
class cfdImage;
class cfdAnimatedImage;
class cfdAnimatedStreamlineCone;
class cfdContour;
class cfdObjects;
class cfdCursor;
class cfdGraphicsObject;
class cfdTextOutput;

namespace scenegraph
{
class DCS;
}

/*!\file SteadyStateVizHandler.h
 * SteadyStateVizHandler API
 */

/*!\class ves::xplorer::SteadyStateVizHandler
 *
 */
class VE_XPLORER_EXPORTS SteadyStateVizHandler
{
private:
    //Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< SteadyStateVizHandler >;
    ///Constructor
    SteadyStateVizHandler();

    ///Destructor
    ~SteadyStateVizHandler();

    ///????
    vprSingletonHeader( SteadyStateVizHandler );

public:
    ///Destructor functions since destructors don't get called yet
    //void CleanUp();
    ///Called once by cfdApp to create any necessary objects
    void InitScene();

    ///The standard preframe function
    void PreFrameUpdate();

    ///The thread function used to create geodes and actors
    void CreateActorThread();

    ///The function used to create streamlines
    //void streamers();

    ///Set the active vis object - to be used by the addvis eh
    ///\param tempObject The active Object to be used by the handler
    void SetActiveVisObject( cfdObjects* tempObject );

    ///Set the flag to tell sshandler that actors and geodes need to be created
    ///- to be used by the addvis eh
    ///\param actorsAndGeodes Go create geodes and actors in sshandler
    void SetComputeActorsAndGeodes( bool actorsAndGeodes );

    ///Set the flag to tell sshandler that actors are ready
    ///- to be used by the addvis eh
    ///\param actorsReady Bool that tells sshandler to add geodes
    void SetActorsAreReady( bool actorsReady );

    ///Clear all the vis objects from the graphics objects list
    ///right now this must be done in ssvishandler
    ///because the map is a private member of ssvishandler and this is the
    ///easiest way to handle for the time being
    void ClearVisObjects();

    // Helper functions
    //ves::xplorer::scenegraph::cfdTempAnimation* GetActiveAnimation( void );
    bool TransientGeodesIsBusy();

    ///Return all cfdGraphicsObject of a certain type
    ///\param type The type
    std::vector< cfdGraphicsObject* > GetGraphicsObjectsOfType( cfdGeodeEnum type );

private:
    //Common objects for all functions
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _activeDataSetDCS;
    cfdObjects* _activeObject;
    ///A queue to stack active objects in to enable commands
    ///to send multiple vis objec requests
    std::queue< cfdObjects* > m_visObjectQueue;
    
    //ves::xplorer::scenegraph::cfdTempAnimation* _activeTempAnimation;

    //Classes and variables for multithreading.
    vpr::Thread* vjTh[ 1 ];

    bool actorsAreReady;
    bool computeActorsAndGeodes;
    bool changeGeometry;
    bool texturesActive;
    vtkPolyData* lastSource;
    cfdCursor* cursor;

    //Stores data from cfdCursor
    //Variable will eventually be used to define bounding box
    //for data interagation
    double cur_box[ 6 ];

    //Need to get rid of this bool fix
    bool runIntraParallelThread;
    bool useLastSource;
    bool transientBusy;
    bool transientActors;

    ///multi map to hold graphics objects
    ///the key is the viz type and the value is cfdGraphicsObject
    std::multimap< int, cfdGraphicsObject* > graphicsObjects;
    ///The event handler for commands.
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;
    ///The mp of graphics objects with unique ids to be tied to the ui
    typedef std::tr1::unordered_map<vpr::GUID, cfdGraphicsObject*, vpr::GUID::hash> graphics_objects_map;
    graphics_objects_map m_graphicsObjectMap;
};
}
}

#endif // end STEADY_STATE_VIZ_HANDLER_H
