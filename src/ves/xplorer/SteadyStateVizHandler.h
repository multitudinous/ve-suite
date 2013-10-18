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
#ifndef STEADY_STATE_VIZ_HANDLER_H
#define STEADY_STATE_VIZ_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/GlobalBasePtr.h>

#include <ves/xplorer/event/EventHandlerPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/CommandPtr.h>

#include <ves/xplorer/Logging.h>

#include <switchwire/ScopedConnectionList.h>

#include <latticefx/core/PlayControl.h>

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
 * \class ves::xplorer::SteadyStateVizHandler
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

    ///Typdef
    typedef std::pair< std::string, std::pair< std::string, std::string > > VizKeyPair;

    typedef std::map< VizKeyPair, ves::xplorer::cfdObjects* >::const_iterator VisObjectConstIter;

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

    ///Delete the viz feature
    void DeleteVizFeature( std::string const& featureUUID );

    ///Add the viz feature
    void AddVizFeature( std::string const& featureUUID, std::string const& tableName );

    ///Hide a given viz feature by uuid
    void HideVizFeature( const std::string& uuid, const bool& onOff );

    ///Hide a viz feature by name or name pattern (SQL pattern syntax, not regex)
    void HideVizFeatureByName( const std::string& pattern, const bool& onOff );

    ///Create the map of viz objects
    void CreateVizObjectMap();

    ///Get the active object from the map
    ///\param vizKey The key to a specific viz object
    cfdObjects* GetVizObject( VizKeyPair const& vizKey );

    ///Get the bool providing the state of the lfx volume call
	bool GetLfxDataObjReady();
    ///Set the data triggering the creation of volume viz data
	void SetLfxDataObjReady( bool b, const std::string &uuid );

    ///Initialize the LFX cam
    void InitializeLfxCamera();

private:

	void ReadyLfxDataObj();

    ///Set the flag to tell sshandler that actors are ready
    ///- to be used by the addvis eh
    ///\param actorsReady Bool that tells sshandler to add geodes
    void SetActorsAreReady( bool actorsReady );

    //Common objects for all functions
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _activeDataSetDCS;
    ///A queue to stack active objects in to enable commands
    ///to send multiple vis objec requests
    std::queue< cfdObjects* > m_visObjectQueue;

    ///THe queue that holds the objects that need to be added to the sg
    std::queue< cfdObjects* > m_visObjectSGQueue;

    //Classes and variables for multithreading.
    vpr::Thread* m_vizThread;

    bool actorsAreReady;
    bool computeActorsAndGeodes;
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
    std::multimap< int, cfdGraphicsObject* > m_graphicsObjects;
    ///The event handler for commands.
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;
    ///The mp of graphics objects with unique ids to be tied to the ui
    typedef std::tr1::unordered_map<vpr::GUID, cfdGraphicsObject*, vpr::GUID::hash> graphics_objects_map;
    graphics_objects_map m_graphicsObjectMap;
    /// Required to be able to connect up to signals.
    switchwire::ScopedConnectionList m_connections;
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
    ///The container for all of the cfdObjects
    std::map< std::pair< std::string, std::pair< std::string, std::string > > , ves::xplorer::cfdObjects* > m_visObjectMap;
    ///Play controller
    lfx::core::PlayControlPtr m_playControl;
    ///Previous time tag
    double m_frameTime;
	/// LfxDataObject Ready
	bool m_lfxDataObjReady;
    ///The uuid of the lfx volume viz data
	std::string m_lfxuuid;
    ///The lfx volume camer
    osg::ref_ptr< osg::Camera > m_lfxCam;
};
}
}

#endif // end STEADY_STATE_VIZ_HANDLER_H
