/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
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

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#endif

// --- VTK Includes --- //
class vtkPolyData;

// --- C/C++ Libraries --- //
#include <vector>
#include <map>

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

    //SteadyStateVizHandler( const SteadyStateVizHandler& o ) { ; }
    //SteadyStateVizHandler& operator=( const SteadyStateVizHandler& o ) { ; }
    ///Destructor
    ~SteadyStateVizHandler();

    ///????
    vprSingletonHeader( SteadyStateVizHandler );

public:
    ///Initialize the sshandler claass
    void Initialize( std::string );

    ///Destructor functions since destructors don't get called yet
    //void CleanUp();
    ///Called once by cfdApp to create any necessary objects
    void InitScene();

    ///The standard preframe function
    void PreFrameUpdate();

    ///The thread function used to create geodes and actors
#if __VJ_version > 2000003
    void CreateActorThread();
#elif __VJ_version == 2000003
    void CreateActorThread();
#endif

    ///The function used to create streamlines
    void streamers();

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
    cfdPolyData* surface;
    cfdIsosurface* isosurface;
    cfdContour* contour;
    cfdPresetContour* x_contour;
    cfdPresetContour* y_contour;
    cfdPresetContour* z_contour;
    cfdContours* x_contours;
    cfdContours* y_contours;
    cfdContours* z_contours;
    cfdMomentum* momentum;
    cfdPresetMomentum* x_momentum;
    cfdPresetMomentum* y_momentum;
    cfdPresetMomentum* z_momentum;
    cfdMomentums* x_momentums;
    cfdMomentums* y_momentums;
    cfdMomentums* z_momentums;
    cfdVector* vector;
    cfdPresetVector* x_vector;
    cfdPresetVector* y_vector;
    cfdPresetVector* z_vector;
    cfdVectors* x_vectors;
    cfdVectors* y_vectors;
    cfdVectors* z_vectors;
    cfdStreamers* streamlines;
    cfdPolyData* particles;
    cfdImage* image;
    cfdAnimatedImage* animImg;
    cfdAnimatedStreamlineCone* animStreamer;
    cfdTextOutput* textOutput;

    //Common objects for all functions
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _activeDataSetDCS;
    cfdObjects* _activeObject;
    //ves::xplorer::scenegraph::cfdTempAnimation* _activeTempAnimation;

    //Classes and variables for multithreading.
    vpr::Thread* vjTh[ 1 ];

    //Vectors that will eventually be stored as maps
    //these hold all the objectsa for easy access and management
    //std::vector< cfdObjects* > dataList;
    std::vector< GlobalBase* > commandList;

    std::string _param;
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
    //bool inter_activeObject;
    //bool chgMod;
    //bool runStreamersThread;
    bool runIntraParallelThread;
    bool useLastSource;
    bool transientBusy;
    bool transientActors;

    ///multi map to hold graphics objects
    ///the key is the viz type and the value is cfdGraphicsObject
    std::multimap< int, cfdGraphicsObject* > graphicsObjects;
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;///<The event handler for commands.

};
}
}

#endif // end STEADY_STATE_VIZ_HANDLER_H
