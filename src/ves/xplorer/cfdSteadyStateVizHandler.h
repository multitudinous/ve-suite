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
#ifndef CFD_STEADYSTATEVIZHANDLER_H
#define CFD_STEADYSTATEVIZHANDLER_H
/*!\file cfdSteadyStateVizHandler.h
cfdSteadyStateVizHandler API
*/
/*!\class VE_Xplorer::cfdSteadyStateVizHandler
* 
*/
#include <ves/VEConfig.h>

#include <VE_Xplorer/SceneGraph/DCS.h>

#include <vector>
#include <map>

#include <vpr/Thread/Thread.h>
#include <vpr/Util/Singleton.h>

#include <vrj/vrjParam.h>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

class vtkPolyData;

namespace VE_Xplorer
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
   class cfdGlobalBase;
   class cfdObjects;
   class cfdCommandArray;
   class cfdCursor;
   class cfdGraphicsObject;
   class cfdModel;
   class cfdTextOutput;
}

namespace VE_SceneGraph
{
   class DCS;
}

namespace VE_XML
{
   class Command;
}

namespace VE_EVENTS
{
   class EventHandler;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdSteadyStateVizHandler //: public vpr::Singleton< cfdSteadyStateVizHandler >
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdSteadyStateVizHandler >;
	///Constructor
   cfdSteadyStateVizHandler( void );
   //cfdSteadyStateVizHandler(const cfdSteadyStateVizHandler& o) { ; }
   //cfdSteadyStateVizHandler& operator=(const cfdSteadyStateVizHandler& o) { ; }
   ///Destructor
   ~cfdSteadyStateVizHandler( void );
   ///????
   vprSingletonHeader( cfdSteadyStateVizHandler );   

public:
   ///Initialize the sshandler claass
   void Initialize( std::string );
   ///Destructor functions since destructors don't get called yet
   //void CleanUp( void );
   ///Called once by cfdApp to create any necessary objects
   void InitScene( void );
   ///The standard preframe function
   void PreFrameUpdate( void );
   ///The thread function used to create geodes and actors
#if __VJ_version > 2000003
   void CreateActorThread( void );
#elif __VJ_version == 2000003
   void CreateActorThread( void * );
#endif
   ///The function used to create streamlines
   void streamers( void );
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
   void ClearVisObjects( void );
   
   // Helper functions
   void SetCommandArray( cfdCommandArray* );
   //VE_SceneGraph::cfdTempAnimation* GetActiveAnimation( void );
   bool TransientGeodesIsBusy();

private:
   cfdPolyData*         surface;
   cfdIsosurface*       isosurface;
   cfdContour*          contour;
   cfdPresetContour*    x_contour;
   cfdPresetContour*    y_contour;
   cfdPresetContour*    z_contour;
   cfdContours*         x_contours;
   cfdContours*         y_contours;
   cfdContours*         z_contours;
   cfdMomentum*         momentum;
   cfdPresetMomentum*   x_momentum;
   cfdPresetMomentum*   y_momentum;
   cfdPresetMomentum*   z_momentum;
   cfdMomentums*        x_momentums;
   cfdMomentums*        y_momentums;
   cfdMomentums*        z_momentums;
   cfdVector*           vector;
   cfdPresetVector*     x_vector;
   cfdPresetVector*     y_vector;
   cfdPresetVector*     z_vector;
   cfdVectors*          x_vectors;
   cfdVectors*          y_vectors;
   cfdVectors*          z_vectors;
   cfdStreamers*        streamlines;
   cfdPolyData*         particles;
   cfdImage*            image;
   cfdAnimatedImage*    animImg;
   cfdAnimatedStreamlineCone* animStreamer;
   cfdTextOutput*       textOutput;
   // Common objects for all functions
   cfdCommandArray*  commandArray;
	osg::ref_ptr< VE_SceneGraph::DCS > _activeDataSetDCS;
   cfdObjects* _activeObject;
   //VE_SceneGraph::cfdTempAnimation* _activeTempAnimation;

   // Classes and variables for multithreading.
   vpr::Thread* vjTh[1];

   // Vectors that will eventually be stored as maps
   // these hold all the objectsa for easy access and management
   //std::vector< cfdObjects* > dataList;
   std::vector< cfdGlobalBase* > commandList;

   std::string _param;
   bool actorsAreReady;
   bool computeActorsAndGeodes;
   bool changeGeometry;
   bool texturesActive;
   vtkPolyData*   lastSource;
   cfdCursor*     cursor;
   // Stores data from cfdCursor
   // Variable will eventually be used to define bounding box
   // for data interagation
   double cur_box[6];

   // Need to get rid of this bool fix 
   //bool inter_activeObject;
   //bool chgMod;
   //bool runStreamersThread;
   bool runIntraParallelThread;
   bool useLastSource;
   bool transientBusy;
   bool transientActors;

   /// multi map to hold graphics objects
   /// the key is the viz type and the value is cfdGraphicsObject
   std::multimap< int, cfdGraphicsObject* > graphicsObjects;
   std::map< std::string,VE_EVENTS::EventHandler*> _eventHandlers;///<The event handler for commands.
};
}
#endif
