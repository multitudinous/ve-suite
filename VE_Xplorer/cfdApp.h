/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdApp.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_APP_H
#define CFD_APP_H

#include <VjObs_i.h>     //added for corba stuff

#ifdef _TAO
#include <orbsvcs/CosNamingC.h>
#else
//#include <CorbaManager.h>
#endif

/// VR Juggler Stuff
#include <vrj/Kernel/Kernel.h>
//#include <gmtl/Math.h>
//#include <gmtl/Quat.h>
//#include <gmtl/Vec.h>
//#include <gmtl/Matrix.h>
//#include <gmtl/Coord.h>
//#include <gmtl/Generate.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>
#include <vpr/Thread/Thread.h>
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
#include <vrj/Util/Debug.h>
#include <vrj/Display/Projection.h>  /* for setNearFar (for setting clipping planes) */

/// C/C++ libraries
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
//#include <omp.h>
#ifndef WIN32
#include <sys/time.h>
#endif
#include <vector>

#ifdef _TAO
class cfdExecutive;
#endif

class cfdPfSceneManagement;
class cfdEnvironmentHandler;
class cfdSteadyStateVizHandler;
class cfdTransientVizHandler;
class cfdModelHandler;
class cfdIHCCModel;

// Scene graph dependent forward declarations
class pfGroup;

using namespace vrj;
using namespace gmtl;
using namespace gadget;

// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

// Declare my application class
#ifdef TABLET
class cfdApp : public vrj::PfApp, public VjObs_i
#else
class cfdApp : public vrj::PfApp
#endif
{
   public:
      //cfdApp( vrj::Kernel* kern);
      cfdApp( );//vrj::Kernel* kern );

      virtual void init( );

      virtual void apiInit( );

      // Called After pfInit()
      virtual void preForkInit( );
      // Called Before pfConfig()

      // Initialize the scene graph
      virtual void initScene( );

      // Return the current scene graph
      virtual pfGroup* getScene( );

      // Function called by the DEFAULT drawChan function 
      // before clearing the channel
      // and drawing the next frame (pfFrame( ))
      //virtual void preDrawChan(pfChannel* chan, void* chandata);

      // Function called before pfSync
      virtual void preSync( );

      // Function called after pfSync and before pfDraw
      virtual void preFrame( );

      // Function called after pfDraw
      virtual void intraFrame( );

      // Function called after intraFrame
      virtual void postFrame();

      // Performer calls before exiting
      virtual void exit( void );

      // Used to override getFrameBufferAttrs()
      // Should be able to set multi sampling in the config
      // Look for a fix in future juggler releases
      //std::vector< int > getFrameBufferAttrs( void );

      // Computes streamlines
      void streamers( void * ); 

      void intraSerialThread( void * );

      void intraParallelThread( void * );

      void NavigationForIHCC( void );

      void pushDataToStateInfo( void );

      void SetCORBAVariables( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr );

   cfdPfSceneManagement*      _sceneManager;
   cfdEnvironmentHandler*     _environmentHandler;
   cfdSteadyStateVizHandler*  _steadystateHandler;
   cfdTransientVizHandler*    _transientHandler;
   cfdModelHandler*          _modelHandler;
   // Objects defined in virtual environment space.
   /*cfdPolyData       *surface;
   cfdMenu           *menu;
   cfdLaser          *laser;
   cfdNavigate       *nav;
   cfdCursor         *cursor;
   cfdContour        *contour;
   cfdPresetContour  *x_contour;
   cfdPresetContour  *y_contour;
   cfdPresetContour  *z_contour;
   cfdContours       *x_contours;
   cfdContours       *y_contours;
   cfdContours       *z_contours;
   cfdMomentum       *momentum;
   cfdPresetMomentum *x_momentum;
   cfdPresetMomentum *y_momentum;
   cfdPresetMomentum *z_momentum;
   cfdMomentums      *x_momentums;
   cfdMomentums      *y_momentums;
   cfdMomentums      *z_momentums;
   cfdVector         *vector;
   cfdPresetVector   *x_vector;
   cfdPresetVector   *y_vector;
   cfdPresetVector   *z_vector;
   cfdVectors        *x_vectors;
   cfdVectors        *y_vectors;
   cfdVectors        *z_vectors;
   cfdReadParam      *paramReader;
   cfdStreamers      *streamlines;
   cfdPolyData       *particles;
   cfdImage          *image;  
   cfdAnimation      *transientSequence;
   cfdTeacher        *teacher;
   cfdScalarBarActor *scalarBarActor;
   cfdAnimatedStreamlineCone  *animStreamer;
   cfdAnimatedImage           *animImg;
   cfdIsosurface              *isosurface;
   cfdQuatCamHandler          *quatcamHandler;
   textPrompt                 *tPrompt;
   //cfdDashboard*     dashBoard;*/
   cfdIHCCModel               *ihccModel;
#ifdef _TAO
   cfdExecutive*     executive;
#endif

   CosNaming::NamingContext_var naming_context;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   //std::vector< cfdSound * > sounds;
   //std::vector< cfdFILE * > geomL;

   
   //biv -- transient stuff
   /*cfdTransientFlowManager* _cfdTFM_X_Contour[2];//added for windshield hack
   cfdTransientFlowManager* _cfdTFM_Y_Contour;
   cfdTransientFlowManager* _cfdTFM_Z_Contour;
   cfdTransientFlowManager* _cfdTFM_X_Vector;
   cfdTransientFlowManager* _cfdTFM_Y_Vector;
   cfdTransientFlowManager* _cfdTFM_Z_Vector;
   cfdTransientFlowManager* _cfdTFM_Geometry[2];
   cfdTransientFlowManager* _cfdTFM_Particle;*/

/*   // Used for defining the vectors for data display
   vtkPolyData * arrow;

   // Text processing stuff: needs to be reimplemented with a little thought
   void flush_text(char *);

   time_t my_timer;
   double time_r;
   double tt; //for recording time
   char * prompt_text;
   pfNode *temp_text;
   int text_sig;

   // pfb file counter: used to create new pfb names
   int  pfb_count;
 
   // Geometry and opacity controls
   bool  chgMod;
   bool  changeGeometry;
*/   
   // Stores data from cfdCursor
   // Variable will eventually be used to define bounding box
   // for data interagation
   double cur_box[6];

   // Only used in preframe for transient stuff
   int   lastFrame;

   // A hack for multi - model stuff with streamlines
   // will probably disappear in the future
   //int   useLastSource;
   //vtkPolyData * lastSource;

   // Thread state flags
   bool  runStreamersThread;
   bool  runIntraParallelThread;   
   bool  interactiveObject;
   bool  computeActorsAndGeodes;

   // Scalar bar flag
   bool  isTimeToUpdateScalarBar;
   
   int   cursorId;
   
   // Used to store data for multi-dataset functions
   char oldDatasetName[256];

   // State Variables
   short cfdNumScalars;       // intial stuff
   short cfdNumVectors;       // intial stuff
   short cfdNumGeoArrays;     // intial stuff
   int   cfdClients;          // intial stuff
   short cfdNumTeacherArrays; // intial stuff
   bool  actorsAreReady;

#ifndef TABLET
  // cfdApp side variables
   int   cfdIso_value;
   int   cfdSc;
   int   cfdMin;
   int   cfdMax;
   long  cfdId;
   long  cfdGeo_state;
   short cfdPostdata_state;
   bool  cfdPre_state;
   short cfdTimesteps;
   short cfdTeacher_state; 
#endif

#ifdef _CLUSTER   
   virtual void GetUpdateClusterStateVariables( void );
#endif
   //biv -- the write traverser
   protected:
      //cfdWriteTraverser* _cfdWT;

   private:
   char * filein_name;
};

#endif
