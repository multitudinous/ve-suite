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
 * Version:       $Revision: 1.63 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PF_NAV_H
#define PF_NAV_H

//biv -- added for non-corba build
//#ifdef TABLET
#include <VjObs_i.h>     //added for corba stuff
//#endif

#ifdef _TAO
#include <orbsvcs/CosNamingC.h>
#else
//#include <CorbaManager.h>
#endif

#include <vtkTimerLog.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkFieldData.h>

/// VR Juggler Stuff
#include <vrj/Kernel/Kernel.h>
#include <gmtl/Math.h>
#include <gmtl/Quat.h>
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>
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

#ifndef WIN32
#include <unistd.h>
#else
//#include <windows.h>
#endif
/// Performer libraries
#include <Performer/pr/pfGeoState.h>
#include <Performer/pf/pfChannel.h>
#include <Performer/pf/pfEarthSky.h>
#include <Performer/pf/pfLightSource.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfTraverser.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/pf.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr.h>
#include <Performer/pf/pfSwitch.h>
#ifndef WIN32
//biv--check here if build/run problems occur
#include <Performer/pfdb/pfiv.h>
#endif
#include <Performer/pr/pfLight.h>
#include <Performer/pf/pfSequence.h>

#include "cfdEnum.h"
#include "cfdGeode.h"
#include "cfdScalarBarActor.h"
#include "cfdCursor.h"

#include "cfdDataSet.h"
#include "cfdMenu.h"
#include "cfdLaser.h"
#include "cfdNavigate.h"
#include "cfdImage.h"
#include "cfdIsosurface.h"
#include "cfdContour.h"
#include "cfdMomentum.h"
#include "cfdVector.h"
#include "cfdPresetContour.h"
#include "cfdPresetMomentum.h"
#include "cfdPresetVector.h"
#include "cfdContours.h"
#include "cfdMomentums.h"
#include "cfdVectors.h"
#include "cfdStreamers.h"
#include "cfdAnimatedStreamlineCone.h"
#include "cfdAnimatedImage.h"
#include "cfdObjects.h"

#include "cfdTransientFlowManager.h"
#include "cfdAnimation.h"

#include "textPrompt.h"
#include "Plot3Dviewer.h"
#include "cfdReadParam.h"
#include "cfdFILE.h"

#include "cfdSound.h"
#include "cfdQuatCamHandler.h"

#include "cfdIHCCModel.h"
//biv-- the new write traverser class to handle
//cfdSequence nodes
#include "cfdWriteTraverser.h"

class cfdScalarBarActor;
//class cfdDashboard;
#ifdef _TAO
class cfdExecutive;
#endif
class cfdPolyData;
class cfdTeacher;
class cfdSound;

class vtkTransform;
class vtkTransformFilter;

using namespace vrj;
using namespace gmtl;
using namespace gadget;
using namespace snx;

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
#ifdef TABLET
      void SetCORBAVariables( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr );
#else
	  void setId( int x ){ this->cfdId = x; }
#endif

   //biv-- get inputs for navigation from the GUI
   void updateNavigationFromGUI();

   //biv -- the write traverser to write out perfly
   //compliant pfb
   void writePFBFile(pfNode* graph,char* fileName);

// protected:
   
   void RefreshScalarBar();
   
   // Performer objects - used to control the scene graph.
   pfLightModel  * sunModel;
   pfLightSource * sun,*lit;
   pfGeoState    * gstate;
   pfScene       * scene;
   pfGroup       * rootNode;  
   pfDCS         * worldDCS;
   pfDCS *activeDataSetDCS;

   // Classes and variables for multithreading.
   vpr::ThreadMemberFunctor<cfdApp> *vjThFunc[2];
   vpr::Thread *vjTh[2];
   //yang-REI, change the interface implementation
   
   //this is the mutex to prevent the preframe, 
   //intra_frame and intra_parallel to intervene each other
   vpr::Mutex inner_mutex;    
   
   // Objects defined in virtual environment space.
   cfdPolyData       *surface;
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
   cfdIHCCModel               *ihccModel;
   textPrompt                 *tPrompt;
   //cfdDashboard*     dashBoard;
#ifdef _TAO
   cfdExecutive*     executive;
#endif
#ifdef TABLET
   CosNaming::NamingContext_var naming_context;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
#endif
   std::vector< cfdSound * > sounds;
   std::vector< cfdFILE * > geomL;
   std::vector< cfdObjects * > dataList;

   cfdObjects *activeObject;
   cfdObjects *activeSequenceObject;
   
   //biv -- transient stuff
   cfdTransientFlowManager* _cfdTFM_X_Contour[2];//added for windshield hack
   cfdTransientFlowManager* _cfdTFM_Y_Contour;
   cfdTransientFlowManager* _cfdTFM_Z_Contour;
   cfdTransientFlowManager* _cfdTFM_X_Vector;
   cfdTransientFlowManager* _cfdTFM_Y_Vector;
   cfdTransientFlowManager* _cfdTFM_Z_Vector;
   cfdTransientFlowManager* _cfdTFM_Geometry[2];
   cfdTransientFlowManager* _cfdTFM_Particle;

   // Used for defining the vectors for data display
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
   
   // Stores data from cfdCursor
   // Variable will eventually be used to define bounding box
   // for data interagation
   double cur_box[6];

   // Only used in preframe for transient stuff
   int   lastFrame;

   // A hack for multi - model stuff with streamlines
   // will probably disappear in the future
   int   useLastSource;
   vtkPolyData * lastSource;

   // Thread state flags
   bool  runStreamersThread;
   bool  runIntraParallelThread;   
   bool  interactiveObject;
   bool  animatedStreamlines;
   bool  animatedImages;
   bool  computeActorsAndGeodes;

   // Scalar bar flag
   bool  isTimeToUpdateScalarBar;
   
   // Menu interaction variables
   bool  menuB;
   int   cursorId;
   
   // Used to store data for multi-dataset functions
   char oldDatasetName[256];

   // State Variables
   short cfdNumScalars;       // intial stuff
   short cfdNumVectors;       // intial stuff
   short cfdNumGeoArrays;     // intial stuff
   int   cfdClients;          // intial stuff
   short cfdNumTeacherArrays; // intial stuff

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
      cfdWriteTraverser* _cfdWT;

};

#endif
