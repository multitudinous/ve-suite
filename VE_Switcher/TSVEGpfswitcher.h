/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: TSVEGpfswitcher.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _SIMPLE_PF_NAV_APP_H_
#define _SIMPLE_PF_NAV_APP_H_

#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gmtl/Matrix.h>
#include <gmtl/Quat.h>
#include <gmtl/Vec.h>
#include <Performer/pr.h>
#include <Performer/pf/pfLightSource.h>
#include <Performer/pr/pfLinMath.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/prmath.h>

#include <StopWatch.h>
#include <TSVEGpfSwitchAppHandler.h>
#include "cfdNavigate.h"

// --- VR Juggler Stuff --- //
#include <vrj/Kernel/Kernel.h>
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
#include <vrj/Util/Debug.h>
#include <vrj/Display/Projection.h>  /* for setNearFar (for setting clipping planes) */
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

using namespace vrj;

// Declare my application class
class simplePfApp : public PfApp
{
public:
     
   //Constructor
   simplePfApp() :
      mModelFileName(""), mLightGroup( NULL ), mSun(NULL), mRootNode( NULL ), mRoomApp( NULL ), mModelRoot( NULL ), modelNodes( NULL ), mApps( NULL )
   {
   }
   
   //Destructor
   virtual ~simplePfApp (void)
   {;}


   virtual void readInBinaries();

   //Sets some necessary variables, and does initial scene graph assembly
   virtual void constructInitSceneGraph();
   
   //Declares the Juggler buttons and sets the flags for the initial state  
   virtual void init( );
   virtual void apiInit( );

   /**
    * Called between pfInit and pfConfig.  This function allows the user
    * application to do any processing that needs to happen before Performer
    * forks its processes off but after pfInit().
    */
   virtual void preForkInit()
   {
      // Initialize model converters
      pfdInitConverter( mModelFileName.c_str() );
   }

   /**
    * Initializes the scene graph.
    * Called after pfInit & pfConfig but before apiInit.
    */
   virtual void initScene();

   /// Return the current scene graph
   virtual pfGroup* getScene()
   {
      vprDEBUG(vprDBG_ALL,1) << "simplePfApp::getScene" << std::endl << vprDEBUG_FLUSH;
      return mRootNode;
   }

   // Function called by the DEFAULT drawChan function 
   // before clearing the channel
   // and drawing the next frame (pfFrame( ))
   virtual void preDrawChan(pfChannel* chan, void* chandata);

   // Function called before pfSync
   virtual void preSync( );

   // Function called after pfSync and before pfDraw
   virtual void preFrame( );

   // Function called after pfDraw
   virtual void intraFrame( );

   // Performer calls before exiting
   //virtual void exit( void );


   /**
    * Sets the name of the model to load.  This must be called before the
    * application is given to the kernel.
    */
   void setModel(std::string modelFile)
   {
      mModelFileName = modelFile;
   }

protected:  // --- HELPERS --- //
   //Takes in user data and sets the flags accordingly
   void updateInteraction();

   //Resets the scene graph and fades the selected app in and the room out
   void transAppIn();

   //Resets the scene graph and fades the selected app out and the room in
   void transAppOut();

   //Controls the functions needed to move to the next app in the scene graph
   void nextApp();

   //Does all the work for the initial flythrough
   void flyThrough();

   //Handles resetting an app's location if navigation was done
   void reCenterApp();



public:
   // CONFIG PARAMS
   std::string    mModelFileName;

   //Transition Flags
   int SWITCH_IN;           // We are Switching into an app
   int SWITCH_OUT;          // We are Switching out of an app
   int APP_ACTIVE;          // We are currently in one app exclusively
   int NEXT_APP;            // Moving to the next app  
   int RUN_FLY_THROUGH;     // Time to start the fly through
   int FLY_THROUGH_ON;      // The fly through is currently running
   int TURN_OFF_BUTTONS;    // A function is in process, so don't register button activity
   int START_INCREMENT;     // Starts the frame counter to handle scaling and translating functions 
   int ORIG_POSITION;       // Tells us if an active app is still at the origin

   // SCENE GRAPH NODES
   pfGroup*       mLightGroup;
   pfLightSource* mSun;                 /**< Sun to light the environment */
   pfGroup*       mRootNode;            /**< The root of the scene graph */
   pfDCS*         mRoomApp;             
   pfNode*        mModelRoot;           /**< Root of the model */
   pfNode*        mModelRoot2;
   std::vector<pfNode*>  modelNodes;   //Read in models, store in the vector
   std::vector<pfAppHandle*>   mApps;  //pfAppHandle class does all the work with the models

   
   std::vector<pfBinaryFiles* > pfBinaries;
   //Flythrough Helpers
   pfQuat *myquat;
   pfQuat q1, q2, q3, q4, q5;
   pfMatrix m, transm, rotm;
   float t;
   int state;

   // VR Juggler's wand digital interface.
   gadget::DigitalInterface buttons[6];
   int buttonData[ 6 ];                //Gets data from the buttons
   int numApps;                        //Gives the number of apps in the switcher(does not include the room)
   int room_active;                    //Flag to tell us if the room is active
   int mActiveApp;                     //Which app is currently active
   float percentage;                   //Frame rate counter to handle scaling and translating functions 

   //Navigation Tools
   cfdNavigate*   nav;   
   // x, y, and z translation of objects in world coordinates.
   float worldTrans[3];
   float worldRot[3];
   float *currentWandDirection;
};

#endif
