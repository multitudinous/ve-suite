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
 * File:          $RCSfile: TSVEGpfswitcher.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <TSVEGpfswitcher.h>


using namespace vrj;

// ------- SCENE GRAPH ----
// a standard organized interface for derived applications:
//
//          /-- mLightGroup -- mSun
// mRootNode --  mModelRoot(room)
//          \-- pfAppHandle vector -- individual apps



inline void simplePfApp::init()
{
   vprDEBUG(vprDBG_ALL,1) << "simplePfApp::init" << std::endl << vprDEBUG_FLUSH;
   buttons[0].init("VJButton0");   // trigger (and top right button) -- menu selection
   buttons[1].init("VJButton1");   // top left button -- toggle cursor mode: laser, streamlines, box, & arrow
   buttons[2].init("VJButton2");   // 12 o'clock -- forward navigation
   buttons[3].init("VJButton3");   // 3 o'clock -- not used at present
   //buttons[4].init("VJButton4");   // 6 o'clock -- reset
   //buttons[5].init("VJButton5");   // 9 o'clock -- exit streamer while loop



   SWITCH_IN = 0;           // We are Switching into an app
   SWITCH_OUT = 0 ;         // We are Switching out of an app
   APP_ACTIVE = 0 ;         // We are currently in one app exclusively
   NEXT_APP = 0;            // Moving to the next app
   RUN_FLY_THROUGH = 0;     // Time to start the fly through
   FLY_THROUGH_ON = 1;      // The fly through is currently running
   TURN_OFF_BUTTONS = 0;    // A function is in process, so don't register button activity
   START_INCREMENT = 0;     // Starts the frame counter to handle scaling and translating functions 
   ORIG_POSITION = 1;       // Tells us if an active app is still at the origin
}


inline void simplePfApp::apiInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "simplePfApp::apiInit" << std::endl << vprDEBUG_FLUSH;
}


inline void simplePfApp::preDrawChan(pfChannel* chan, void* chandata)
{
  vprDEBUG(vprDBG_ALL,3) << "cfdApp::preDrawChan" 
                         << std::endl << vprDEBUG_FLUSH;
}


inline void simplePfApp::preSync( )
{
  vprDEBUG(vprDBG_ALL,1) << "cfdApp::preSync" << std::endl << vprDEBUG_FLUSH;
}


void simplePfApp::readInBinaries()
{
   char textLine [ 256 ];
   

   //std::ifstream inBinariesFile( "Load_Switcher.param", std::ios::in);
   std::ifstream inBinariesFile( mModelFileName.c_str(), std::ios::in);
   
   if(!inBinariesFile)
   {
      std::cout << "Load_Switcher.param could not be loaded" << std::endl;
      exit();
   }


   //int fileapps;
   inBinariesFile >> numApps;
   inBinariesFile.getline( textLine, 256 );

   for (int i=0; i<numApps; i++)
   {
      pfBinaries.push_back( new pfBinaryFiles() );
      inBinariesFile >> pfBinaries[i]->binFiles;
      inBinariesFile.getline( textLine, 256 );
   }
      
   std::cout << "# apps " << numApps << std::endl;

   for (int i=0; i<numApps; i++) 
   {
      inBinariesFile >> pfBinaries[i]->pfBinScale;
      inBinariesFile.getline( textLine, 256 );
   }  

   for (int i=0; i<numApps; i++)
   {
      mApps.push_back( new pfAppHandle( mRootNode ) );
      modelNodes.push_back( pfdLoadFile(pfBinaries[i]->binFiles));
      modelNodes[i]->flatten( 0 );
      mApps[i]->setIconScale(pfBinaries[i]->pfBinScale);
      mApps[i]->addToGraph(modelNodes[i]);
      std::cout << "binaries "<< pfBinaries[i]->binFiles << std::endl;
      std::cout << "scale "<< pfBinaries[i]->pfBinScale << std::endl;
   }
}


void simplePfApp::constructInitSceneGraph()
{
   //numApps = 3;
   mActiveApp = 0;
   room_active = 1;
   state = 1;
   percentage = 0.0f;

   //Root of our scene graph
   mRootNode      = new pfGroup;

   //---------TIME TO BRING IN THE MODELS---------//

   readInBinaries();
   //Room's pfDCS
   mRoomApp = new pfDCS;   

   //Load the room model
   //mModelRoot = pfdLoadFile(mModelFileName.c_str());
   mModelRoot = pfdLoadFile("room_tall_withsky.flt");
   mModelRoot2 = pfdLoadFile("room_tall2.flt");
   mRoomApp->setScale(1.0f);
   mRoomApp->setTrans(0.0f, 70.0f, 0.0f);



   //---------GETTING THE MODELS INTO THE SCENE GRAPH---------//
   
   mRoomApp->addChild(mModelRoot);
   mRootNode->addChild(mLightGroup);
   mRootNode->addChild(mRoomApp);
   

     
}


void simplePfApp::initScene()
{
   // Load the scene
   vprDEBUG(vprDBG_ALL, vprDBG_CRITICAL_LVL) << "simplePfApp::initScene\n" << vprDEBUG_FLUSH;
   

   // Create the SUN light source
   mLightGroup = new pfGroup;
   mSun = new pfLightSource;
   mLightGroup->addChild( mSun );
   mSun->setPos( 0.3f, 0.0f, 0.3f, 0.0f );
   mSun->setColor( PFLT_DIFFUSE,1.0f,1.0f,1.0f );
   mSun->setColor( PFLT_AMBIENT,0.3f,0.3f,0.3f );
   mSun->setColor( PFLT_SPECULAR, 1.0f, 1.0f, 1.0f );
   mSun->on();
   //mRootNode->addChild(mLightGroup);

   // --- LOAD THE MODEL --- //
   constructInitSceneGraph();

   nav = new cfdNavigate();
   nav->Initialize( 0.05 );   
}


//Call updateInteraction which gets user input, then calls functions based on the flags
inline void simplePfApp::preFrame( void )
{

   if (START_INCREMENT == 1)
      percentage += .05;
   else percentage = 0.0;

   updateInteraction();
   
   if (SWITCH_IN == 1)
   {
      transAppIn();    
   }


   else if (SWITCH_OUT == 1)
   {
      transAppOut();
   }


   else if (NEXT_APP == 1)
   {
      nextApp();         
   }


   else if (FLY_THROUGH_ON == 1)
   {
      if (RUN_FLY_THROUGH == 1)
      {
         if (t<1.0)
         {
            t += .01;
            flyThrough();
         }
         else
         {
            t = 0.0;
            state += 1;
         }
      }
   }
}


inline void simplePfApp::intraFrame( )
{
}



// Perform any interaction updating needed
void simplePfApp::updateInteraction()
{
   bool out_btn(false);
   bool nextapp_btn(false);
   bool select_btn(false);
   bool start_flythrough(false);
   
   if (TURN_OFF_BUTTONS == 0)
   {
      if (FLY_THROUGH_ON == 0)
      {
         if ( this->buttons[0]->getData() == gadget::Digital::TOGGLE_ON ||
               this->buttons[0]->getData() == gadget::Digital::ON )
         {
            select_btn = true;
         }

         if (this->buttons[1]->getData() == gadget::Digital::TOGGLE_ON )
         {
            out_btn = true;
         }  

         if (this->buttons[2]->getData() == gadget::Digital::TOGGLE_ON )
         {
            nextapp_btn = true;
         }
      }

      if (this->buttons[3]->getData() == gadget::Digital::TOGGLE_ON )
      {
         start_flythrough = true;
      }
   }


   if (select_btn && APP_ACTIVE == 0)
   {
      TURN_OFF_BUTTONS = 1;
      SWITCH_IN = 1;            
   }

   else if (out_btn && APP_ACTIVE == 1)
   {
      APP_ACTIVE = 0;
      TURN_OFF_BUTTONS = 1;
      SWITCH_OUT = 1;
   }


   else if (nextapp_btn && APP_ACTIVE == 0)
   {
      TURN_OFF_BUTTONS = 1;
      if (mActiveApp < numApps-1)
           mActiveApp += 1;
      else
           mActiveApp = 0;
      NEXT_APP = 1;
   }

   else if (start_flythrough && APP_ACTIVE == 0)
   {
      RUN_FLY_THROUGH = 1;
   }

   else if (select_btn && APP_ACTIVE == 1)
   {
      vprDEBUG(vprDBG_ALL,1) << " Navigate" << std::endl << vprDEBUG_FLUSH;
      nav->FwdTranslate();
      nav->GetWorldLocation( worldTrans );   
      mApps[mActiveApp]->setPosAllAxis( -(worldTrans[0]), 
                                -(worldTrans[1]), 
                                -(worldTrans[2]) );
      if (worldTrans[0] != 0.0f || worldTrans[1] != 0.0f || worldTrans[2] != 0.0f)
         ORIG_POSITION = 0;
   }
}



void simplePfApp::transAppIn()
{   
   if (percentage < 1.0) 
   {
      START_INCREMENT = 1;
      mRoomApp->setScale(1-percentage);
      mApps[mActiveApp]->selectApp(percentage);
   }
   
   else
   {
      mRootNode->removeChild(mRoomApp);
      mApps[mActiveApp]->selectApp(1.0f);
      TURN_OFF_BUTTONS = 0;
      room_active = 0;
      SWITCH_IN = 0;
      START_INCREMENT = 0;
      percentage = 0.0f;
      APP_ACTIVE = 1;
   }
}


void simplePfApp::transAppOut()
{   
   if (ORIG_POSITION == 0)
      reCenterApp();

   else if (room_active == 0)
   {     
      START_INCREMENT = 1;
      mRoomApp->setScale(0.01f);   
      mRootNode->addChild(mRoomApp);
      room_active = 1;
   }

   else if (percentage < 1.0) 
   {  
      
      mRoomApp->setScale(percentage+.01);   
      mApps[mActiveApp]->deSelectApp(percentage);
   }
   
   else
   {
      mRoomApp->setScale(1.0f);
      mApps[mActiveApp]->deSelectApp(1.0f);
      TURN_OFF_BUTTONS = 0;
      START_INCREMENT = 0;
      SWITCH_OUT = 0;
      percentage = 0.0f;
   }
}


void simplePfApp::nextApp()
{  
   if (START_INCREMENT == 0)
   {
      mApps[mActiveApp]->addDCSToRoot();
      mApps[mActiveApp]->setPos(9.0f);
      START_INCREMENT = 1;
   }
           
   else if (percentage < 1.0)
   {
      mApps[mActiveApp]->bringAppIn(percentage);
      if (mActiveApp == 0)
         mApps[numApps - 1]->pushAppOut(percentage);
      else
         mApps[mActiveApp - 1]->pushAppOut(percentage);     
   }

   else 
   {
      mApps[mActiveApp]->setPos(4.0f);
      if (mActiveApp == 0)
         mApps[numApps - 1]->removeDCSFromRoot();
      else
         mApps[mActiveApp - 1]->removeDCSFromRoot(); 
      TURN_OFF_BUTTONS = 0;
      NEXT_APP = 0;
      START_INCREMENT = 0;
   }   
 
}


void simplePfApp::flyThrough()
{
   myquat = new pfQuat();

   q1.makeRot(0, 0, 0, 1);
   q2.makeRot(90, 1, 0, 0);
   q3.makeRot(0, 0, 0, 1);
   q4.makeRot(180, 0, 0, 1);
   q5.makeRot(360, 0, 0, 1);
   
   if (state == 1)
   {
      myquat->slerp( t, q1, q2);
      transm.makeTrans(0.0f, 70-(t)*40, 0.0f);
      myquat->getRot(m);
      m = m * transm;
      mRoomApp->setMat(m);
      
   }
   //state 2 is a pause to read the text
   else if (state == 3)
   {
      myquat->slerp( t, q2, q3);
      transm.makeTrans(0.0f, 30+t*20.0f, t*-30.0f);
      myquat->getRot(m);
      m = m * transm;
      mRoomApp->setMat(m);
   }

   else if (state == 4)
   {
      myquat->slerp( t, q3, q4);
      transm.makeTrans(0.0f, 50.0f, -30.0f+t*15.0f);
      myquat->getRot(m);
      m = m * transm;
      mRoomApp->setMat(m);
   }

   else if (state == 5)
   {
      myquat->slerp( t, q4, q5);
      transm.makeTrans(0.0f, 50.0f, -15+t*15.0f);
      myquat->getRot(m);
      m = m * transm;
      mRoomApp->setMat(m);
   }

   //Small Rotation and Translation adjustment was made to get the room in line with c6 walls
   else if (state == 6)
   {
      rotm.makeRot(0.4f, 0.0f, 0.0f, 1.0f);
      transm.makeTrans(0.0f, (50-50*t)+0.5, 0.0f);
      m = rotm * transm;
      mRoomApp->setMat(m);
   }

   else if (state > 6)
   {
      mRoomApp->removeChild(mModelRoot);
      mRoomApp->addChild(mModelRoot2);
      mApps[0]->addDCSToRoot();
      state = 0;
      RUN_FLY_THROUGH = 0;
      FLY_THROUGH_ON = 0;
   }
}


void simplePfApp::reCenterApp()
{
   if (percentage * 2 < 1.0)
   {
      
      START_INCREMENT = 1;
      for (int i=0; i < 3; i++)      
         worldTrans[i] -= (worldTrans[i]*(percentage*2));
      mApps[mActiveApp]->setPosAllAxis(-worldTrans[0],-worldTrans[1],-worldTrans[2]);
   }
   
   else
   {
      START_INCREMENT = 0;
      for (int i=0; i < 3; i++)      
         worldTrans[i] = 0.0f;
      mApps[mActiveApp]->setPosAllAxis(worldTrans[0],worldTrans[1],worldTrans[2]);
      nav->ResetCoordsToZero();
      ORIG_POSITION = 1;
   }
}
   
