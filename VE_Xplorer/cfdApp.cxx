/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdApp.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// .NAME cfdApp - An virtual reality (VRJuggler) application class.
// .SECTION Description
// A class to execute an CFD application in the virtual environment.
// It is derived by using the VTK and VRJuggler classes.

#include "cfdApp.h"
#ifdef _OSG
#ifdef VE_PATENTED
#include "cfdTextureBasedVizHandler.h"
#endif
#endif

#include "cfdEnum.h"
#include "fileIO.h"
#include "cfdPfSceneManagement.h"
#include "cfdEnvironmentHandler.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdModelHandler.h"
#include "cfdModel.h"
#include "cfdSwitch.h"
#include "cfdDataSet.h"

#include "cfdCommandArray.h"
#include "cfdNode.h"
#include "cfdGroup.h"
#include "cfdDCS.h"
#include "cfdObjects.h"
#include "cfdTempAnimation.h"
#include "cfdVjObsWrapper.h"
#include "cfdDataSet.h"

#ifdef _TAO
#include "cfdExecutive.h"
#endif //_TAO

// Scene graph dependant headers
#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pfdb/pfpfb.h>
#include "cfdNotify.h"
#elif _OSG
#include <osg/Group>
#include <osgDB/WriteFile>
#include <osg/FrameStamp>
#include <osgUtil/SceneView>
#endif
#ifdef _OSG
#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
#include "cfdPBufferManager.h"
#endif
#endif
#endif
/// C/C++ libraries
#include <iostream>
//#include <omp.h>

#include <vrj/Kernel/Kernel.h>

//web interface stuff
#ifdef _WEB_INTERFACE
#include <corona.h>
#include <vrj/Draw/OGL/GlApp.h>
#endif   //_WEB_INTERFACE

cfdApp::cfdApp( void )
{
#ifdef _OSG
#ifdef VE_PATENTED
   _tbvHandler = 0;
#ifdef CFD_USE_SHADERS
   _pbuffer = 0;
#endif
#endif
   _frameNumber = 0;
#endif
}

void cfdApp::exit()
{
   delete filein_name;
   cfdPfSceneManagement::instance()->CleanUp();
   cfdModelHandler::instance()->CleanUp();
   cfdEnvironmentHandler::instance()->CleanUp();
   cfdSteadyStateVizHandler::instance()->CleanUp();
#ifdef _OSG
#ifdef VE_PATENTED
   cfdTextureBasedVizHandler::instance()->CleanUp();
#endif
#endif

#ifdef _TAO
   if ( this->executive ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->executive" << std::endl << vprDEBUG_FLUSH;
      delete this->executive;
   }
#endif // _TAO

   if ( this->_vjobsWrapper != NULL )
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_vjobsWrapper" << std::endl << vprDEBUG_FLUSH;
      delete this->_vjobsWrapper;
   }
   
#ifdef _WEB_INTERFACE
   runWebImageSaveThread=false;
   vpr::System::msleep( 1000 );  // one-second delay
   delete writeWebImageFileThread;
   if(readyToWriteWebImage)   //if we've captured the pixels, but didn't write them out
      delete[] webImagePixelArray;   //delete the pixel array
#endif  //_WEB_INTERFACE
}

#ifdef _PERFORMER
inline void cfdApp::apiInit( )
{
   vrj::PfApp::apiInit();
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::apiInit" << std::endl << vprDEBUG_FLUSH;
   pfNotifyHandler( notifyHandler );
}

inline void cfdApp::preForkInit( )
{
   vrj::PfApp::preForkInit();
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::preForkInit"
                          << std::endl << vprDEBUG_FLUSH;
   pfdInitConverter("./NewComponents/Terrain_all.obj");
   //pfdInitConverter( "air_system.flt" );
}

void cfdApp::appChanFunc( pfChannel* chan )
{
   // used to adjust lod scaling
   vrj::PfApp::appChanFunc( chan );
   
   if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_LOD_SCALE )
   {
      chan->setLODAttr(PFLOD_SCALE, (float)_vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_SC ));
   }
}

inline void cfdApp::preSync( )
{
  vprDEBUG(vprDBG_ALL,1) << "cfdApp::preSync" << std::endl << vprDEBUG_FLUSH;
}
#endif

#ifdef _PERFORMER
inline pfGroup* cfdApp::getScene()
#elif _OSG
inline osg::Group* cfdApp::getScene()
#endif
{
   //osgDB::writeNodeFile(*this->_sceneManager->GetRootNode()->GetRawNode(),
   //   "C:/test.osg");
#ifdef _PERFORMER
   return (pfGroup*)(cfdPfSceneManagement::instance()->GetRootNode()->GetRawNode());
#elif _OSG
   return (osg::Group*)cfdPfSceneManagement::instance()->GetRootNode()->GetRawNode();
#endif
}

#ifdef _OSG
#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
//////////////////////////
void cfdApp::contextInit()
{
   vrj::OsgApp::contextInit();

   if (!_pbuffer){
      _pbuffer = new cfdPBufferManager();
      _pbuffer->isSupported();
   } 
   if(_tbvHandler){
      _tbvHandler->SetPBuffer(_pbuffer);
   }
}
///////////////////////////
void cfdApp::contextClose()
{
   if(!_pbuffer){
      delete _pbuffer;
      _pbuffer = 0;
   }
}
////////////////////////////////////////
cfdPBufferManager* cfdApp::GetPBuffer()
{
   if(_pbuffer){
      return _pbuffer;
   }
   return 0;
}
#endif
#endif
void cfdApp::configSceneView(osgUtil::SceneView* newSceneViewer)
{
   newSceneViewer->setDefaults();
   newSceneViewer->setClearColor(osg::Vec4(0,0,0,1));
   _sceneViewer = newSceneViewer;
   _frameStamp = new osg::FrameStamp;
   _start_tick = _timer.tick();
   _frameStamp->setReferenceTime(0.0);
   _frameStamp->setFrameNumber(0);
   _sceneViewer->setFrameStamp(_frameStamp.get());
}
void cfdApp::bufferPreDraw()
{
   glClearColor(0.0, 0.0, 0.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
#endif //_OSG

inline void cfdApp::init( )
{
   //_corbaManager = new CorbaManager();
   vprDEBUG(vprDBG_ALL,0) << "cfdApp::init" << std::endl << vprDEBUG_FLUSH;
   filein_name = new char [ 256 ];
   do
   {
      std::cout << "|   Enter VE_Xplorer parameter filename: ";
      std::cin >> filein_name;
      if ( ! fileIO::isFileReadable( this->filein_name ) )
      {
         std::cerr << "\n\"" << this->filein_name << "\" is not readable." << std::endl;
      }
   }
   while ( ! fileIO::isFileReadable( this->filein_name ) );

   //std::cout << "filein_name: " << this->filein_name << std::endl;

   std::cout << std::endl;
   std::cout << "| ***************************************************************** |" << std::endl;
   std::cout << "|  3. Initializing........................... Parameter File Reader |" << std::endl;
   _vjobsWrapper->InitCluster();
#ifdef _OSG
   initScene();
#endif

#ifdef _WEB_INTERFACE
   timeOfLastCapture = 0;
   runWebImageSaveThread = true;
   readyToWriteWebImage = false;
   writingWebImageNow = false;
   captureNextFrameForWeb = false
#endif   //_WEB_INTERFACE
}

void cfdApp::SetWrapper( cfdVjObsWrapper* input )
{
   _vjobsWrapper = input;
}

void cfdApp::initScene( )
{
   vprDEBUG(vprDBG_ALL,0) << "cfdApp::initScene" << std::endl << vprDEBUG_FLUSH;

# ifdef _OPENMP
   std::cout << "\n\n\n";
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|          Compiled by an OpenMP-compliant implementation           |" << std::endl;
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|                                                                   |" << std::endl;
# endif // _OPENMP

   // define the rootNode, worldDCS, and lighting
   cfdPfSceneManagement::instance()->Initialize( this->filein_name );
   cfdPfSceneManagement::instance()->InitScene();

   // modelHandler stores the arrow and holds all data and geometry
   cfdModelHandler::instance()->Initialize( this->filein_name );
   cfdModelHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   cfdModelHandler::instance()->InitScene();

   // navigation and cursor 
   cfdEnvironmentHandler::instance()->Initialize( this->filein_name );
   cfdEnvironmentHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   cfdEnvironmentHandler::instance()->InitScene();

   // create steady state visualization objects
   cfdSteadyStateVizHandler::instance()->Initialize( this->filein_name );
   cfdSteadyStateVizHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   cfdSteadyStateVizHandler::instance()->InitScene();

   //create the volume viz handler
#ifdef _OSG
#ifdef VE_PATENTED
   _tbvHandler = cfdTextureBasedVizHandler::instance();
   _tbvHandler->SetParameterFile(filein_name);
   _tbvHandler->SetNavigate( cfdEnvironmentHandler::instance()->GetNavigate() );
   _tbvHandler->SetCursor( cfdEnvironmentHandler::instance()->GetCursor() );
   _tbvHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   //_tbvHandler->SetSceneView(_sceneViewer.get());
   //_tbvHandler->InitVolumeVizNodes();
#endif
#endif

#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   this->executive = new cfdExecutive( _vjobsWrapper->naming_context, _vjobsWrapper->child_poa );
#endif // _TAO

   // This may need to be fixed
   this->_vjobsWrapper->GetCfdStateVariables();
}
void cfdApp::preFrame( void )
{
}

void cfdApp::latePreFrame( void )
{
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::preFrame" << std::endl << vprDEBUG_FLUSH;
#ifdef _CLUSTER
   //call the parent method
   _vjobsWrapper->GetUpdateClusterStateVariables();
#endif // _CLUSTER
#ifdef _OSG
   // This is order dependent
   // don't move above function call
   if ( _frameStamp.valid() )
   {
      _frameStamp->setFrameNumber( _frameNumber++ );
      _frameStamp->setReferenceTime( this->_vjobsWrapper->GetSetAppTime(-1) );
   }
#endif

   ///////////////////////
   cfdModelHandler::instance()->PreFrameUpdate();
   ///////////////////////
   cfdEnvironmentHandler::instance()->PreFrameUpdate();
   ///////////////////////
   cfdSteadyStateVizHandler::instance()->PreFrameUpdate();
   ///////////////////////
#ifdef _OSG
#ifdef VE_PATENTED
   //_tbvHandler->SetActiveTextureManager(cfdModelHandler::instance()->GetActiveTextureManager());
   _tbvHandler->SetParentNode((cfdGroup*)cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->GetChild(1) );
   _tbvHandler->SetActiveTextureDataSet(cfdModelHandler::instance()->GetActiveTextureDataSet());
   _tbvHandler->ViewTextureBasedVis(cfdModelHandler::instance()->GetVisOption());
   _tbvHandler->PreFrameUpdate();
#endif
#endif

   if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == EXIT )
   {
      // exit cfdApp was selected
#ifdef _TAO
      this->executive->UnbindORB();
#endif // _TAO
      vrj::Kernel::instance()->setApplication( NULL ); // Stopping kernel 
   }

#ifdef _TAO
   this->executive->UpdateModules();
   this->executive->CheckCommandId( _vjobsWrapper->GetCommandArray() );
#endif // _TAO

   this->_vjobsWrapper->PreFrameUpdate();
   vprDEBUG(vprDBG_ALL,3) << " cfdApp::End preFrame" << std::endl << vprDEBUG_FLUSH;
}

void cfdApp::intraFrame()
{
   vprDEBUG(vprDBG_ALL,3) << " intraFrame" << std::endl << vprDEBUG_FLUSH;
   // Do nothing here
   // Usually slows things down
}

void cfdApp::postFrame()
{
   vprDEBUG(vprDBG_ALL,3) << " postFrame" << std::endl << vprDEBUG_FLUSH;

#ifdef _OSG
   time_since_start = _timer.delta_s(_start_tick,_timer.tick());
#ifdef _WEB_INTERfACE
   if(time_since_start - timeOfLastCapture >= 5.0)      //if it's been five seconds since the last image cap
   {
      if(!readyToWriteWebImage)
      {
         captureNextFrameForWeb = true
         timeOfLastCapture = time_since_start;
      }
   }
#endif  //_WEB_INTERFACE
#endif  //_OSG

   // if transient data is being displayed, then update gui progress bar
  /* if (  this->_modelHandler->GetActiveSequence() )
   {
      int currentFrame = this->_modelHandler->GetActiveSequence()->GetSequence()->GetFrameOfSequence();
      //cout << " Current Frame :  " << currentFrame << endl;
      if (this->lastFrame != currentFrame )
      {
         this->setTimesteps( currentFrame );
         this->lastFrame = currentFrame;
      }
   }*/

#ifdef _OSG
   this->_vjobsWrapper->GetSetAppTime( time_since_start );
   //this->_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );
#endif   //_OSG
   this->_vjobsWrapper->GetCfdStateVariables();
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}

//web interface thread for writing the file
#ifdef _WEB_INTERFACE

void cfdApp::captureWebImage()
{
   if(writingWebImageNow || readyToWriteWebImage) return;
   int dummyOx=0;
   int dummyOy=0;

   printf("Reading viewport size...\n");
   //get the viewport height and width
   vrj::GlDrawManager::instance()->currentUserData()->getGlWindow()->
      getOriginSize(dummyOx, dummyOy, webImageWidth, webImageHeight);
   printf("Copying frame buffer %ix%i.......\n", frameWidth, frameHeight);
   captureNextFrameForWeb=false;      //we're not going to capture next time around
   webImagePixelArray=new char[frameHeight*frameWidth*3];      //create an array to store the data
   glReadPixels(0, 0, webImageWidth, webImageHeight, GL_RGB, GL_UNSIGNED_BYTE, webImagePixelArray);   //copy from the framebuffer
   readyToWriteWebImage=true;      
}

void cfdApp::writeImageFileForWeb(void*)
{
   while(runImageSaveThread)
   {
      vpr::System::msleep( 500 );  // half-second delay
      if(readyToWriteWebImage)
      {
         readyToWriteWebImage=false;
         writingWebImageNow = true;
         //let's try saving the image with Corona
         Image* frameCap=CreateImage(frameWidth, frameHeight, PF_R8G8B8, (void*)webImagePixelArray);
         frameCap=FlipImage(frameCap, CA_X);
         if(!SaveImage("../../public_html/PowerPlant/VE/dump.png", FF_PNG, frameCap))
            printf("error saving image\n");
         else printf("Image saved successfully.\n");
         delete frameCap;
         delete [] webImagePixelArray;                              //delete our array
         printf("All done!\n");
         writingWebImageNow = false;
      }
   }
}

#ifdef _OSG
void cfdApp::draw()
   glClear(GL_DEPTH_BUFFER_BIT);

   glPushAttrib(GL_ALL_ATTRIB_BITS);
   glPushAttrib(GL_TRANSFORM_BIT);
   glPushAttrib(GL_VIEWPORT_BIT);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();

   glMatrixMode(GL_TEXTURE);
   glPushMatrix();


   osgUtil::SceneView* sv(NULL);
   sv = (*sceneViewer);    // Get context specific scene viewer
   vprASSERT( sv != NULL);

   GlDrawManager*    gl_manager;    /**< The openGL manager that we are rendering for. */
   gl_manager = GlDrawManager::instance();

   // Set the up the viewport (since OSG clears it out)
   float vp_ox, vp_oy, vp_sx, vp_sy;   // The float vrj sizes of the view ports
   int w_ox, w_oy, w_width, w_height;  // Origin and size of the window
   gl_manager->currentUserData()->getViewport()->getOriginAndSize(vp_ox, vp_oy, vp_sx, vp_sy);
   gl_manager->currentUserData()->getGlWindow()->getOriginSize(w_ox, w_oy, w_width, w_height);

   // compute unsigned versions of the viewport info (for passing to glViewport)
   unsigned ll_x = unsigned(vp_ox*float(w_width));
   unsigned ll_y = unsigned(vp_oy*float(w_height));
   unsigned x_size = unsigned(vp_sx*float(w_width));
   unsigned y_size = unsigned(vp_sy*float(w_height));

   // Add the tree to the scene viewer and set properties
   sv->setSceneData(getScene());
   //sv->setCalcNearFar(false);
   sv->setComputeNearFarMode(osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR);
   sv->setViewport(ll_x, ll_y, x_size, y_size);

   //Get the view matrix and the frustrum form the draw manager
   GlDrawManager* drawMan = dynamic_cast<GlDrawManager*>(this->getDrawManager());
   vprASSERT(drawMan != NULL);
   GlUserData* userData = drawMan->currentUserData();

   // Copy the matrix
   Projection* project = userData->getProjection();
   const float* vj_proj_view_mat = project->getViewMatrix().mData;
   osg::RefMatrix* osg_proj_xform_mat = new osg::RefMatrix;
   osg_proj_xform_mat->set( vj_proj_view_mat );

   //Get the frustrum
   Frustum frustum = project->getFrustum();
   sv->setProjectionMatrixAsFrustum(frustum[Frustum::VJ_LEFT],
                                    frustum[Frustum::VJ_RIGHT],
                                    frustum[Frustum::VJ_BOTTOM],
                                    frustum[Frustum::VJ_TOP],
                                    frustum[Frustum::VJ_NEAR],
                                    frustum[Frustum::VJ_FAR]);

   sv->setViewMatrix(*osg_proj_xform_mat);
   bool goCapture = false;         //gocapture becomes true if we're going to capture this frame
   if(userData->getViewport()->isSimulator())   //if this is a sim window context....
   {
      //Matrix44f headMat=mHead->getData();      //grab the head matrix
//      Matrix44f h=headMat;
//      glMultMatrixf(headMat.mData);         //and multiply to cancel it out of the modelview
//      gluLookAt(0, 100, 0, 0, 0, 0, 0, 0, -1);   //an overhead view
      if(captureNextFrameForWeb) goCapture=true;   //now if we're go for capture, we'll know for sure
   }

   //Draw the scene
   sv->update();
   sv->cull();
   sv->draw();
   if(goCapture)
      captureWebImage();
   glMatrixMode(GL_TEXTURE);
   glPopMatrix();

   glMatrixMode(GL_PROJECTION);
   glPopMatrix();

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

   glPopAttrib();
   glPopAttrib();
   glPopAttrib();
}
#endif //_OSG
#endif   //_WEB_INTERFACE
