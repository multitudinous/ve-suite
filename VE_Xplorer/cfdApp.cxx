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
#include "cfdTextureBasedVizHandler.h"
#endif

#include "cfdEnum.h"
#include "fileIO.h"
#include "cfdPfSceneManagement.h"
#include "cfdEnvironmentHandler.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdModelHandler.h"
#include "cfdModel.h"
#include "cfdSwitch.h"

#include "cfdCommandArray.h"
#include "cfdNode.h"
#include "cfdGroup.h"
#include "cfdDCS.h"
#include "cfdObjects.h"
#include "cfdTempAnimation.h"
#include "cfdVjObsWrapper.h"

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
#include "cfdPBufferManager.h"
/// C/C++ libraries
#include <iostream>
//#include <omp.h>
#include <vrj/Kernel/Kernel.h>

cfdApp::cfdApp( void )
{
#ifdef _OSG
   _tbvHandler = 0;
   _frameNumber = 0;
   _pbuffer = 0;
#endif
}

#ifdef _PERFORMER
void cfdApp::exit()
{
   delete filein_name;
#ifdef _OSG
   if ( _tbvHandler )
   {
      delete _tbvHandler;
   }
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
}

inline void cfdApp::apiInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::apiInit" << std::endl << vprDEBUG_FLUSH;
   pfNotifyHandler( notifyHandler );
}

inline void cfdApp::preForkInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::preForkInit"
                          << std::endl << vprDEBUG_FLUSH;
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
//////////////////////////
void cfdApp::contextInit()
{
   vrj::OsgApp::contextInit();
#ifdef CFD_USE_SHADERS
   if (!_pbuffer){
      _pbuffer = new cfdPBufferManager();
      _pbuffer->isSupported();
   } 
   if(_tbvHandler){
      _tbvHandler->SetPBuffer(_pbuffer);
   }
#endif
}
///////////////////////////
void cfdApp::contextClose()
{
#ifdef CFD_USE_SHADERS
   if(!_pbuffer){
      delete _pbuffer;
      _pbuffer = 0;
   }
#endif
}
////////////////////////////////////////
cfdPBufferManager* cfdApp::GetPBuffer()
{
#ifdef CFD_USE_SHADERS
   if(_pbuffer){
      return _pbuffer;
   }
#endif
   return 0;
}
void cfdApp::configSceneView(osgUtil::SceneView* newSceneViewer)
{
   //testing--move to cfdApp.cxx
   newSceneViewer->setDefaults();
   //newSceneViewer->setBackgroundColor( osg::Vec4(0.0f, 0.0f, 0.0f, 0.0f) );
   newSceneViewer->getLight()->setAmbient(osg::Vec4(0.3f,0.3f,0.3f,1.0f));
   newSceneViewer->getLight()->setDiffuse(osg::Vec4(0.9f,0.9f,0.9f,1.0f));
   newSceneViewer->getLight()->setSpecular(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
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
#endif

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
 
   _tbvHandler = cfdTextureBasedVizHandler::instance();
   _tbvHandler->SetParameterFile(filein_name);
   _tbvHandler->SetParentNode((cfdGroup*)cfdModelHandler::instance()->GetActiveModel()->GetSwitchNode()->GetChild(1) );
   _tbvHandler->SetNavigate( cfdEnvironmentHandler::instance()->GetNavigate() );
   _tbvHandler->SetCursor( cfdEnvironmentHandler::instance()->GetCursor() );
   _tbvHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   //_tbvHandler->SetSceneView(_sceneViewer.get());
   _tbvHandler->InitVolumeVizNodes();
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
      _frameStamp->setFrameNumber( this->_vjobsWrapper->GetSetFrameNumber(-1) );
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
   _tbvHandler->SetActiveTextureManager(cfdModelHandler::instance()->GetActiveTextureManager());
   _tbvHandler->ViewTextureBasedVis(cfdModelHandler::instance()->GetVisOption());
   _tbvHandler->PreFrameUpdate();
#endif
   ///////////////////////

   if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == EXIT )
   {
      // exit cfdApp was selected
#ifdef _TAO
      this->executive->UnbindORB();
#endif // _TAO
      vrj::Kernel::instance()->stop(); // Stopping kernel 
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
   double time_since_start = _timer.delta_s(_start_tick,_timer.tick());
   this->_vjobsWrapper->GetSetAppTime( time_since_start );
   this->_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );
#endif
   this->_vjobsWrapper->GetCfdStateVariables();
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}
