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

// Scene graph dependant headers
#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pfdb/pfpfb.h>
#elif _OSG
#include <osg/Group>
#include <osgDB/WriteFile>
#include <osg/FrameStamp>
 

#include <osgUtil/SceneView>
#endif

#include <vrj/Kernel/Kernel.h>

#include "cfdEnum.h"
#include "fileIO.h"
#include "cfdPfSceneManagement.h"
#include "cfdEnvironmentHandler.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdTransientVizHandler.h"
#include "cfdModelHandler.h"

#include "cfdCommandArray.h"
#include "cfdGroup.h"
#include "cfdDCS.h"
#include "cfdObjects.h"
#include "cfdTempAnimation.h"
#include "cfdIHCCModel.h"
#include "cfdVjObsWrapper.h"

#ifdef _OSG
#include "cfdTextureBasedVizHandler.h"
#endif

#ifdef _TAO
#include "cfdExecutive.h"
#endif //_TAO

/// C/C++ libraries
#include <iostream>
//#include <omp.h>

cfdApp::cfdApp( void )
{
   this->_sceneManager =         NULL;
   this->_environmentHandler =   NULL;
   this->_steadystateHandler =   NULL;
   //this->_transientHandler =     NULL;
   this->_modelHandler =         NULL;
   this->ihccModel =             NULL;
#ifdef _OSG
   _tbvHandler = 0;
   _frameNumber = 0;
#endif
}
#ifdef _PERFORMER
void cfdApp::exit()
{
   delete filein_name;
#ifdef _OSG
   if(_tbvHandler){
      delete _tbvHandler;
   }
#endif
   // we don't have a destructor, so delete items here...
   if ( this->_sceneManager )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_sceneManager" << std::endl << vprDEBUG_FLUSH;
      delete this->_sceneManager;
   }

   if ( this->_environmentHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_environmentHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_environmentHandler;
   }

   if ( this->_steadystateHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_steadystateHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_steadystateHandler;
   }

   /*if ( this->_transientHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_transientHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_transientHandler;
   }*/

   if ( this->_modelHandler )
   {  
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->_modelHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_modelHandler;
   }

   if ( this->ihccModel )
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->ihccModel" << std::endl << vprDEBUG_FLUSH;
      delete this->ihccModel;
   }

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
      delete this->_vjobsWrapper;
   }
}



inline void cfdApp::apiInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::apiInit" << std::endl << vprDEBUG_FLUSH;
}

inline void cfdApp::preForkInit( )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::preForkInit"
                          << std::endl << vprDEBUG_FLUSH;
   //pfdInitConverter( "air_system.flt" );
}

inline pfGroup* cfdApp::getScene( )
{
  //vprDEBUG(vprDBG_ALL,1) << "cfdApp::getScene" << std::endl << vprDEBUG_FLUSH;
  return (pfGroup*)(this->_sceneManager->GetRootNode()->GetRawNode());//for test
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
cfdApp::~cfdApp()
{
}
#ifdef _OSG
void cfdApp::configSceneView(osgUtil::SceneView* newSceneViewer)
{
   //testing--move to cfdApp.cxx
   newSceneViewer->setDefaults();
   //newSceneViewer->setBackgroundColor( osg::Vec4(0.0f, 0.0f, 0.0f, 0.0f) );
   newSceneViewer->getLight()->setAmbient(osg::Vec4(0.3f,0.3f,0.3f,1.0f));
   newSceneViewer->getLight()->setDiffuse(osg::Vec4(0.9f,0.9f,0.9f,1.0f));
   newSceneViewer->getLight()->setSpecular(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
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
inline osg::Group* cfdApp::getScene()
{
   //osgDB::writeNodeFile(*this->_sceneManager->GetRootNode()->GetRawNode(),
   //   "C:/test.osg");
   return (osg::Group*)this->_sceneManager->GetRootNode()->GetRawNode();
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
}



void cfdApp::SetWrapper( cfdVjObsWrapper* input )
{
   _vjobsWrapper = input;
}

inline void cfdApp::initScene( )
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
   this->_sceneManager = new cfdPfSceneManagement( this->filein_name );
   this->_sceneManager->InitScene();

   // modelHandler stores the arrow and holds all data and geometry
   this->_modelHandler = new cfdModelHandler( this->filein_name, 
                                              this->_sceneManager->GetWorldDCS() );
   this->_modelHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   this->_modelHandler->InitScene();

   // navigation and cursor 
   this->_environmentHandler = new cfdEnvironmentHandler( this->filein_name );
   this->_environmentHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_environmentHandler->SetRootNode( this->_sceneManager->GetRootNode() );
   this->_environmentHandler->SetArrow( this->_modelHandler->GetArrow() );
   this->_environmentHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   this->_environmentHandler->InitScene();

   // create steady state visualization objects
   this->_steadystateHandler = new cfdSteadyStateVizHandler( this->filein_name );
   this->_steadystateHandler->SetWorldDCS( this->_sceneManager->GetWorldDCS() );
   this->_steadystateHandler->SetNavigate( this->_environmentHandler->GetNavigate() );
   this->_steadystateHandler->SetCursor( this->_environmentHandler->GetCursor() );
   this->_steadystateHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   this->_steadystateHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_steadystateHandler->InitScene();

   //create the volume viz handler
#ifdef _OSG
   if(!_tbvHandler){
      _tbvHandler = new cfdTextureBasedVizHandler();
   }
   _tbvHandler->SetParameterFile(filein_name);
   //_tbvHandler->SetWorldDCS( _sceneManager->GetWorldDCS() );
   _tbvHandler->SetParentNode(_sceneManager->GetWorldDCS() );
   _tbvHandler->SetNavigate( _environmentHandler->GetNavigate() );
   _tbvHandler->SetCursor( _environmentHandler->GetCursor() );
   _tbvHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   _tbvHandler->InitVolumeVizNodes();
#endif
/*
   // TODO fix transient
   this->_transientHandler = new cfdTransientVizHandler( this->filein_name );
   this->_transientHandler->InitScene();
std::cout << "|  3d" << std::endl;
*/
#ifdef _OSG
   this->_vjobsWrapper->SetHandlers( _steadystateHandler, _environmentHandler, 
                                 _modelHandler, _tbvHandler );
#else
    this->_vjobsWrapper->SetHandlers( _steadystateHandler, _environmentHandler, 
                                 _modelHandler);
#endif

#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   this->executive = new cfdExecutive( _vjobsWrapper->naming_context, _vjobsWrapper->child_poa, this->_sceneManager->GetWorldDCS() );
   this->executive->SetModelHandler( this->_modelHandler, this->_environmentHandler );

#endif // _TAO

    this->ihccModel = NULL;
/*
   //
   // Make IHCC Model - should be deleted at a later date
   //
   // Fix this with new read param method
   //if ( this->paramReader->ihccModel )
   {
      std::cout << "| 54. Initializing...................................... IHCC Model |" << std::endl;
      this->ihccModel = new cfdIHCCModel( NULL, this->_sceneManager->GetWorldDCS() );
   }
*/
   // This may need to be fixed
   this->_vjobsWrapper->GetCfdStateVariables();
}

void cfdApp::preFrame( void )
{
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::preFrame" << std::endl << vprDEBUG_FLUSH;
#ifdef _OSG
   if(!_modelHandler)initScene();
    double time_since_start = _timer.delta_s(_start_tick,_timer.tick());
    if(_frameStamp.valid()){
       _frameStamp->setFrameNumber(_frameNumber++);
       _frameStamp->setReferenceTime(time_since_start);
    }
   
#endif
#ifdef _CLUSTER
   //call the parent method
   _vjobsWrapper->GetUpdateClusterStateVariables();
#endif // _CLUSTER

   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_modelHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_modelHandler->PreFrameUpdate();
   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_environmentHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_environmentHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_environmentHandler->PreFrameUpdate();
   ///////////////////////
   vprDEBUG(vprDBG_ALL,3) << "cfdApp::this->_steadystateHandler->PreFrameUpdate()" << std::endl << vprDEBUG_FLUSH;
   this->_steadystateHandler->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   this->_steadystateHandler->SetActiveModel( this->_modelHandler->GetActiveModel() );
   this->_steadystateHandler->PreFrameUpdate();
   ///////////////////////
#ifdef _OSG
   _tbvHandler->SetActiveTextureManager(_modelHandler->GetActiveTextureManager());
   _tbvHandler->PreFrameUpdate();
#endif
   ///////////////////////

   if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SEND_PARAM )
   {
      // This IHCC hack needs to go very soon
/*   
      double data[ 6 ];// = { 0 };
      data[ 0 ] = _vjobsWrapper->GetShortArray( 1 ); //200;  //Agitation (rpm)  initial value 200
      data[ 1 ] = _vjobsWrapper->GetShortArray( 2 ); //1.25; //Air Concentration initial value 1.25;
      data[ 2 ] = _vjobsWrapper->GetShortArray( 3 ); //6;    //Initial pH value    initial value 6
      data[ 3 ] = _vjobsWrapper->GetShortArray( 4 ); //0.1;  //Nitrate Concentration     initial value 0.1
      data[ 4 ] = _vjobsWrapper->GetShortArray( 5 ); //37;   //Temperate (Celsius)        initial value 37
      data[ 5 ] = _vjobsWrapper->GetShortArray( 6 ); //240;  //Simulate [a text box] Hours in 10 seconds, initial value 240

      this->ihccModel->UpdateModelVariables( data );
      this->ihccModel->Update();
*/
   }
   else if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == EXIT )
   {
      // exit cfdApp was selected
#ifdef _TAO
      this->executive->UnbindORB();
#endif // _TAO
      vrj::Kernel::instance()->stop(); // Stopping kernel using the inherited member variable
   }

#ifdef _TAO
   if ( this->_modelHandler->GetActiveDataSet() != NULL )
   {
      this->executive->SetActiveDataSet( this->_modelHandler->GetActiveDataSet() );
   }
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

   this->_vjobsWrapper->GetCfdStateVariables();
   vprDEBUG(vprDBG_ALL,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}
