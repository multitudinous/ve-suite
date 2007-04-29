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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/GE/cfdApp.h"

#ifdef _OSG
#ifdef VE_PATENTED
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"
#endif
#endif

#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/Utilities/fileIO.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"

#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"
#include "VE_Xplorer/GE/cfdVjObsWrapper.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/Model/ModelCreator.h"

#ifdef _TAO
#include "VE_Xplorer/XplorerNetwork/cfdExecutive.h"
#endif //_TAO

// Scene graph dependant headers
#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pfdb/pfpfb.h>
#include "VE_Xplorer/cfdNotify.h"
#elif _OSG
#include <osg/Group>
#include <osg/FrameStamp>
#include <osg/MatrixTransform>
#include <osg/Matrix>
#include <osg/Referenced>
#include <osg/Light>
#include <osg/LightModel>
#include <osg/LightSource>

#include <osgDB/WriteFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>

#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#endif

#ifdef _OSG
#ifdef VE_PATENTED
//#ifdef CFD_USE_SHADERS
#include "VE_Xplorer/TextureBased/cfdPBufferManager.h"
using namespace VE_TextureBased;
//#endif
#endif
#endif

/// C/C++ libraries
#include <iostream>
//#include <omp.h>

#include <vrj/Kernel/Kernel.h>

//web interface stuff
#ifdef _WEB_INTERFACE
   #include <corona.h>
   #include <vpr/System.h>
#endif //_WEB_INTERFACE

using namespace VE_Xplorer;
using namespace VE_Util;

////////////////////////////////////////////////////////////////////////////////
cfdApp::cfdApp( int argc, char* argv[] ) 
#ifdef _OSG
: vrj::OsgApp( vrj::Kernel::instance() )
#endif
{
   filein_name.erase();// = 0;
   isCluster = false;
#ifdef _OSG
   //osg::Referenced::instance()->setThreadSafeRefUnref(true);
   osg::Referenced::setThreadSafeReferenceCounting(true);
   osg::DisplaySettings::instance()->setMaxNumberOfGraphicsContexts( 20 );
   _frameStamp = new osg::FrameStamp;
   mUpdateVisitor = new osgUtil::UpdateVisitor();
   _frameStamp->setReferenceTime(0.0);
   _frameStamp->setFrameNumber(0);
   svUpdate = false;

   light_0 = new osg::Light;
   light_source_0 = new osg::LightSource;

   light_0->setLightNum( 0 );
   light_0->setAmbient( osg::Vec4f( 0.36862f, 0.36842f, 0.36842f, 1.0f ) );
   //light_0->setDiffuse( osg::Vec4f( 0.0f, 0.88500f, 0.0f, 1.0f ) );
   light_0->setDiffuse( osg::Vec4f( 0.88627f, 0.88500f, 0.88500f, 1.0f ) );
   light_0->setSpecular( osg::Vec4f( 0.49019f, 0.48872f, 0.48872f, 1.0f ) );
   //light_0->setSpecular( osg::Vec4f( 0.0f, 1.0f, 0.0f, 1.0f ) );
   light_0->setPosition( osg::Vec4f( 10000.0f, -10000.0f, 10000.0f, 0.0f ) );
   light_0->setDirection( osg::Vec3f( -1, 1, -1 ) );

   light_source_0->setLight( light_0.get() );
   light_source_0->setLocalStateSetModes( osg::StateAttribute::ON );

#ifdef VE_PATENTED
   _tbvHandler = 0;
   _pbuffer = 0;
#endif
   _frameNumber = 0;
#endif
   this->argc = argc;
   this->argv = argv;
   //Set the deafult clear color of black
   clearColor.push_back( 0.0f );
   clearColor.push_back( 0.0f );
   clearColor.push_back( 0.0f );
   clearColor.push_back( 1.0f );
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::exit()
{
   VE_SceneGraph::cfdPfSceneManagement::instance()->CleanUp();
   cfdModelHandler::instance()->CleanUp();
   cfdEnvironmentHandler::instance()->CleanUp();
   cfdSteadyStateVizHandler::instance()->CleanUp();

#ifdef _OSG
#ifdef VE_PATENTED
   cfdTextureBasedVizHandler::instance()->CleanUp();
#endif
#endif

#ifdef _TAO
   cfdExecutive::instance()->CleanUp();
#endif // _TAO

#ifdef _WEB_INTERFACE
   runWebImageSaveThread=false;
   //vpr::System::msleep( 1000 );  // one-second delay
   delete writeWebImageFileThread;
   if(readyToWriteWebImage)   //if we've captured the pixels, but didn't write them out
      delete[] webImagePixelArray;   //delete the pixel array
#endif  //_WEB_INTERFACE
}

#ifdef _PERFORMER
inline void cfdApp::apiInit( )
{
   vrj::PfApp::apiInit();
   vprDEBUG( vesDBG, 1 ) << "cfdApp::apiInit" << std::endl << vprDEBUG_FLUSH;
   pfNotifyHandler( notifyHandler );
}

inline void cfdApp::preForkInit( )
{
   vrj::PfApp::preForkInit();
   vprDEBUG(vesDBG,1) << "cfdApp::preForkInit"
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
  vprDEBUG(vesDBG,1) << "cfdApp::preSync" << std::endl << vprDEBUG_FLUSH;
}
#endif

#ifdef _PERFORMER
inline pfGroup* cfdApp::getScene()
#elif _OSG
osg::Group* cfdApp::getScene()
#endif
{
   //osgDB::writeNodeFile(*this->_sceneManager->GetRootNode()->GetRawNode(),
   //   "C:/test.osg");
#ifdef _PERFORMER
   return (pfGroup*)(VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode()->GetRawNode());
#elif _OSG
   return (osg::Group*)VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode();
#endif
}

#ifdef _OSG
#ifdef VE_PATENTED
//#ifdef CFD_USE_SHADERS
////////////////////////////////////////////////////////////////////////////////
void cfdApp::contextInit()
{
	//Override the vrj::OsgApp::contextInit() default functionality
	//**************************************************************************
	unsigned int unique_context_id = vrj::GlDrawManager::instance()->getCurrentContext();

   // --- Create new context specific scene viewer -- //
   osg::ref_ptr< osgUtil::SceneView > new_sv( new osgUtil::SceneView );

	// Configure the new viewer
   this->configSceneView( new_sv.get() );             
   new_sv->getState()->setContextID( unique_context_id );

	(*sceneViewer) = new_sv;
   //This is important - if this is commented out then the screen goes black
   new_sv->getGlobalStateSet()->setAssociatedModes(light_0.get(),osg::StateAttribute::ON);
   new_sv->getGlobalStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::ON);
   //osg::ref_ptr< osg::LightModel > lightmodel = new osg::LightModel;
   //lightmodel->setAmbientIntensity(osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
   //new_sv->getGlobalStateSet()->setAttributeAndModes( lightmodel.get(), osg::StateAttribute::ON );
   // Add the tree to the scene viewer and set properties
   new_sv->setSceneData(getScene());

	//Setup OpenGL light
	//This should actualy be done in the simulator code
	//GLfloat light0_ambient[] = { 0.36862f, 0.36842f, 0.36842f, 1.0f };
	//GLfloat light0_diffuse[] = { 0.88627f, 0.88500f, 0.88500f, 1.0f };
	//GLfloat light0_specular[] = { 0.49019f, 0.48872f, 0.48872f, 1.0f };
	//GLfloat light0_position[] = { 10000.0f, 10000.0f, 10000.0f, 0.0f };

	//GLfloat mat_ambient[] = { 0.7f, 0.0f, 0.0f, 1.0f };
	//GLfloat mat_diffuse[] = { 0.8f, 0.0f, 0.0f, 1.0f };
	//GLfloat mat_specular[] = { 1.0f, 0.0f, 0.0f, 1.0f };
	//GLfloat mat_shininess[] = { 20.0f };
	//GLfloat mat_emission[] = { 1.0f, 1.0f, 1.0f, 1.0f };
	//GLfloat no_mat[] = { 0.0f, 0.0f, 0.0f, 1.0f};

	//glLightfv( GL_LIGHT0, GL_AMBIENT, light0_ambient );
	//glLightfv( GL_LIGHT0, GL_DIFFUSE, light0_diffuse );
	//glLightfv( GL_LIGHT0, GL_SPECULAR, light0_specular );
	//glLightfv( GL_LIGHT0, GL_POSITION, light0_position );

	//glMaterialfv( GL_FRONT, GL_AMBIENT, mat_ambient );
	//glMaterialfv( GL_FRONT, GL_DIFFUSE, mat_diffuse );
	//glMaterialfv( GL_FRONT, GL_SPECULAR, mat_specular );
	//glMaterialfv( GL_FRONT, GL_SHININESS, mat_shininess );
	//glMaterialfv( GL_FRONT, GL_EMISSION, no_mat );

	//glEnable( GL_DEPTH_TEST );
	//glEnable( GL_NORMALIZE );
	//glEnable( GL_LIGHTING );
	//glEnable( GL_LIGHT0 );
	//glEnable( GL_COLOR_MATERIAL );
	//glShadeModel( GL_SMOOTH );
	//**************************************************************************

   if ( !_pbuffer )
   {
      _pbuffer = new cfdPBufferManager();
      _pbuffer->isSupported();
   } 

   //if ( _tbvHandler )
   {
      _tbvHandler->SetPBuffer(_pbuffer);
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::contextClose()
{
   if(_pbuffer)
   {
      delete _pbuffer;
      _pbuffer = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
cfdPBufferManager* cfdApp::GetPBuffer()
{
   if ( _pbuffer )
   {
      return _pbuffer;
   }
   return 0;
}
//#endif
#endif
////////////////////////////////////////////////////////////////////////////////
void cfdApp::configSceneView( osgUtil::SceneView* newSceneViewer )
{
	//Override the vrj::OsgApp::configSceneView() default functionality
	//**************************************************************************
	newSceneViewer->setDefaults( osgUtil::SceneView::NO_SCENEVIEW_LIGHT | 
                                osgUtil::SceneView::COMPILE_GLOBJECTS_AT_INIT  );
	newSceneViewer->init();
	newSceneViewer->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );

	//Needed for stereo to work.
	newSceneViewer->setDrawBufferValue( GL_NONE );
	//**************************************************************************

	newSceneViewer->setSmallFeatureCullingPixelSize( 10 );

	newSceneViewer->setFrameStamp( _frameStamp.get() );

   //newSceneViewer->setComputeNearFarMode( osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::bufferPreDraw()
{
   //glClearColor(0.0, 0.0, 0.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
#endif //_OSG
////////////////////////////////////////////////////////////////////////////////
void cfdApp::SetWrapper( cfdVjObsWrapper* input )
{
   _vjobsWrapper = input;
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::initScene( void )
{
   vprDEBUG(vesDBG,0) << "cfdApp::initScene" << std::endl << vprDEBUG_FLUSH;
#ifdef _SGL
   CSGLVector2d length( 280.0, 210.0 );
	CSGLVector2i pixels( 1024, 768 );
	CSGLDisplay display( 60.0, 100.0, 700.0, 62.0, length, pixels );
	SGLContext.SetDisplay( display );
   
	// - set auto depth and frame cancelation flags
	SGLContext.Enable( SGL_DEPTH_AUTO|SGL_FRAME_CANCEL );
   
	// - set interlacing to stencil
	SGLContext.SetInterlacing( SGL_STENCIL );
   
	// - set vpi
	CSGLVPI vpi( SGL_VPI_SOLID, 10, 20, 1, 0 );
	SGLContext.SetVPI( vpi );
#endif
   
# ifdef _OPENMP
   std::cout << "\n\n\n";
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|          Compiled by an OpenMP-compliant implementation           |" << std::endl;
   std::cout << "|===================================================================|" << std::endl;
   std::cout << "|                                                                   |" << std::endl;
# endif // _OPENMP

   //Initialize all the XML objects
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("XML",new VE_XML::XMLCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("Shader",new VE_XML::VE_Shader::ShaderCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("Model",new VE_XML::VE_Model::ModelCreator());
   VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator("CAD",new VE_XML::VE_CAD::CADCreator());

   std::cout << std::endl;
   std::cout << "| ***************************************************************** |" << std::endl;
   _vjobsWrapper->InitCluster();

#ifdef _WEB_INTERFACE
   timeOfLastCapture = 0;
   runWebImageSaveThread = true;
   readyToWriteWebImage = false;
   writingWebImageNow = false;
   captureNextFrameForWeb = false;
#endif   //_WEB_INTERFACE

   // define the rootNode, worldDCS, and lighting
   //VE_SceneGraph::cfdPfSceneManagement::instance()->Initialize( this->filein_name );
   VE_SceneGraph::cfdPfSceneManagement::instance()->InitScene();


   this->getScene()->addChild( light_source_0.get() );

#ifdef _OSG
   VE_SceneGraph::cfdPfSceneManagement::instance()->ViewLogo(true);
#endif

   // modelHandler stores the arrow and holds all data and geometry
   //cfdModelHandler::instance()->Initialize( this->filein_name );
   cfdModelHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   cfdModelHandler::instance()->SetXMLCommand( _vjobsWrapper->GetXMLCommand() );
   cfdModelHandler::instance()->InitScene();
   
   // navigation and cursor 
   cfdEnvironmentHandler::instance()->Initialize();
   cfdEnvironmentHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   for(int i=1;i<argc;++i)
   {
      if((std::string( argv[i] ) == std::string("-VESDesktop"))&&(argc>=i+2))
      {
         cfdEnvironmentHandler::instance()->
                     SetDesktopSize( atoi( argv[i+1] ), atoi( argv[i+2] ) );
         //break;
      }
      else if ( std::string( argv[i] ) == std::string("-VESCluster") )
      {
         isCluster = true;
      }
   }
   cfdEnvironmentHandler::instance()->InitScene();
   cfdQuatCamHandler::instance()->SetMasterNode( _vjobsWrapper->IsMaster() );
   
   // create steady state visualization objects
   cfdSteadyStateVizHandler::instance()->Initialize( this->filein_name );
   cfdSteadyStateVizHandler::instance()->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   cfdSteadyStateVizHandler::instance()->InitScene();

   //create the volume viz handler
#ifdef _OSG
   _start_tick = _timer.tick();
#ifdef VE_PATENTED
   _tbvHandler = cfdTextureBasedVizHandler::instance();
   _tbvHandler->SetCommandArray( _vjobsWrapper->GetCommandArray() );
   //_tbvHandler->SetSceneView(_sceneViewer.get());
   //_tbvHandler->InitVolumeVizNodes();
#endif
#endif

#ifdef _TAO
   std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
   cfdExecutive::instance()->Initialize( _vjobsWrapper->naming_context, _vjobsWrapper->child_poa );
#endif // _TAO

   // This may need to be fixed
   this->_vjobsWrapper->GetCfdStateVariables();
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::preFrame( void )
{
   //Sets the worldDCS before it is synced
   cfdEnvironmentHandler::instance()->PreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::latePreFrame( void )
{
   static long lastFrame=0;
	//Used for framerate calculation as integers only
   static float lastTime=0.0f;

   vprDEBUG(vesDBG,3)<<"cfdApp::latePreFrame"<<std::endl<<vprDEBUG_FLUSH;

   //call the parent method
   _vjobsWrapper->GetUpdateClusterStateVariables();

	float current_time = this->_vjobsWrapper->GetSetAppTime( -1 );

#ifdef _OSG
   //This is order dependent
   //don't move above function call
   if( _frameStamp.valid() )
   {
      _frameStamp->setFrameNumber( _frameNumber++ );
      _frameStamp->setReferenceTime( current_time );
      //This is a frame rate calculation
      float deltaTime = current_time - lastTime;
      if( deltaTime >= 1.0f )
      {
         float framerate;
         framerate = _frameNumber - lastFrame;
			VE_Xplorer::cfdEnvironmentHandler::instance()->SetFrameRate( framerate );

         lastTime = current_time;
         lastFrame = _frameNumber;
      }
   }
#endif
         
   VE_SceneGraph::cfdPfSceneManagement::instance()->PreFrameUpdate();
   ///////////////////////
   cfdModelHandler::instance()->PreFrameUpdate();
   ///////////////////////
   cfdEnvironmentHandler::instance()->LatePreFrameUpdate(); 
   ///////////////////////
   svUpdate = cfdEnvironmentHandler::instance()->BackgroundColorChanged();
   ///////////////////////
   cfdSteadyStateVizHandler::instance()->PreFrameUpdate();
	

#ifdef _OSG
#ifdef VE_PATENTED
   if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      if ( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() )
      {
			_tbvHandler->SetParentNode((VE_SceneGraph::Group*)cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->GetChild(1) );
         _tbvHandler->SetActiveTextureDataSet(cfdModelHandler::instance()->GetActiveTextureDataSet());
         _tbvHandler->ViewTextureBasedVis(cfdModelHandler::instance()->GetVisOption());
         _tbvHandler->SetCurrentTime(this->_vjobsWrapper->GetSetAppTime(-1));
         _tbvHandler->PreFrameUpdate();
      }
   }
#endif
#endif

   if ( _vjobsWrapper->GetXMLCommand()->GetCommandName() == "EXIT_XPLORER" )
   {
      // exit cfdApp was selected
      vrj::Kernel::instance()->stop(); // Stopping kernel 
   }

   cfdExecutive::instance()->PreFrameUpdate();
   this->_vjobsWrapper->PreFrameUpdate();


	//Update physics objects with time passed from last frame
	//**********************************************************************
#ifdef _OSG
	//Used for physics_simulator to grab real dt per frame as floats
	static float previous_time = 0.0f;

	float dt = current_time - previous_time;
	//std::cout<<dt<<std::endl;
	VE_SceneGraph::PhysicsSimulator::instance()->UpdatePhysics( dt );
	previous_time = current_time;
#endif
	//**********************************************************************


   vprDEBUG(vesDBG,3) << " cfdApp::End latePreFrame" << std::endl << vprDEBUG_FLUSH;
#ifdef _OSG
   this->update();
#endif
}

void cfdApp::intraFrame()
{
   vprDEBUG(vesDBG,3) << " intraFrame" << std::endl << vprDEBUG_FLUSH;
   // Do nothing here
   // Usually slows things down
}
#ifdef VE_PATENTED
#ifdef _OSG
void cfdApp::contextPostDraw()
{
   //if(_tbvHandler)
     _tbvHandler->PingPongTextures();
}
#endif//_OSG
#endif//VE_PATENTED

void cfdApp::postFrame()
{
   vprDEBUG(vesDBG,3) << " postFrame" << std::endl << vprDEBUG_FLUSH;
   
#ifdef _OSG
   svUpdate = false;
   cfdEnvironmentHandler::instance()->ResetBackgroundColorUpdateFlag();
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


#ifdef _OSG
   this->_vjobsWrapper->GetSetAppTime( time_since_start );
	cfdEnvironmentHandler::instance()->PostFrameUpdate();
   //this->_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );
#endif   //_OSG
	cfdExecutive::instance()->PostFrameUpdate();
   this->_vjobsWrapper->GetCfdStateVariables();
   vprDEBUG(vesDBG,3) << " End postFrame" << std::endl << vprDEBUG_FLUSH;
}

//web interface thread for writing the file
#ifdef _WEB_INTERFACE

void cfdApp::captureWebImage()
{
   if(writingWebImageNow || readyToWriteWebImage) return;
   int dummyOx=0;
   int dummyOy=0;

   std::cout << "Reading viewport size...!" << std::endl;
   //get the viewport height and width
   vrj::GlDrawManager::instance()->currentUserData()->getGlWindow()->
      getOriginSize(dummyOx, dummyOy, webImageWidth, webImageHeight);
   std::cout << "Copying frame buffer "<< webImageWidth 
               << " " << webImageHeight << "......." << std::endl;
   captureNextFrameForWeb=false;      //we're not going to capture next time around
   std::string webImagePixelArray; //=new char[webImageHeight*webImageWidth*3];      //create an array to store the data
   glReadPixels(0, 0, webImageWidth, webImageHeight, GL_RGB, GL_UNSIGNED_BYTE, webImagePixelArray);   //copy from the framebuffer
   readyToWriteWebImage=true;      
}

void cfdApp::writeImageFileForWeb(void*)
{
   while(runWebImageSaveThread)
   {
      vpr::System::msleep( 500 );  // half-second delay
      if(readyToWriteWebImage)
      {
         readyToWriteWebImage=false;
         writingWebImageNow = true;
         //let's try saving the image with Corona
         corona::Image* frameCap=corona::CreateImage(webImageWidth, webImageHeight, corona::PF_R8G8B8, (void*)webImagePixelArray);
         frameCap=corona::FlipImage(frameCap, corona::CA_X);
         if(!corona::SaveImage("../../public_html/PowerPlant/VE/dump.png", corona::FF_PNG, frameCap))
            std::cout << "error saving image!" << std::endl;
         else 
            std::cout << "Image saved successfully.!" << std::endl;
         delete frameCap;
         delete [] webImagePixelArray;                              //delete our array
         std::cout << "All done!" << std::endl;
         writingWebImageNow = false;
      }
   }
}
#endif   //_WEB_INTERFACE

#ifdef _OSG
void cfdApp::contextPreDraw( void )
{
   if ( svUpdate )
   {
      osg::ref_ptr<osgUtil::SceneView> sv;
      sv = (*sceneViewer);    // Get context specific scene viewer
      if(sv.valid())
      {
         clearColor = VE_Xplorer::cfdEnvironmentHandler::instance()->GetBackgroundColor();
         sv->setClearColor(osg::Vec4(clearColor.at(0),clearColor.at(1),clearColor.at(2),1.0));
      }
   }
   glClearColor( clearColor.at(0),clearColor.at(1),clearColor.at(2),1.0);
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::draw()
{
#ifndef _SGL
   glClear(GL_DEPTH_BUFFER_BIT);
#endif
   //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   // Users have reported problems with OpenGL reporting stack underflow
   // problems when the texture attribute bit is pushed here, so we push all
   // attributes *except* GL_TEXTURE_BIT.
   glPushAttrib(GL_ALL_ATTRIB_BITS & ~GL_TEXTURE_BIT);
   glPushAttrib(GL_TRANSFORM_BIT);
   glPushAttrib(GL_VIEWPORT_BIT);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   
   // The code below is commented out because it causes 
   // problems with the cg shader code
   // for more details please contact Gerrick
   //glMatrixMode(GL_TEXTURE);
   //glPushMatrix();

   osg::ref_ptr<osgUtil::SceneView> sv;
   sv = (*sceneViewer);    // Get context specific scene viewer
   vprASSERT(sv.get() != NULL);

   /*if ( _vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_LOD_SCALE )
   {
      sv->setLODScale( (float)_vjobsWrapper->GetCommandArray()->GetCommandValue( cfdCommandArray::CFD_SC ));
   }*/
   vrj::GlDrawManager*    gl_manager;    /**< The openGL manager that we are rendering for. */
   gl_manager = vrj::GlDrawManager::instance();

   // Set the up the viewport (since OSG clears it out)
   float vp_ox, vp_oy, vp_sx, vp_sy;   // The float vrj sizes of the view ports
   int w_ox, w_oy, w_width, w_height;  // Origin and size of the window
   gl_manager->currentUserData()->getViewport()->getOriginAndSize(vp_ox, vp_oy, vp_sx, vp_sy);
   gl_manager->currentUserData()->getGlWindow()->getOriginSize(w_ox, w_oy, w_width, w_height);

   //gl_manager->currentUserData()->getProjection()->getViewMatrix();
  
   // compute unsigned versions of the viewport info (for passing to glViewport)
   unsigned ll_x = unsigned( vp_ox*float( w_width ) );
   unsigned ll_y = unsigned( vp_oy*float( w_height) );
   unsigned x_size = unsigned( vp_sx*float( w_width) );
   unsigned y_size = unsigned( vp_sy*float( w_height) );

   //There is no resize event as of now
   //cfdEnvironmentHandler::instance()->SetWindowDimensions(x_size,y_size);

   //sv->setCalcNearFar(false);
   //sv->setComputeNearFarMode(osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR);
   sv->setViewport(ll_x, ll_y, x_size, y_size);
   sv->getRenderStage()->setClearMask(GL_NONE);

   //Get the view matrix and the frustrum form the draw manager
   vrj::GlDrawManager* drawMan = dynamic_cast<vrj::GlDrawManager*>(this->getDrawManager());
   vprASSERT(drawMan != NULL);
   vrj::GlUserData* userData = drawMan->currentUserData();

   // get the current projection
   vrj::Projection* project = userData->getProjection();

   //Get the frustrum
   vrj::Frustum frustum = project->getFrustum();
   sv->setProjectionMatrixAsFrustum(frustum[vrj::Frustum::VJ_LEFT],
                                    frustum[vrj::Frustum::VJ_RIGHT],
                                    frustum[vrj::Frustum::VJ_BOTTOM],
                                    frustum[vrj::Frustum::VJ_TOP],
                                    frustum[vrj::Frustum::VJ_NEAR],
                                    frustum[vrj::Frustum::VJ_FAR]);

	//Allow trackball to grab frustum values to calculate FOVy
   cfdEnvironmentHandler::instance()->SetFrustumValues(frustum[vrj::Frustum::VJ_LEFT],
                                                       frustum[vrj::Frustum::VJ_RIGHT],
                                                       frustum[vrj::Frustum::VJ_TOP],
																		 frustum[vrj::Frustum::VJ_BOTTOM],
                                                       frustum[vrj::Frustum::VJ_NEAR],
                                                       frustum[vrj::Frustum::VJ_FAR]);

#ifdef _WEB_INTERFACE
   bool goCapture = false;         //gocapture becomes true if we're going to capture this frame
   if(userData->getViewport()->isSimulator())   //if this is a sim window context....
   {
      //Matrix44f headMat=mHead->getData();      //grab the head matrix
//      Matrix44f h=headMat;
//      glMultMatrixf(headMat.mData);         //and multiply to cancel it out of the modelview
//      gluLookAt(0, 100, 0, 0, 0, 0, 0, 0, -1);   //an overhead view
      if(captureNextFrameForWeb) goCapture=true;   //now if we're go for capture, we'll know for sure
   }
#endif   //_WEB_INTERFACE

#ifdef _SGL
   SGLContext.Begin(SGL_FORMAT_FRAME);
   SGLContext.Begin(SGL_LEFT_FRAME);
#endif// _SGL
   gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
#ifdef _SGL
   gmtl::Matrix44f _vjMatrixLeft( userData->getViewport()->getLeftProj()->getViewMatrix() );
#else
   gmtl::Matrix44f _vjMatrixLeft( project->getViewMatrix() );
#endif// _SGL
   gmtl::postMult(_vjMatrixLeft, gmtl::makeRot<gmtl::Matrix44f>( gmtl::AxisAnglef( gmtl::Math::deg2Rad(-90.0f), x_axis ) ));
   //copy the matrix
   osg::ref_ptr<osg::RefMatrix> osg_proj_xform_mat = new osg::RefMatrix;
   osg_proj_xform_mat->set( _vjMatrixLeft.mData );
   
   // set the view matrix
   sv->setViewMatrix(*(osg_proj_xform_mat.get()) );
   
   sv->cull();
   sv->draw();
#ifdef _SGL
   SGLContext.End();
   
   SGLContext.Begin(SGL_RIGHT_FRAME);
   gmtl::Matrix44f _vjMatrixRight( userData->getViewport()->getRightProj()->getViewMatrix() );
   gmtl::postMult(_vjMatrixRight, gmtl::makeRot<gmtl::Matrix44f>( gmtl::AxisAnglef( gmtl::Math::deg2Rad(-90.0f), x_axis ) ));
   osg_proj_xform_mat->set( _vjMatrixRight.mData );
   
   // Copy the view matrix
   sv->setViewMatrix(*(osg_proj_xform_mat.get()) );
   
   sv->cull();
   sv->draw();
   SGLContext.End();
	
   SGLContext.End();
#endif// _SGL

#ifdef _WEB_INTERFACE
   if(goCapture)
      captureWebImage();
#endif   //_WEB_INTERFACE
   //glMatrixMode(GL_TEXTURE);
   //glPopMatrix();

   glMatrixMode(GL_PROJECTION);
   glPopMatrix();

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

   glPopAttrib();
   glPopAttrib();
   glPopAttrib();
}
////////////////////////////////////////////////////////////////////////////////
void cfdApp::update( void )
{
	// Update the frame stamp with information from this frame
	//frameStamp->setFrameNumber( getFrameNumber() );
   //frameStamp->setReferenceTime( getFrameTime().secd() );
	
	// Set up the time and frame number so time dependant things (animations, particle system)
	// function correctly
	mUpdateVisitor->setTraversalNumber( _frameNumber );//getFrameNumber() ); // I'm not sure if this is nessisary
	mUpdateVisitor->setFrameStamp( _frameStamp.get() ); 
	
	// update the scene by traversing it with the the update visitor which will
	// call all node update callbacks and animations. This is equivalent to calling
	// SceneView::update
	getScene()->accept(*mUpdateVisitor);
}
#endif //_OSG
