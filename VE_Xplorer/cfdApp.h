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
 * File:          $RCSfile: cfdApp.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_APP_H
#define CFD_APP_H

#ifdef _TAO
class cfdExecutive;
#endif

class cfdVjObsWrapper;

// Scene graph dependent forward declarations
// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

#ifdef _PERFORMER
class pfGroup;
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
// Declare my application class
class cfdApp : public vrj::PfApp
#elif _OSG
#include <osg/Timer>
#include <vrj/Draw/OSG/OsgApp.h>
#include <vpr/Sync/Mutex.h>
namespace osg
{   
   class Group;
   class FrameStamp;
} 
namespace osgUtil { class SceneView; }
#ifdef _WEB_INTERFACE
#include <vpr/Thread/Thread.h>
#endif
#ifdef VE_PATENTED
namespace VE_TextureBased{
   class cfdPBufferManager;
}
class cfdTextureBasedVizHandler;
using namespace VE_TextureBased;
#endif
class cfdApp: public vrj::OsgApp
#elif _OPENSG
#endif
{
   public:
      cfdApp( void );
      ~cfdApp( void ) { ; }
     
      // Initialize the scene graph
      virtual void initScene( void );

      // Juggler calls before exiting
      virtual void exit( void );

#ifdef _PERFORMER
      virtual void apiInit( void );

      // Called After pfInit()
      virtual void preForkInit( void );
      // Called Before pfConfig()

      virtual void appChanFunc( pfChannel* chan );

      // Return the current scene graph
      virtual pfGroup* getScene( void );
      
      // Function called before pfSync
      virtual void preSync( void );
#elif _OSG
      osg::Group* getScene( void );
      void bufferPreDraw( void );
      void draw();
      virtual void configSceneView(osgUtil::SceneView* newSceneViewer);
#ifdef VE_PATENTED
      void contextInit( void );
      virtual void contextClose( void );
      cfdPBufferManager* GetPBuffer( void );
      void contextPostDraw();
#endif
#elif _OPENSG
#endif
     
#ifdef _WEB_INTERFACE
      void writeImageFileForWeb(void*);
#endif
      // Function called by the DEFAULT drawChan function 
      // before clearing the channel
      // and drawing the next frame (pfFrame( ))
      //virtual void preDrawChan(pfChannel* chan, void* chandata);


      // Function called after pfSync and before pfDraw
      virtual void preFrame( void );

      // Function called after pfSync and before pfDraw
      //Function called after preFrame() and application-specific data syncronization (in a cluster configuration) but before the start of a new frame.
      virtual void latePreFrame( void );

      // Function called after pfDraw
      virtual void intraFrame( void );

      // Function called after intraFrame
      virtual void postFrame( void );

      

      // Used to override getFrameBufferAttrs()
      // Should be able to set multi sampling in the config
      // Look for a fix in future juggler releases
      //std::vector< int > getFrameBufferAttrs( void );

      void pushDataToStateInfo( void );

      void SetWrapper( cfdVjObsWrapper* );
      //void SetCORBAVariables( CosNaming::NamingContext_ptr, CORBA::ORB_ptr, PortableServer::POA_ptr );

#ifdef _OSG
#ifdef VE_PATENTED
      cfdTextureBasedVizHandler* _tbvHandler;
      //biv --may convert this to a singleton later
      cfdPBufferManager* _pbuffer;
#endif
      osg::ref_ptr<osg::FrameStamp> _frameStamp;
      osg::Timer _timer;
      osg::Timer_t _start_tick;
      unsigned int _frameNumber;
#endif
      cfdVjObsWrapper*              _vjobsWrapper;
#ifdef _TAO
      cfdExecutive*     executive;
#endif

      // Only used in preframe for transient stuff
      int   lastFrame;
   private:
      vpr::Mutex mValueLock;  /**< A mutex to protect variables accesses */
      char * filein_name;
	   double time_since_start;
	   //web interface stuff for writing the image file
	   //to be viewed over the web
#ifdef _WEB_INTERFACE
	   bool runWebImageSaveThread;
	   bool readyToWriteWebImage;
	   bool writingWebImageNow;
	   bool captureNextFrameForWeb;
	   int webImageWidth;
	   int webImageHeight;
	   vpr::Thread* writeWebImageFileThread;			//thread in which we write to the file
	   char* webImagePixelArray;
	   void writeWebImageFile(void*);
	   void captureWebImage();
	   double timeOfLastCapture;
#endif 
};

#endif
