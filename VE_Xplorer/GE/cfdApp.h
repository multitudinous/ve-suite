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
#ifndef CFD_APP_H
#define CFD_APP_H

/*!\file cfdApp.h
cfdApp API
*/

/*!\class VE_Xplorer::cfdApp
*
*/


namespace VE_Xplorer
{
   class cfdVjObsWrapper;
}

// Scene graph dependent forward declarations
// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

#ifdef _OSG

   #include <osg/Version>
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>=2) || (OSG_VERSION_MAJOR>=2))
   #include <osg/ref_ptr>
   #include <osg/Timer>
   #include <osg/LightModel>
#endif

#include <vrj/vrjParam.h>
#if __VJ_version >= 2003000
#include <vrj/Draw/OSG/App.h>
#else
#include <vrj/Draw/OSG/OsgApp.h>
#endif
#include <vpr/Sync/Mutex.h>

   namespace osg
   {  
      class Group;
      class FrameStamp;
      class Light;
      class LightSource;
   }

   namespace osgUtil 
   { 
      class SceneView;
      class UpdateVisitor;
   }

      namespace VE_TextureBased
      {
         class cfdPBufferManager;
         class cfdTextureBasedVizHandler;
      }
#endif //_PERFORMER _OSG

namespace VE_Xplorer
{
#ifdef _PERFORMER
class cfdApp : public vrj::PfApp
#elif _OSG
#if __VJ_version >= 2003000
class cfdApp : public vrj::osg::App
#else
class cfdApp : public vrj::OsgApp
#endif
#elif _OPENSG
#endif //_PERFORMER _OSG _OPENSG
{
public:
   ///Contructor
   cfdApp( int argc, char* argv[] );
   ///destructor
   virtual ~cfdApp( void ) { ; }
   /// Initialize the scene graph
   virtual void initScene( void );
   /// Juggler calls before exiting
   virtual void exit( void );

#ifdef _OSG
   ///Get the raw group node
   virtual osg::Group* getScene( void );
   ///This gets called when??
   ///Note: Remember that this is called in parrallel in a multiple context 
   ///      situation so setting variables should not be done here
   virtual void bufferPreDraw( void );
   ///This is our gl draw function
   ///Note: Remember that this is called in parrallel in a multiple context 
   ///      situation so setting variables should not be done here
   virtual void draw();
   ///Configure the scene view on a per context basis
   virtual void configSceneView( osgUtil::SceneView* newSceneViewer );
   ///after the preframe calls but still have a vaild context
   ///Note: Remember that this is called in parrallel in a multiple context 
   ///      situation so setting variables should not be done here
   virtual void contextPreDraw( void );
   ///after the draw call but still have a vaild context
   ///Note: Remember that this is called in parrallel in a multiple context 
   ///      situation so setting variables should not be done here
   virtual void contextPostDraw();
   ///Signal to change the background color
   void ChangeBackgroundColor();
   ///Initialize a context 
   virtual void contextInit( void );
   ///close a context
   virtual void contextClose( void );
   ///Get the pbuffer
   ///should remove this since pbuffer is a singleton
   VE_TextureBased::cfdPBufferManager* GetPBuffer( void );
   ///Override default vrj implementation
   osgUtil::SceneView::Options getSceneViewDefaults();

#elif _OPENSG
#endif //_PERFORMER _OSG _OPENSG
  
   void writeImageFileForWeb(void);

   /// Function called after pfSync and before pfDraw
   virtual void preFrame( void );
   /// Function called after pfSync and before pfDraw
   /// Function called after preFrame() and application-specific data 
   ///syncronization (in a cluster configuration) but before the start of a new frame.
   virtual void latePreFrame( void );
   /// Function called after pfDraw
   virtual void intraFrame( void );
   /// Function called after intraFrame
   virtual void postFrame( void );
   /// Used to override getFrameBufferAttrs()
   /// Should be able to set multi sampling in the config
   /// Look for a fix in future juggler releases
   ///std::vector< int > getFrameBufferAttrs( void );
   ///Push data to state info shoudl be removed 
   void pushDataToStateInfo( void );
   ///Set the wrapper for vjobs so that we can change things
   void SetWrapper( cfdVjObsWrapper* );

#ifdef _OSG
   bool svUpdate; ///< update sceneview
   VE_TextureBased::cfdTextureBasedVizHandler* _tbvHandler;///< should be removed since this is a singleton
   //biv --may convert this to a singleton later
   VE_TextureBased::cfdPBufferManager* _pbuffer;///< should be removed since this is a singleton
   osg::ref_ptr< osg::FrameStamp > _frameStamp;///<The framestamp to control animations
   osg::Timer _timer;///<The timer for framestamp
   osg::Timer_t _start_tick;///< The timer for framestamp
   unsigned int _frameNumber;///< the current frame number
#endif
   cfdVjObsWrapper*              _vjobsWrapper;///< the vjobs wrapper

   // Only used in preframe for transient stuff
   int   lastFrame;///The last frame
   void update();///< update the framestamp and traverse the scenegraph
private:
    bool isCluster;///< are we in cluster mode

    vpr::Mutex mValueLock;  ///< A mutex to protect variables accesses
    std::string filein_name;///< file name for something should be removed
    double time_since_start;///< time to start
    int argc;///< command line args
    char** argv;///< command line args
	bool runWebImageSaveThread;///< not sure what this is for
	bool readyToWriteWebImage;///< not sure what this is for
	bool writingWebImageNow;///< not sure what this is for
	bool captureNextFrameForWeb;///< not sure what this is for
	int webImageWidth;///< not sure what this is for
	int webImageHeight;///< not sure what this is for

#ifdef _OSG
   osg::ref_ptr<osg::NodeVisitor> mUpdateVisitor;///<update visitor
   osg::ref_ptr<osg::FrameStamp> frameStamp;///<framestamp
   osg::ref_ptr< osg::Light > light_0;///< ligth for the scene
   osg::ref_ptr< osg::LightSource > light_source_0;///< light source for the scene
   osg::ref_ptr< osg::LightModel > light_model_0;///< light model for the scene
#endif
};
}
#endif
