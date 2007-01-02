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
#ifndef CFD_APP_H
#define CFD_APP_H
/*!\file cfdApp.h
cfdApp API
*/

/*!\class VE_Xplorer::cfdApp
*
*/
namespace VE_SceneGraph
{
   class cfdDCS;
}

namespace VE_Xplorer
{
   class cfdVjObsWrapper;
}

// Scene graph dependent forward declarations
// The sleep time for sampling of threads.
const float SAMPLE_TIME = 1.0f;

#ifdef _PERFORMER
class pfGroup;
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
// Declare my application class
#elif _OSG
#include <osg/Timer>
#include <osg/Geode>
#include <osgText/Text>
#include <osgText/Font>
#include <vrj/Draw/OSG/OsgApp.h>
#include <vpr/Sync/Mutex.h>

namespace osg
{  
   class Group;
   class FrameStamp;
}
namespace osgText
{
   class Text;
}
namespace osgUtil 
{ 
   class SceneView;
   class UpdateVisitor;
}
#ifdef _WEB_INTERFACE
   #include <vpr/Thread/Thread.h>
#endif
#ifdef VE_PATENTED
namespace VE_TextureBased
{
   class cfdPBufferManager;
   class cfdTextureBasedVizHandler;
}
#endif
#endif 

namespace VE_Xplorer
{
#ifdef _PERFORMER
class cfdApp : public vrj::PfApp
#elif _OSG
class cfdApp: public vrj::OsgApp
#elif _OPENSG
#endif
{
public:
   cfdApp( int argc, char* argv[] );
   virtual ~cfdApp( void ) { ; }
  
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
   virtual osg::Group* getScene( void );
   virtual void bufferPreDraw( void );
   virtual void draw();
   virtual void configSceneView(osgUtil::SceneView* newSceneViewer);
   virtual void contextPreDraw( void );

   ///Initialize the text layout for framerate
   void InitFrameRateText();

   ///Signal to change the background color
   void ChangeBackgroundColor();

#ifdef VE_PATENTED
   virtual void contextInit( void );
   virtual void contextClose( void );
   VE_TextureBased::cfdPBufferManager* GetPBuffer( void );
   virtual void contextPostDraw();
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
   bool svUpdate;
   //osg::ref_ptr<osgUtil::SceneView> tempSvVector;
#ifdef VE_PATENTED
   VE_TextureBased::cfdTextureBasedVizHandler* _tbvHandler;
   //biv --may convert this to a singleton later
   VE_TextureBased::cfdPBufferManager* _pbuffer;
#endif
   osg::ref_ptr<osg::FrameStamp> _frameStamp;
   osg::Timer _timer;
   osg::Timer_t _start_tick;
   unsigned int _frameNumber;
#endif
   cfdVjObsWrapper*              _vjobsWrapper;

   // Only used in preframe for transient stuff
   int   lastFrame;
   void update();
private:
   bool isCluster;
   vpr::Mutex mValueLock;  /**< A mutex to protect variables accesses */
   std::string filein_name;
	double time_since_start;
   int argc;
   char** argv;
   std::vector<float> _clearColor;///<The clear color
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
	std::string webImagePixelArray;
	void writeWebImageFile(void*);
	void captureWebImage();
	double timeOfLastCapture;
#endif 
#ifdef _OSG
   osg::ref_ptr<osg::NodeVisitor> mUpdateVisitor;
   osg::ref_ptr<osg::FrameStamp> frameStamp;

   VE_SceneGraph::cfdDCS* framerate_dcs;
   osg::ref_ptr<osg::Geode> framerate_geode;
   osg::ref_ptr<osgText::Text> framerate_text;
   osg::ref_ptr<osgText::Font> framerate_font;
#endif
};
}
#endif
