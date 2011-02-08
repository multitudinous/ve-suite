/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef VES_XPLORER_APP_H
#define VES_XPLORER_APP_H

// --- VE-Suite Includes --- //
#include "SceneRenderToTexturePtr.h"
#include "SceneGLTransformInfoPtr.h"

#include <ves/xplorer/TextureBasedVizHandlerPtr.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <ves/xplorer/Logging.h>

// ---  VR Juggler Includes --- //
#include <vrj/vrjParam.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/CondVar.h>

#include <vrj/Draw/OSG/App.h>
#include <vrj/Draw/OpenGL/ContextData.h>

// --- OSG Includes --- //
#include <osg/Version>
#include <osg/ref_ptr>
#include <osg/Timer>
#include <osg/LightModel>

// --- Boost Includes --- //
#include <boost/signals2/signal.hpp>


// --- Poco Includes --- //
#include <Poco/Logger.h>

class QApplication;

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

// --- STL Includes --- //
#include <sstream>

namespace ves
{
namespace xplorer
{
class VjObsWrapper;

namespace volume
{
#ifdef _PBUFFER
class cfdPBufferManager;
#endif
} //end volume

/*!\file App.h
 * App API
 */

/*!\class ves::xplorer::App
 *
 * Signals emitted:
 *      "App.LatePreFrame" -- allows sync to draw loop
 */
class App : public vrj::osg::App
{
public:
    ///Contructor
    App( int argc, char* argv[], bool enableRTT );

    ///Destructor
    virtual ~App();

    ///Initialize the scene graph
    virtual void initScene();

    ///Juggler calls before exiting
    virtual void exit();

    ///Get the raw group node
    virtual osg::Group* getScene();

    ///This gets called when??
    ///Note: Remember that this is called in parrallel in a multiple context
    ///      situation so setting variables should not be done here
    virtual void bufferPreDraw();

    ///This is our gl draw function
    ///Note: Remember that this is called in parrallel in a multiple context
    ///      situation so setting variables should not be done here
    virtual void draw();

    ///Configure the scene view on a per context basis
    virtual void configSceneView( osgUtil::SceneView* newSceneViewer );

    ///After the preframe calls but still have a vaild context
    ///Note: Remember that this is called in parrallel in a multiple context
    ///      situation so setting variables should not be done here
    virtual void contextPreDraw();

    ///After the draw call but still have a vaild context
    ///Note: Remember that this is called in parrallel in a multiple context
    ///      situation so setting variables should not be done here
    virtual void contextPostDraw();

    ///Initialize a context
    virtual void contextInit();

    ///Close a context
    virtual void contextClose();

    ///Signal to change the background color
    void ChangeBackgroundColor();

#ifdef _PBUFFER
    ///Get the pbuffer
    ///should remove this since pbuffer is a singleton
    ves::xplorer::volume::cfdPBufferManager* GetPBuffer();
#endif //_PBUFFER

    ///Override default vrj implementation
    //virtual osgUtil::SceneView::Options getSceneViewDefaults();

    ///Function called after pfSync and before pfDraw
    virtual void preFrame();

    ///Function called after pfSync and before pfDraw
    ///Function called after preFrame() and application-specific data
    ///syncronization (in a cluster configuration)
    ///but before the start of a new frame.
    virtual void latePreFrame();

    ///Function called after pfDraw
    virtual void intraFrame();

    ///Function called after intraFrame
    virtual void postFrame();

    ///Used to override getFrameBufferAttrs()
    ///Should be able to set multi sampling in the config
    ///Look for a fix in future juggler releases
    ///std::vector< int > getFrameBufferAttrs();
    ///Push data to state info shoudl be removed
    void pushDataToStateInfo();

    ///Set the wrapper for vjobs so that we can change things
    void SetWrapper( VjObsWrapper* );

    ///Update the framestamp and traverse the scenegraph
    void update();

    ///Start up in-process user interface thread
    void LoadUI();
    
    ///Calls LoadUI and other Qt initialization tools
    void preRun();
    
    ///Calls the Qt event loop
    void runLoop();
    
protected:

private:
    /// Slot connected to signal "UIManager.EnterLeaveUI", called whenever
    /// mouse enters or leaves UI quad
    void UIEnterLeave( bool entered );

    ///Set the near/far slor
    void SetNearFarRatio( bool const& enable, double const& nearFar );
    
    ///Update sceneview
    bool svUpdate;

    ///Are we in cluster mode
    bool isCluster;

    ///Not sure what this is for
    bool m_captureNextFrame;

    ///Not sure what this is for
    bool m_captureMovie;

    ///Turn off/on RTT
    bool mRTT;

    /// Is the UI initialized?
    bool m_uiInitialized;

    /// Is the mouse inside the UI quad?
    bool m_MouseInsideUI;

    ///
    vrj::opengl::ContextData< bool > mViewportsChanged;

    ///
    vrj::opengl::ContextData< bool > m_skipDraw;

    ///The current frame number
    unsigned int _frameNumber;

    ///Used to count frames specifically for profilling
    unsigned int mProfileCounter;

    //Only used in preframe for transient stuff
    ///The last frame
    int lastFrame;

    ///Not sure what this is for
    int webImageWidth;

    ///Not sure what this is for
    int webImageHeight;

    ///Command line args
    int argc;

    ///Command line args
    char** argv;

    ///The last frame executed
    long mLastFrame;

    ///Used for framerate calculation as integers only
    float mLastTime;

    ///Last time from last frame
    float mLastFrameTime;

    ///The frame delta time
    float mFrameDT;

    ///Time to start
    double time_since_start;

    ///
    double mLastQtLoopTime;

    ///A mutex to protect variables accesses
    vpr::Mutex mValueLock;
    ///A mutex to protect variables accesses
    vpr::Mutex m_signalLock;

    ///File name for screen capture filename
    std::string m_filename;

    ///Stream buffer to write stats too
    std::ostringstream mStatsStream;

    ///The timer for framestamp
    osg::Timer _timer;

    ///The timer for framestamp
    osg::Timer_t _start_tick;

    ///Update visitor
    osg::ref_ptr< osg::NodeVisitor > mUpdateVisitor;

    ///Framestamp
    osg::ref_ptr< osg::FrameStamp > mFrameStamp;

    ///Light for the scene
    osg::ref_ptr< osg::Light > light_0;

    ///Light source for the scene
    osg::ref_ptr< osg::LightSource > light_source_0;

    ///Light model for the scene
    osg::ref_ptr< osg::LightModel > light_model_0;

    ///User nav position for camera
    gmtl::Matrix44d mNavPosition;

    ///Sound listener position matrix
    osg::Matrixd m_listenerPosition;

    ///The vjobs wrapper
    VjObsWrapper* m_vjobsWrapper;

#ifdef _PBUFFER
    //biv --may convert this to a singleton later
    ///Should be removed since this is a singleton
    ves::xplorer::volume::cfdPBufferManager* _pbuffer;
#endif //_PBUFFER

    ///Should be removed since this is a singleton
    ves::xplorer::TextureBasedVizHandler* _tbvHandler;

    ///The RTT manager
    ves::xplorer::SceneRenderToTexturePtr mSceneRenderToTexture;

    ///The manager of the window stack
    ves::xplorer::SceneGLTransformInfoPtr m_sceneGLTransformInfo;

    ///Thread to run the Qt ui
    //vpr::Thread* m_qtUIThread;

    /// The UI
    QApplication* m_qtApp;

    /// Required for connecting to signals via EventManager
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;

    /// Signal "App.LatePreFrame", emitted during LatePreFrame to allow
    /// other objects to sync operations to the draw loop
    typedef boost::signals2::signal< void () > latePreFrame_SignalType;
    latePreFrame_SignalType mLatePreFrame;
    ///Logger
    Poco::Logger& m_logger;
    ///Logger stream
    ves::xplorer::LogStreamPtr m_logStream;
    ///Try to tell when we have a valid context
    bool m_windowIsOpen;
    ///Tell when the near far ratio is set
    vrj::opengl::ContextData< bool > m_setNearFarRatio;
    ///The near far ratio
    double m_nearFarRatio;
    ///The framenumber for comparison of setting near far
    unsigned int m_frameSetNearFarRatio;
    /// Required to be able to connect up to signals.
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;
    ///Mutex to sync the draw thread with the signal loop
    vpr::CondVar m_syncCond;
    ///Tell when to process signals    
    bool m_processSignals;
};
} //end xplorer
} //end ves

#endif //VES_XPLORER_APP_H
