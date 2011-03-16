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

#ifndef VES_XPLORER_SCENEGRAPH_SCENEMANAGER_H
#define VES_XPLORER_SCENEGRAPH_SCENEMANAGER_H

// --- VES Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>
#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/highlight/HighlightManager.h>
#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/FrameStamp>
#include <osg/Matrix>

#ifdef VE_SOUND
#include <osgAudio/Config.h>

#if defined( ENABLE_SUBSYSTEM_FMOD ) && defined( VPR_OS_Linux )
namespace FMOD
{
class System;
}
#endif
#endif

// --- VR Juggler Includes --- //
#include <vrj/vrjParam.h>

#include <vrj/Display/ViewportPtr.h>

#include <vpr/Util/Singleton.h>

#include <gadget/Type/PositionInterface.h>

#include <gmtl/Matrix.h>
#include <gmtl/Point.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class Group;
class Switch;

#ifdef VE_SOUND
class Sound;
#endif
class CharacterController;

namespace camera
{
class CameraManager;
}

namespace highlight
{
class HighlightManager;
}

namespace manipulator
{
class ManipulatorManager;
}

/*!\file SceneManager.h
 *
 */

/*!\class ves::xplorer::scenegraph::SceneManager
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS SceneManager
{
public:
    ///Return the active nav switch node of the scenegraph
    ///\return The DCS that should be used for nav matrix generation
    DCS* GetActiveNavSwitchNode() const;

    ///Return the active switch node of the scenegraph
    osg::Group* GetActiveSwitchNode() const;

    ///Return the camera manager of the scenegraph
    camera::CameraManager& GetCameraManager() const;

    ///
    ///\return
    CharacterController& GetCharacterController() const;

    ///
    osg::Uniform& GetClearColorUniform() const;

    ///Get the framestamp used by SceneView
    ///\return The osg::FrameStamp for the osg::SceneView
    osg::FrameStamp* GetFrameStamp() const;

    ///
    ///\return
    GLTransformInfoPtr const GetGLTransformInfo(
        vrj::ViewportPtr const viewport );

    ///
    ///\return
    GLTransformInfoPtr const GetCurrentGLTransformInfo();
    
    ///
    ///
    void SetCurrentGLTransformInfo( GLTransformInfoPtr const transformInfo );
    
    ///Return the graphical plugin manager
    Group& GetGraphicalPluginManager() const;

    ///
    highlight::HighlightManager& GetHighlightManager() const;

    ///Return the manipulator root node of the scenegraph
    manipulator::ManipulatorManager& GetManipulatorManager() const;

    ///Return the model root node of the scenegraph
    ///\return
    osg::Group* GetModelRoot() const;

    ///Return the network DCS of the scenegraph
    osg::Group* GetNetworkDCS() const;
    
    ///Return the root node of the scenegraph
    osg::Group* GetRootNode() const;

    ///Return the world DCS of the scenegraph
    ///\return The world DCS
    DCS* GetNavDCS() const;

    ///Get the inverted world DCS matrix
    ///\return The inverted matrix
    const gmtl::Matrix44d& GetInvertedNavMatrix() const;
    
    ///Get the inverted world DCS matrix
    ///\return The inverted matrix
    const osg::Matrixd& GetInvertedNavMatrixOSG() const;

    ///Return the head matrix
    ///\return The matrix
    const gmtl::Matrix44d& GetHeadMatrix() const;

    ///Return the global view matrix
    ///\return The matrix
    const gmtl::Matrix44d& GetGlobalViewMatrix() const;

    ///Return the global view matrix
    ///\return The matrix
    const osg::Matrixd& GetGlobalViewMatrixOSG() const;
    
    ///Return the global view matrix
    ///\return The inverted matrix
    const gmtl::Matrix44d& GetInvertedGlobalViewMatrix() const;
    
    ///Return the global view matrix
    ///\return The inverted matrix
    const osg::Matrixd& GetInvertedGlobalViewMatrixOSG() const;
    
    ///Initialize member variables for scene manager
    void Initialize();

    ///Initialize the scene
    void InitScene();

    ///LatePreFrameUpdate call to sync DCS information across cluster
    void LatePreFrameUpdate();

    ///Called after the draw function
    void PostFrameUpdate();

    ///
    void PushBackGLTransformInfo(
        vrj::ViewportPtr viewport,
        GLTransformInfoPtr glTransformInfo );

    ///Set the node on the switch node that is active
    ///\param activeNode The node to activate
    void SetActiveSwitchNode( int activeNode );

    ///Set the background color
    ///\param color the color to set the background color
    void SetBackgroundColor( std::vector< double > color );

    ///Set the framestamp to make it accessible to other areas of ves
    ///This can be used when traditional access through osg does not work
    ///for example when plugins need to set the SimulationTime
    void SetFrameStamp( osg::FrameStamp* frameStamp );

    ///Set the root node from the render to texture class
    void SetRootNode( osg::Group* rootNode );

    ///Shutdown the sound manager and other extra engines used in the world
    void Shutdown();

    ///Switch the logo on and off
    ///\param trueFalse Turn the logo on and off
    void ViewLogo( bool trueFalse );

    ///Tell if RTT is on
    bool IsRTTOn();

    ///Set the RTT flag on
    void SetRTT( bool isRTTOn );

    ///Set if we are in desktop mode
    void SetDesktopMode( bool isDesktopMode );

    ///Tell if we are in Desktop mode
    bool IsDesktopMode();

    ///Set if we are in desktop mode
    void SetScreenAlignedNormals( bool isScreenAligned );

    ///Tell if we are in Desktop mode
    bool IsScreenAligned();

    ///Tell if we are the master node
    ///Returns true if in desktop mode as well
    bool IsMasterNode();

    ///Set if it is the master node
    void SetMasterNode( bool isMasterNode );
    
    ///Set device handler group node
    void SetDeviceHandlerGroup( osg::Group* deviceGroup );

    ///Get device handler group node
    osg::Group& GetDeviceHandlerGroup();
    
    ///Get the center point
    gmtl::Point3d& GetCenterPoint();
    
#if defined( VE_SOUND ) && defined( ENABLE_SUBSYSTEM_FMOD ) && defined( VPR_OS_Linux )
    ///Configure the System interface of FMOD
    void  ConfigureFMODSystem( FMOD::System* system );
#endif

protected:
    ///Create the model for the logo
    void _createLogo();

private:
    //Required so that vpr::Singleton can instantiate this class
    //Friend class vpr::Singleton< SceneManager >;
    //SceneManager(const SceneManager& o){;}
    //SceneManager& operator=(const SceneManager& o){;}

    ///Base Constructor
    SceneManager();

    ///Destructor
    ~SceneManager();

    ///
    vprSingletonHeader( SceneManager );

    ///The root node of our scenegraph
    osg::ref_ptr< osg::Group > mRootNode;

    ///The root model node of our scenegraph
    osg::ref_ptr< osg::Group > mModelRoot;
    //osg::ref_ptr< DCS > mModelRoot;

    ///
    osg::ref_ptr< Group > m_graphicalPluginManager;

    ///
    osg::ref_ptr< camera::CameraManager > m_cameraManager;

    ///
    osg::ref_ptr< highlight::HighlightManager > m_highlightManager;

    ///The root manipulator node of our scenegraph
    ///If we add all manipulators under this node,
    ///we don't have to traverse the whole scenegraph to find manipulators
    osg::ref_ptr< manipulator::ManipulatorManager > m_manipulatorManager;

    ///The node which contains our logo
    osg::ref_ptr< DCS > mLogoNode;

    ///Node to switch between the logo and the worldDCS
    osg::ref_ptr< Switch > mLogoSwitch;

    ///Node to switch between the nav dcs for logo, world, and network
    osg::ref_ptr< Switch > mNavSwitch;

    ///A convenience pointer to help find which node is being used for nav
    osg::ref_ptr< DCS > mActiveNavDCS;

    ///Node to control navigation
    osg::ref_ptr< DCS > m_navDCS;

    ///Node to hold a network view of the system under investigation
    osg::ref_ptr< osg::Group > mNetworkDCS;

    ///VR Juggler head matrix
    gmtl::Matrix44d m_vrjHeadMatrix;

    ///Inverteded world dcs values
    gmtl::Matrix44d m_invertedNavMatrix;

    ///Inverted OSG nav matrix
    osg::Matrixd m_invertedNavMatrixOSG;

    ///Inverteded world dcs values
    gmtl::Matrix44d m_invertedGlobalViewMatrix;

    ///Inverted OSG view matrix
    osg::Matrixd m_invertedGlobalViewMatrixOSG;

    ///Inverteded world dcs values
    gmtl::Matrix44d m_globalViewMatrix;

    ///Inverted OSG view matrix
    osg::Matrixd m_globalViewMatrixOSG;

#ifdef VE_SOUND
    ///Sound file to play as background audio for VE-Suite
    Sound* m_sound;
#endif

    ///
    osg::ref_ptr< osg::Uniform > m_clearColorUniform;

    ///FrameStamp to control sequence nodes and other osg animations
    osg::ref_ptr< osg::FrameStamp > mFrameStamp;

    ///
    CharacterController* mCharacterController;

    ///Flag to tell if RTT is off or on
    bool m_isRTTOn;

    ///Flag to tell if we are in Desktop mode
    bool m_isDesktopMode;

    ///Flag to tell if we are using screen aligned normals
    bool m_screenAlignedNormals;

    ///Tell if we are on the master node
    bool m_isMasterNode;

    ///This containes a map to context specific data given a viewport
    ///This map should never be returned to the user as the map
    ///can have more viewports than active at a given time
    typedef std::map< vrj::ViewportPtr, GLTransformInfoPtr > GLTransformInfoMap;
    GLTransformInfoMap m_glTransformInfoMap;

    ///VRJuggler's head positional interface
    gadget::PositionInterface m_vrjHead;
    
    ///A reference to the device handler group node
    osg::ref_ptr< osg::Group > m_deviceHandlerGroup;
    
    ///The current GLTransformIno pointer
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;
    
    ///The center point used for rotation purposes with the kmb and wand
    gmtl::Point3d m_centerPoint;
};
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_SCENEMANAGER_H
