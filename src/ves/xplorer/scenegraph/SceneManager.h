/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef SCENE_MANAGER_H
#define SCENE_MANAGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>
#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/ClearNode>
#include <osg/FrameStamp>

// --- VR Juggler Includes --- //
#include <vrj/vrjParam.h>

#if __VJ_version >= 2003000
#include <vrj/Display/ViewportPtr.h>
#else
namespace vrj
{
    class Viewport;
}
#endif

#include <vpr/Util/Singleton.h>

#include <gmtl/Matrix.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class Group;
class Switch;
class CADEntity;
#ifdef VE_SOUND
class Sound;
#endif
class CharacterController;

namespace manipulator
{
class ManipulatorManager;
}

/*!\file SceneManager.h
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
    DCS* const GetActiveNavSwitchNode() const;

    ///Return the active switch node of the scenegraph
    osg::Group* const GetActiveSwitchNode() const;

    ///
    ///\return
    CharacterController* const GetCharacterController() const;

    ///Get the framestamp used by SceneView
    ///\return The osg::FrameStamp for the osg::SceneView
    osg::FrameStamp* const GetFrameStamp() const;

    ///Get the inverted world DCS matrix
    ///\return The inverted matrix
    const gmtl::Matrix44d& GetInvertedWorldDCS() const;

    ///Return the manipulator root node of the scenegraph
    manipulator::ManipulatorManager* const GetManipulatorManager() const;

    ///
    ///\return
    GLTransformInfoPtr const GetGLTransformInfo(
#if __VJ_version >= 2003000
        vrj::ViewportPtr const viewport );
#else
        vrj::Viewport* const viewport );
#endif

    ///Return the model root node of the scenegraph
    ///\return
    osg::Group* const GetModelRoot() const;

    ///Return the network DCS of the scenegraph
    osg::Group* const GetNetworkDCS() const;
    
    ///Return the root node of the scenegraph
    osg::Group* const GetRootNode() const;

    ///Return the world DCS of the scenegraph
    ///\return The world DCS
    DCS* const GetWorldDCS() const;

    ///???
    ///\param param
    void Initialize( std::string& param );

    ///Initialize the scene
    void InitScene();

    ///PreFrameUpdate call to sync DCS information across cluster
    void PreFrameUpdate();

    ///
    void SceneManager::PushBackGLTransformInfo(
#if __VJ_version >= 2003000
        vrj::ViewportPtr viewport,
#else
        vrj::Viewport* viewport,
#endif
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
    osg::ref_ptr< DCS > worldDCS;

    ///Node to hold a network view of the system under investigation
    //osg::ref_ptr< DCS > networkDCS;
    osg::ref_ptr< osg::Group > mNetworkDCS;

    ///Inverteded world dcs values
    gmtl::Matrix44d mInvertedWorldDCS;

#ifdef VE_SOUND
    ///
    Sound* m_sound;
#endif

    ///The logo
    CADEntity* m_blueArrow;
    
    ///
    CADEntity* m_greyArrow;
    
    ///
    CADEntity* m_orangeArrow;
    
    ///
    CADEntity* m_veText;
    
    ///
    CADEntity* m_suiteText;

    ///Clear node to control the background color
    osg::ref_ptr< osg::ClearNode > m_clrNode;

    ///FrameStamp to control sequence nodes and other osg animations
    osg::ref_ptr< osg::FrameStamp > mFrameStamp;

    ///
    CharacterController* mCharacterController;
    
    ///Flag to tell if RTT is off or on
    bool m_isRTTOn;

#if __VJ_version >= 2003000
    typedef std::map< vrj::ViewportPtr, GLTransformInfoPtr > GLTransformInfoMap;
    GLTransformInfoMap m_glTransformInfoMap;
#else
    typedef std::map< vrj::Viewport*, GLTransformInfoPtr > GLTransformInfoMap;
    GLTransformInfoMap m_glTransformInfoMap;
#endif

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //SCENE_MANAGER_H
