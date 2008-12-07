/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/ClearNode>
#include <osg/FrameStamp>

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>

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
    ///???
    ///\param param
    void Initialize( std::string param );

    ///Initialize the scene
    void InitScene();
    
    ///Set the root node from the render to texture class
    void SetRootNode( osg::Group* rootNode );
    
    ///Return the root node of the scenegraph
    osg::Group* GetRootNode();

    ///Return the world DCS of the scenegraph
    ves::xplorer::scenegraph::DCS* GetWorldDCS();

    ///Return the network DCS of the scenegraph
    ves::xplorer::scenegraph::DCS* GetNetworkDCS();

    ///Return the active switch node of the scenegraph
    ves::xplorer::scenegraph::DCS* GetActiveSwitchNode();

    ///Return the model root node of the scenegraph
    ves::xplorer::scenegraph::DCS* GetModelRoot();

    ///Return the active switch node of the scenegraph
    //osgOQ::OcclusionQueryContext* GetOcclusionQueryContext();

    ///Set the node on the switch node that is active
    ///\param activeNode The node to activate
    void SetActiveSwitchNode( int activeNode );

    ///Switch the logo on and off
    ///\param trueFalse Turn the logo on and off
    void ViewLogo( bool trueFalse );

    ///PreFrameUpdate call to sync DCS information across cluster
    void PreFrameUpdate();

    ///Set the background color
    ///\param color the color to set the background color
    void SetBackgroundColor( std::vector< double > color );

    ///Shutdown the sound manager and other extra engines used in the world
    void Shutdown();
    
    ///Set the framestamp to make it accessible to other areas of ves
    ///This can be used when traditional access through osg does not work
    ///for example when plugins need to set the SimulationTime
    void SetFrameStamp( osg::FrameStamp* frameStamp );

    ///Get the framestamp used by SceneView
    ///\return The osg::FrameStamp for the osg::SceneView
    osg::FrameStamp* GetFrameStamp();

private:
    //Required so that vpr::Singleton can instantiate this class
    //Friend class vpr::Singleton< SceneManager >;
    //SceneManager(const SceneManager& o){;}
    //SceneManager& operator=(const SceneManager& o){;}

    ///Base Constructor
    SceneManager();

    ///Destructor
    ~SceneManager();

    vprSingletonHeader( SceneManager );

    ///The root node of our scenegraph
    osg::ref_ptr< osg::Group > mRootNode;
    ///The root model node of our scenegraph
    //osg::ref_ptr< osg::Group > mModelRoot;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mModelRoot;
    ///The node which contains our logo
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _logoNode;
    ///Node to switch between the logo and the worldDCS
    osg::ref_ptr< ves::xplorer::scenegraph::Switch > _logoSwitch;
    ///Node to control navigation
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > worldDCS;
    ///Node to hold a network view of the system under investigation
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > networkDCS;
#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* m_sound;
#endif

    //The logo
    ves::xplorer::scenegraph::CADEntity* m_blueArrow;
    ves::xplorer::scenegraph::CADEntity* m_greyArrow;
    ves::xplorer::scenegraph::CADEntity* m_orangeArrow;
    ves::xplorer::scenegraph::CADEntity* m_veText;
    ves::xplorer::scenegraph::CADEntity* m_suiteText;

    ///Clear node to control the background color
    osg::ref_ptr< osg::ClearNode > m_clrNode;
    ///FrameStamp to control sequence nodes and other osg animations
    osg::ref_ptr< osg::FrameStamp > mFrameStamp;

protected:
    ///Create the model for the logo
    void _createLogo();
};
}
}
}

#endif // SCENE_MANAGER_H
