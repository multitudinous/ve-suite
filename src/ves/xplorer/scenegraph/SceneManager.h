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
#ifndef SCENE_MANAGER_H
#define SCENE_MANAGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>
#ifdef VE_SOUND
#include <ves/xplorer/scenegraph/Sound.h>
#endif

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/ClearNode>
#endif

#include <osgOQ/OcclusionQueryRoot.h>
#include <osgOQ/OcclusionQueryContext.h>

#ifdef VE_SOUND
#include <osgAL/SoundRoot>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>
#endif

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

    ///Acts as the destructor
    //void CleanUp();

    ///Initialize the scene
    void InitScene();

    ///Return the root node of the scenegraph
    ves::xplorer::scenegraph::Group* GetRootNode();

    ///Return the world DCS of the scenegraph
    ves::xplorer::scenegraph::DCS* GetWorldDCS();

    ///Return the network DCS of the scenegraph
    ves::xplorer::scenegraph::DCS* GetNetworkDCS();

    ///Return the active switch node of the scenegraph
    ves::xplorer::scenegraph::DCS* GetActiveSwitchNode();

    ///Return the active switch node of the scenegraph
    osgOQ::OcclusionQueryContext* GetOcclusionQueryContext();

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

    ///Reset the osgOQC node accessor
    ///Should be used when new ves files are loaded
    void ResetOcclusionQueryContext();

private:
    //Required so that vpr::Singleton can instantiate this class
    //Friend class vpr::Singleton< SceneManager >;
    //SceneManager(const SceneManager& o){;}
    //SceneManager& operator=(const SceneManager& o){;}

    ///Base Constructor
    SceneManager();

    ///Destructor
    ///Never gets called, don't implement
    ~SceneManager();

    vprSingletonHeader( SceneManager );

    std::string _param;///<
    osg::ref_ptr< ves::xplorer::scenegraph::Group > rootNode;///<The root node of our scenegraph
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _logoNode;///<The node which contains our logo
    osg::ref_ptr< ves::xplorer::scenegraph::Switch > _logoSwitch;///<Node to switch between the logo and the worldDCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > worldDCS;///<Node to control navigation
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > networkDCS;///<Node to hold a network view of the system under investigation

    #ifdef VE_SOUND
    //Create ONE (only one, otherwise the transformation
    //of the listener and update for SoundManager will be
    //called several times, which is not catastrophic, but unnecessary) 
    //SoundRoot that will make sure the listener is updated and
    //to keep the internal state of the SoundManager updated
    //This could also be done manually, this is just a handy way of doing it.
    osg::ref_ptr< osgAL::SoundRoot > m_soundRoot;
    //osg::ref_ptr< ves::xplorer::scenegraph::Sound > m_sound;
    #endif

    //The logo
    ves::xplorer::scenegraph::CADEntity* m_blueArrow;
    ves::xplorer::scenegraph::CADEntity* m_greyArrow;
    ves::xplorer::scenegraph::CADEntity* m_orangeArrow;
    ves::xplorer::scenegraph::CADEntity* m_veText;
    ves::xplorer::scenegraph::CADEntity* m_suiteText;

    ///Clear node to control the background color
    osg::ref_ptr< osg::ClearNode > m_clrNode;
    osg::ref_ptr< osgOQ::OcclusionQueryContext > m_oqc;

    ///Map to store state information about each dcs
    std::map< int, gmtl::Matrix44d > m_matrixStore;

protected:
    ///Create the model for the logo
#ifdef _OSG
    void _createLogo();
#endif

};
}
}
}

#endif // SCENE_MANAGER_H