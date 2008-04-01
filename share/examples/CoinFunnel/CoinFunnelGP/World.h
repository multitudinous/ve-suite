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

#ifndef WORLD_H
#define WORLD_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class CADEntity;
    class PhysicsSimulator;
}
}
}

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Image;
    class TextureCubeMap;
}

// --- C/C++ Libraries --- //
#include <string>
#include <map>

namespace demo
{
class FunnelEntity;
class MarbleEntity;
class QuarterEntity;
class RailingEntity;
class SlideEntity;
class WaterEntity;

class World
{
public:
    World( ves::xplorer::scenegraph::DCS* pluginDCS,
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
           , osgAL::SoundManager* soundManager
#endif
           );

    ~World();

    void Initialize();
    void PreFrameUpdate();

    ves::xplorer::scenegraph::DCS* GetPluginDCS();

private:
    void CreateRoom( float width );

    std::map< std::string, osg::Image* > m_imageMap;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;

#ifdef VE_SOUND
    osgAL::SoundManager* m_soundManager;
#endif

    osg::ref_ptr< osg::TextureCubeMap > m_tcm;

    demo::FunnelEntity* m_funnelEntity;
    demo::MarbleEntity* m_marbleEntity;
    demo::QuarterEntity* m_quarterEntity;
    demo::RailingEntity* m_railingEntity;
    demo::SlideEntity* m_slideEntity;
    demo::WaterEntity* m_waterEntity;
};
}

#endif // end WORLD_H
