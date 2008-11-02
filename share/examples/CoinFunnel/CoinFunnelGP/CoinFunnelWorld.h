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

#ifndef COIN_FUNNEL_WORLD_H
#define COIN_FUNNEL_WORLD_H

// --- My Includes --- //
#include "CoinFunnelWorldPtr.h"

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
    class ResourceManager;
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

namespace funnel
{
class FunnelEntity;
class MarbleEntity;
class RailingEntity;
class SlideEntity;
class WaterEntity;

class CoinFunnelWorld
{
public:
    CoinFunnelWorld(
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
        ves::xplorer::scenegraph::ResourceManager* resourceManager
#ifdef VE_SOUND
        ,
        osgAL::SoundManager* soundManager
#endif
        );

    ~CoinFunnelWorld();

    void PreFrameUpdate();

protected:

private:
    void Initialize();

    void CreateRoom( float width );

    ves::xplorer::scenegraph::PhysicsSimulator* mPhysicsSimulator;
    ves::xplorer::scenegraph::ResourceManager* mResourceManager;

#ifdef VE_SOUND
    osgAL::SoundManager* mSoundManager;
#endif

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    funnel::FunnelEntity* mFunnelEntity;
    funnel::MarbleEntity* mMarbleEntity;
    funnel::RailingEntity* mRailingEntity;
    funnel::SlideEntity* mSlideEntity;
    funnel::WaterEntity* mWaterEntity;

};
} //end funnel

#endif //COIN_FUNNEL_WORLD_H
