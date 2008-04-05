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

#ifndef MARBLE_ENTITY_H
#define MARBLE_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

#ifdef VE_SOUND
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class Sound;
}
}
}
#endif

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

// --- C/C++ Libraries --- //
#include <string>

namespace funnel
{
class MarbleEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    MarbleEntity( std::string geomFile,
                  ves::xplorer::scenegraph::DCS* pluginDCS,
                  ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
                  ,
                  osgAL::SoundManager* soundManager
#endif
                  );

    virtual ~MarbleEntity();

    void SetNameAndDescriptions( const std::string& name );

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* GetMarbleOnWoodSound();
    ves::xplorer::scenegraph::Sound* GetMarbleOnMetalSound();
    ves::xplorer::scenegraph::Sound* GetMarbleOnMarbleSound();
#endif

protected:

private:
    void Initialize();

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mMarbleOnWood;
    ves::xplorer::scenegraph::Sound* mMarbleOnMetal;
    ves::xplorer::scenegraph::Sound* mMarbleOnMarble;
#endif
};
} //end funnel

#endif // end MARBLE_ENTITY_H
