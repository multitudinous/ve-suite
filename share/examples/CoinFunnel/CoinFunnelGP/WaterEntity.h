/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef WATER_ENTITY_H
#define WATER_ENTITY_H

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

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class ResourceManager;
}
}
}

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAudio
{
    class SoundManager;
}
#endif

// --- C/C++ Libraries --- //
#include <string>

namespace funnel
{
class WaterEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    WaterEntity( std::string geomFile,
                ves::xplorer::scenegraph::DCS* pluginDCS,
                ves::xplorer::scenegraph::ResourceManager* resourceManager
#ifdef VE_SOUND
                 ,
                 osgAudio::SoundManager* soundManager
#endif
                );

    virtual ~WaterEntity();

    void SetNameAndDescriptions( const std::string& name );

protected:

private:
    void Initialize();

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mWaterSound;
#endif
    ves::xplorer::scenegraph::ResourceManager* mResourceManager;
};
}

#endif // end WATER_ENTITY_H
