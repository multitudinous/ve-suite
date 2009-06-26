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

#ifndef GRINDER_ENTITY_H
#define GRINDER_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

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

// --- OSG Includes --- //
namespace osg
{
class Geometry;
class Image;
}

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{
class GrinderEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    GrinderEntity( std::string geomFile,
                   osg::Group* parentNode,
                   ves::xplorer::scenegraph::DCS* pluginDCS,
                   ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
                   ves::xplorer::scenegraph::ResourceManager* resourceManager );

    virtual ~GrinderEntity();

    void SetNameAndDescriptions( const std::string& name );

protected:

private:
    osg::Geometry* CreateTexturedQuadGeometry(
        const osg::Vec3& pos, float width, float height, osg::Image* image,
        bool useTextureRectangle, bool xzPlane, bool optionFlip );

    void Initialize();

    ves::xplorer::scenegraph::ResourceManager* m_resourceManager;

    osg::ref_ptr< osg::Group > mParentNode;

};
} //end cpt

#endif //end GRINDER_ENTITY_H
