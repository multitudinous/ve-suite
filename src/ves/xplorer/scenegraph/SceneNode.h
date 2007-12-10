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
#ifndef SCENENODE_H
#define SCENENODE_H

/*!\file SceneNode.h
*/

/*!\class ves::xplorer::scenegraph::SceneNode
*Base class for all scene graph nodes
*Do not understand why we need this class
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Stuff --- //
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Technique;
}
}
}

// --- OSG Includes --- //
#ifdef _OSG
namespace osg
{
class NodeVisitor;
}
#elif _OPENSG
#endif

// --- C/C++ Libraries --- //
#include <string>
#include <map>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS SceneNode
{
public:
    ///Base Constructor
    SceneNode();

    ///Destructor
    virtual ~SceneNode();

// -------------------------------------------------- //
// --- This stuff is used for multipass rendering --- //
// -------------------------------------------------- //
public:
    virtual void InheritedTraverse( osg::NodeVisitor& nv ) = 0;

    void DirtyTechniques();

    void AddTechnique( std::string name, ves::xplorer::scenegraph::Technique* technique );

    void SetTechnique( std::string name );

    ves::xplorer::scenegraph::Technique* GetTechnique( std::string name );

    ves::xplorer::scenegraph::Technique* GetActiveTechnique();

protected:
    std::string m_activeTechnique;

    std::map< std::string, ves::xplorer::scenegraph::Technique* > m_techniques;

};
}
}
}

#endif //SCENENODE_H
