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

#ifndef VES_XPLORER_SCENEGRAPH_UTIL_TSG_VISITOR_H
#define VES_XPLORER_SCENEGRAPH_UTIL_TSG_VISITOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>

namespace osg
{
class Node;
class Geode;
class Geometry;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{

/*!\file TSGVisitor.h
 *
 */

/*!\class ves::xplorer::scenegraph::util::TSGVisitor
 * Class to create tangent space vertex attribute information
 */

/*!\namespace ves::xplorer::scenegraph::util
 *
 */
class VE_SCENEGRAPH_UTILS_EXPORTS TSGVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    TSGVisitor(
        osg::Node* const node,
        unsigned int normalMapTexUnit,
        unsigned int tangentIndex = 6,
        unsigned int binormalIndex = 7,
        unsigned int normalIndex = 15 );

    ///Destructor
    virtual ~TSGVisitor();

    ///
    META_NodeVisitor( ves::xplorer::scenegraph::util, TSGVisitor );

    ///
    virtual void apply( osg::Geode& geode );

protected:

private:
    ///
    void PrepareGeometry( osg::Geometry* const geometry );

    ///
    unsigned int m_normalMapTexUnit;

    ///
    unsigned int m_tangentIndex;

    ///
    unsigned int m_binormalIndex;

    ///
    unsigned int m_normalIndex;

};

} //end util
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_UTIL_TSG_VISITOR_H
