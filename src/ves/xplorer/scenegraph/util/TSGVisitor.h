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

#ifndef TSG_VISITOR_H
#define TSG_VISITOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>

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
    TSGVisitor();

    ///
    //META_NodeVisitor( ves::xplorer::scenegraph::util, TSGVisitor );

    ///Destructor
    virtual ~TSGVisitor();

    ///
    virtual void apply( osg::Geode& geode );

protected:

private:
    ///
    void GenerateTangentSpaceData(
        unsigned int tangentIndex = 6,
        unsigned int binormalIndex = 7,
        unsigned int normalIndex = 15 );


};
} //end util
} //end scenegraph
} //end xplorer
} //end ves

#endif //TSG_VISITOR_H
