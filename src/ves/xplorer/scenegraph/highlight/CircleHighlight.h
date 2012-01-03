/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef VES_XPLORER_SCENEGRAPH_HIGHLIGHT_CIRCLEHIGHLIGHT_H
#define VES_XPLORER_SCENEGRAPH_HIGHLIGHT_CIRCLEHIGHLIGHT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Group>

namespace osg
{
class Geode;
class Geometry;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;

namespace highlight
{

/*!\file CircleHighlight.h
 *
 */

/*!\class CircleHighlight
 * Class for
 */

/*!\namespace ves::xplorer::scenegraph::highlight
 *
 */
class VE_SCENEGRAPH_EXPORTS CircleHighlight : public osg::Group
{
public:
    ///Constructor
    CircleHighlight();

    ///
    CircleHighlight(
        const CircleHighlight& circleHighlight,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    //META_Node( ves::xplorer::scenegraph::highlight, CircleHighlight );

    ///
    DCS& GetDCS();

    ///
    void Update();

protected:
    ///Destructor
    virtual ~CircleHighlight();

private:
    ///
    void Initialize();

    ///Temporary function to allow selection of highlights
    void SetNamesAndDescriptions();

    ///
    osg::ref_ptr< DCS > m_dcs;

};
} //end highlight
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_HIGHLIGHT_CIRCLEHIGHLIGHT_H
