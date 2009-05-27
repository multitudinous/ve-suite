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
 * Date modified: $Date: 2009-05-24 12:01:57 -0600 (Sun, 24 May 2009) $
 * Version:       $Rev: 12717 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: RotateTwist.h 12717 2009-05-24 18:01:57Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef ROTATE_TWIST_H
#define ROTATE_TWIST_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file RotateTwist.h
 * RotateTwist API
 */

/*!\class ves::xplorer::scenegraph::RotateTwist
 *
 */
class VE_SCENEGRAPH_EXPORTS RotateTwist : public Dragger
{
public:
    ///
    RotateTwist();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    RotateTwist(
        const RotateTwist& rotateTwist,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, RotateTwist );

protected:
    ///
    virtual ~RotateTwist();

    ///
    virtual void SetupDefaultGeometry();

private:


};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //ROTATE_TWIST_H
