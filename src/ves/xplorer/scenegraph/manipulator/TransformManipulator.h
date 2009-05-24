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

#ifndef TRANSFORM_MANIPULATOR_H
#define TRANSFORM_MANIPULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class Translate3D;
class Rotate3D;
class Scale3D;

/*!\file TransformManipulator.h
 * TransformManipulator API
 */

/*!\class ves::xplorer::scenegraph::TransformManipulator
 *
 */
class VE_SCENEGRAPH_EXPORTS TransformManipulator : public Manipulator
{
public:
    ///Constructor
    TransformManipulator();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    TransformManipulator(
        const TransformManipulator& transformManipulator,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, TransformManipulator );


protected:
    ///Destructor
    virtual ~TransformManipulator();

    ///
    virtual void SetupDefaultDraggers();

private:
    ///
    osg::ref_ptr< Translate3D > m_translateDragger;

    ///
    osg::ref_ptr< Rotate3D > m_rotateDragger;

    ///
    osg::ref_ptr< Scale3D > m_scaleDragger;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //TRANSFORM_MANIPULATOR_H
