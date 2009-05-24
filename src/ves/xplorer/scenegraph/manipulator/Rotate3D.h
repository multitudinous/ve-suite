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

#ifndef ROTATE_3D_H
#define ROTATE_3D_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/CompoundDragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class RotateAxis;

/*!\file Rotate3D.h
 * Rotate3D API
 */

/*!\class ves::xplorer::scenegraph::Rotate3D
 *
 */
class VE_SCENEGRAPH_EXPORTS Rotate3D : public CompoundDragger
{
public:
    ///
    Rotate3D();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Rotate3D(
        const Rotate3D& rotate3D,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, Rotate3D );

protected:
    ///
    virtual ~Rotate3D();

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    osg::ref_ptr< RotateAxis > m_xRotateAxis;
    
    ///
    osg::ref_ptr< RotateAxis > m_yRotateAxis;
    
    ///
    osg::ref_ptr< RotateAxis > m_zRotateAxis;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //ROTATE_3D_H
