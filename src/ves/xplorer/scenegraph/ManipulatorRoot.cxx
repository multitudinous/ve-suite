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
 * Date modified: $Date: 2009-05-13 15:17:12 -0600 (Wed, 13 May 2009) $
 * Version:       $Rev: 12684 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: ManipulatorRoot.cxx 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ManipulatorRoot.h>

#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot()
    :
    osg::Group()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot(
    const ManipulatorRoot& manipulatorRoot, const osg::CopyOp& copyop )
    :
    osg::Group( manipulatorRoot, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::~ManipulatorRoot()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::addChild( manipulator::Manipulator* child )
{
    return Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::insertChild(
    unsigned int index, manipulator::Manipulator* child )
{
    return Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::replaceChild(
    manipulator::Manipulator* origChild,
    manipulator::Manipulator* newChild )
{
    return Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::setChild( unsigned int i, manipulator::Manipulator* node )
{
    return Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
