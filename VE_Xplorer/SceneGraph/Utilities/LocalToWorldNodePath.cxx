/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-04-10 19:18:51 -0500 (Tue, 10 Apr 2007) $
 * Version:       $Rev: 7298 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: LocalToWorldNodePath.cxx 7298 2007-04-11 00:18:51Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/Utilities/LocalToWorldNodePath.h"

// --- OSG Stuff --- //
#include <osg/PositionAttitudeTransform>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph::Utilities;

////////////////////////////////////////////////////////////////////////////////
LocalToWorldNodePath::LocalToWorldNodePath( osg::Node* node )
:
NodeVisitor( TRAVERSE_PARENTS )
{
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
LocalToWorldNodePath::~LocalToWorldNodePath()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////    
void LocalToWorldNodePath::apply( osg::PositionAttitudeTransform& pat )
{
    if( pat.getName() == "World DCS" )
    {
        m_nodePath = _nodePath;

        return;
    }

    osg::NodeVisitor::apply( (osg::Transform&)pat );
}
////////////////////////////////////////////////////////////////////////////////
osg::NodePath& LocalToWorldNodePath::GetNodePath()
{
    return m_nodePath;
}
////////////////////////////////////////////////////////////////////////////////
