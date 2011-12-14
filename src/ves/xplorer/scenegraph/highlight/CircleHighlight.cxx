/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// --- VES Includes --- //
#include <ves/xplorer/scenegraph/highlight/CircleHighlight.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>

// --- STL Includes --- //
#include <iostream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace highlight
{

////////////////////////////////////////////////////////////////////////////////
CircleHighlight::CircleHighlight()
    :
    osg::Group(),
    m_dcs( NULL )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CircleHighlight::CircleHighlight(
    const CircleHighlight& circleHighlight,
    const osg::CopyOp& copyop )
    :
    osg::Group( circleHighlight, copyop ),
    m_dcs( circleHighlight.m_dcs.get() )
{
    if( &circleHighlight != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CircleHighlight::~CircleHighlight()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CircleHighlight::Initialize()
{
    //SetNamesAndDescriptions();

    //Update();
}
////////////////////////////////////////////////////////////////////////////////
DCS& CircleHighlight::GetDCS()
{
    return *m_dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
void CircleHighlight::SetNamesAndDescriptions()
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );

    m_dcs->setDescriptions( descriptorsList );
    m_dcs->setName( "CircleHighlightDCS" );
}
////////////////////////////////////////////////////////////////////////////////
void CircleHighlight::Update()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////

} //end highlight
} //end scenegraph
} //end xplorer
} //end ves
