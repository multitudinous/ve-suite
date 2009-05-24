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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>
#include <ves/xplorer/scenegraph/manipulator/Translate3D.h>
#include <ves/xplorer/scenegraph/manipulator/Rotate3D.h>
#include <ves/xplorer/scenegraph/manipulator/Scale3D.h>

// --- OSG Includes --- //


using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TransformManipulator::TransformManipulator()
    :
    Manipulator(),
    m_translateDragger( NULL ),
    m_rotateDragger( NULL ),
    m_scaleDragger( NULL )
{
    SetupDefaultDraggers();
}
////////////////////////////////////////////////////////////////////////////////
TransformManipulator::TransformManipulator(
    const TransformManipulator& transformManipulator,
    const osg::CopyOp& copyop )
    :
    Manipulator( transformManipulator, copyop ),
    m_translateDragger( transformManipulator.m_translateDragger.get() ),
    m_rotateDragger( transformManipulator.m_rotateDragger.get() ),
    m_scaleDragger( transformManipulator.m_scaleDragger.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TransformManipulator::~TransformManipulator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TransformManipulator::SetupDefaultDraggers()
{
    osg::ref_ptr< manipulator::Translate3D > translate3D =
        new manipulator::Translate3D();
    addChild( translate3D.get() );

    osg::ref_ptr< manipulator::Rotate3D > rotate3D =
        new manipulator::Rotate3D();
    addChild( rotate3D.get() );

    osg::ref_ptr< manipulator::Scale3D > scale3D =
        new manipulator::Scale3D();
    addChild( scale3D.get() );
}
////////////////////////////////////////////////////////////////////////////////
