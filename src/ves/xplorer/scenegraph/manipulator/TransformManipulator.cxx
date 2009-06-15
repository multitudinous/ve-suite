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
#include <ves/xplorer/scenegraph/manipulator/TranslateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/RotateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/ScaleCompound.h>

// --- OSG Includes --- //


using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TransformManipulator::TransformManipulator()
    :
    Manipulator(),
    m_translateCompound( NULL ),
    m_rotateCompound( NULL ),
    m_scaleCompound( NULL )
{
    SetupDefaultDraggers();
}
////////////////////////////////////////////////////////////////////////////////
TransformManipulator::TransformManipulator(
    const TransformManipulator& transformManipulator,
    const osg::CopyOp& copyop )
    :
    Manipulator( transformManipulator, copyop ),
    m_translateCompound( transformManipulator.m_translateCompound.get() ),
    m_rotateCompound( transformManipulator.m_rotateCompound.get() ),
    m_scaleCompound( transformManipulator.m_scaleCompound.get() )
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
    osg::ref_ptr< manipulator::TranslateCompound > translateCompound =
        new manipulator::TranslateCompound();
    addChild( translateCompound.get() );

    osg::ref_ptr< manipulator::RotateCompound > rotateCompound =
        new manipulator::RotateCompound();
    addChild( rotateCompound.get() );

    osg::ref_ptr< manipulator::ScaleCompound > scaleCompound =
        new manipulator::ScaleCompound();
    addChild( scaleCompound.get() );

    SetEnabledModes( TransformationType::TRANSLATE_COMPOUND );
}
////////////////////////////////////////////////////////////////////////////////
