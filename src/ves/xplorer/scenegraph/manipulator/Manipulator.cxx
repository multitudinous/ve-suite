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
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>
#include <ves/xplorer/scenegraph/manipulator/Translate3D.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/AutoTransform>
#include <osg/MatrixTransform>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator()
    :
    osg::Group(),
    m_autoTransform( new osg::AutoTransform() ),
    m_matrixTransform( new osg::MatrixTransform() )
{
    //vxs::SceneManager::instance()->GetManipulatorRoot()->addChild( this );

    m_autoTransform->setAutoScaleToScreen( true );
    m_autoTransform->setCullingActive( false );
    addChild( m_autoTransform.get() );

    m_matrixTransform->setMatrix(
        osg::Matrix::translate( 2.0, 0.0, 0.0 ) *
        osg::Matrix::scale( 100.0, 100.0, 100.0 ) );
    m_autoTransform->addChild( m_matrixTransform.get() );

    CreateDraggers();
}
////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator(
    const Manipulator& manipulator, const osg::CopyOp& copyop )
    :
    osg::Group( manipulator, copyop ),
    m_autoTransform( manipulator.m_autoTransform.get() ),
    m_matrixTransform( manipulator.m_matrixTransform.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Manipulator::~Manipulator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::CreateDraggers()
{
    osg::ref_ptr< Translate3D > translate3D = new Translate3D();
    m_matrixTransform->addChild( translate3D.get() );
/*
    std::multimap< AxisFlags::Enum, osg::ref_ptr< Dragger > > translateAxisMap;
    //Insert the individual axis
    translateAxisMap.insert(
        std::make_pair( AxisFlags::X, translateAxisX.get() ) );
    translateAxisMap.insert(
        std::make_pair( AxisFlags::Y, translateAxisY.get() ) );
    translateAxisMap.insert(
        std::make_pair( AxisFlags::Z, translateAxisZ.get() ) );

    //
    translateAxisMap.insert(
        std::make_pair( AxisFlags::XYZ, translateAxisX.get() ) );
    translateAxisMap.insert(
        std::make_pair( AxisFlags::XYZ, translateAxisY.get() ) );
    translateAxisMap.insert(
        std::make_pair( AxisFlags::XYZ, translateAxisZ.get() ) );

    m_draggers[ TransformationMode::TranslateAxis ] = translateAxisMap;
*/
}
////////////////////////////////////////////////////////////////////////////////
const TransformationMode::Enum Manipulator::GetActiveMode() const
{
    return m_activeMode;
}
////////////////////////////////////////////////////////////////////////////////
const TransformationMode::Enum Manipulator::GetEnabledModes() const
{
    return m_enabledModes;
}
////////////////////////////////////////////////////////////////////////////////
const AxisFlags::Enum Manipulator::GetSelectedAxes() const
{
    return m_selectedAxes;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum Manipulator::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::SetEnabledModes( TransformationMode::Enum value )
{
    if( m_enabledModes == value )
    {
        return;
    }

    if( m_manipulating )
    {
        //mRedoStack.clear();
    }

    m_manipulating = false;
    m_enabledModes = value;
    m_activeMode = TransformationMode::None;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::SetVectorSpace( VectorSpace::Enum value )
{
    if( m_manipulating )
    {
        //mRedoStack.Clear();
    }

    m_manipulating = false;
    m_vectorSpace = value;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::TurnOn()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::TurnOff()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
