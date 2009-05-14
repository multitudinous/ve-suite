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
#include <ves/xplorer/scenegraph/manipulator/TranslateAxis.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator()
    :
    osg::AutoTransform(),
    m_matrixTransform( new osg::MatrixTransform() )
{
    setAutoScaleToScreen( true );
    setCullingActive( false );
    addChild( m_matrixTransform.get() );
    //vxs::SceneManager::instance()->GetManipulatorRoot()->addChild( this );

    //Set initial size for this manipulator
    m_matrixTransform->setMatrix(
        osg::Matrix::scale( osg::Vec3d( 100.0, 100.0, 100.0 ) ) );

    CreateDraggers();
}
////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator(
    const Manipulator& manipulator, const osg::CopyOp& copyop )
    :
    osg::AutoTransform( manipulator, copyop ),
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
    //Create translate x-axis dragger
    osg::ref_ptr< TranslateAxis > translateAxisX = new TranslateAxis();
    translateAxisX->SetColor(
        Dragger::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );
    m_matrixTransform->addChild( translateAxisX.get() );

    //Create translate y-axis dragger
    osg::ref_ptr< TranslateAxis > translateAxisY = new TranslateAxis();
    translateAxisY->SetColor(
        Dragger::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );
    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        translateAxisY->setMatrix( osg::Matrix( rotation ) );
    }
    m_matrixTransform->addChild( translateAxisY.get() );

    //Create translate z-axis dragger
    osg::ref_ptr< TranslateAxis > translateAxisZ = new TranslateAxis();
    translateAxisZ->SetColor(
        Dragger::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );
    //Rotate z-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 0.0, 1.0 ) );
        translateAxisZ->setMatrix( osg::Matrix( rotation ) );
    }
    m_matrixTransform->addChild( translateAxisZ.get() );

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
