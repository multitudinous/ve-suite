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
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>
#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/AutoTransform>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator()
    :
    osg::MatrixTransform(),
    m_enabledModes( TransformationType::NONE ),
    m_vectorSpace( VectorSpace::WORLD ),
    m_enabled( false ),
    m_autoTransform( new osg::AutoTransform() )
{
    //All manipulators should be based off unit axes
    float initialScale = 100.0;
    setMatrix( osg::Matrix::scale( initialScale, initialScale, initialScale ) );

    //Set manipulator to scale to the same size based off distance from the eye
    SetAutoScaleToScreen( true );
    //Set initial bound so AutoTransform is not culled by small feature culling
    osg::BoundingSphere bs( osg::Vec3f( 0.0, 0.0, 0.0 ), initialScale );
    m_autoTransform->setInitialBound( bs );
    m_autoTransform->addChild( this );
}
////////////////////////////////////////////////////////////////////////////////
Manipulator::Manipulator(
    const Manipulator& manipulator, const osg::CopyOp& copyop )
    :
    osg::MatrixTransform( manipulator, copyop ),
    m_enabledModes( manipulator.m_enabledModes ),
    m_vectorSpace( manipulator.m_vectorSpace ),
    m_enabled( manipulator.m_enabled ),
    m_autoTransform( manipulator.m_autoTransform.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Manipulator::~Manipulator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Manipulator::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const Manipulator* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* Manipulator::className() const
{
    return "Manipulator";
}
////////////////////////////////////////////////////////////////////////////////
const char* Manipulator::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
bool Manipulator::addChild( Dragger* child )
{
    return osg::MatrixTransform::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::ComboForm()
{
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->ComboForm();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::DefaultForm()
{
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->DefaultForm();
    }
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Manipulator::Focus( osg::NodePath::iterator& npItr )
{
    Dragger* activeDragger( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i )->Focus( npItr );
        if( dragger )
        {
            activeDragger = dragger;
        }
    }

    return activeDragger;
}
////////////////////////////////////////////////////////////////////////////////
const std::vector< osg::Transform* >& Manipulator::GetAssociatedTransforms() const
{
    return m_associatedTransforms;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Manipulator::GetChild( unsigned int i )
{
    return static_cast< Dragger* >( osg::MatrixTransform::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
const TransformationType::Enum Manipulator::GetEnabledModes() const
{
    return m_enabledModes;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum Manipulator::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Manipulator::Push(
    const osgUtil::LineSegmentIntersector& deviceInput,
    const osg::NodePath& np,
    osg::NodePath::iterator& npItr )
{
    Dragger* activeDragger( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i )->Push( deviceInput, np, npItr );
        if( dragger )
        {
            activeDragger = dragger;
        }
    }

    return activeDragger;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::PushBackAssociation(
    osg::Transform* transform, bool clearPreviousAssociations )
{
    if( clearPreviousAssociations )
    {
        m_associatedTransforms.clear();
    }

    m_associatedTransforms.push_back( transform );

    ManipulatorManager* manipulatorManager =
        vxs::SceneManager::instance()->GetManipulatorManager();
    if( manipulatorManager->IsEnabled() && !m_enabled )
    {
        TurnOn();
    }
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Manipulator::Release( osg::NodePath::iterator& npItr )
{
    Dragger* activeDragger( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i )->Release( npItr );
        if( dragger )
        {
            activeDragger = dragger;
        }
    }

    return activeDragger;
}
////////////////////////////////////////////////////////////////////////////////
bool Manipulator::insertChild( unsigned int index, Dragger* child )
{
    return osg::MatrixTransform::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
bool Manipulator::replaceChild( Dragger* origChild, Dragger* newChild )
{
    return osg::MatrixTransform::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool Manipulator::setChild( unsigned int i, Dragger* node )
{
    return osg::MatrixTransform::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::SetAutoScaleToScreen( bool autoScaleToScreen )
{
    m_autoTransform->setAutoScaleToScreen( autoScaleToScreen );
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::SetEnabledModes( TransformationType::Enum value )
{
    if( m_enabledModes == value )
    {
        return;
    }

    if( m_enabled )
    {
        //m_redoStack.clear();
    }

    //m_enabled = false;
    m_enabledModes = value;
    //m_activeMode = TransformationType::NONE;

    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i );
        if( dragger->GetTransformationType() & m_enabledModes )
        {
            dragger->TurnOn();
        }
        else
        {
            dragger->TurnOff();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::SetVectorSpace( VectorSpace::Enum value )
{
    if( m_vectorSpace == value )
    {
        return;
    }

    if( m_enabled )
    {
        //m_redoStack.Clear();
    }

    //m_enabled = false;
    m_vectorSpace = value;
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::TurnOff()
{
    m_enabled = false;

    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Manipulator::TurnOn()
{
    m_enabled = true;

    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
