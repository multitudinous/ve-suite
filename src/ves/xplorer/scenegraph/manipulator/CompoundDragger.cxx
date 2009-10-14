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
#include <ves/xplorer/scenegraph/manipulator/CompoundDragger.h>
#include <ves/xplorer/scenegraph/manipulator/RotateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/Rotate.h>
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

#include <ves/xplorer/scenegraph/GLTransformInfo.h>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
CompoundDragger::CompoundDragger(
    const TransformationType::Enum& transformationType )
    :
    Dragger( transformationType )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CompoundDragger::CompoundDragger(
    const CompoundDragger& compoundDragger, const osg::CopyOp& copyop )
    :
    Dragger( compoundDragger, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CompoundDragger::~CompoundDragger()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CompoundDragger* CompoundDragger::AsCompoundDragger()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
RotateCompound* CompoundDragger::AsRotateCompound()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->ComboForm();
    }

    Dragger::ComboForm();
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::Connect( osg::Transform* activeAssociation )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->Connect( activeAssociation );
    }

    return Dragger::Connect( activeAssociation );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->DefaultForm();
    }

    Dragger::DefaultForm();
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::Disconnect()
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->Disconnect();
    }

    return Dragger::Disconnect();
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const CompoundDragger* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* CompoundDragger::className() const
{
    return "CompoundDragger";
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::Focus( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    ++npItr;

    Dragger* activeDragger( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i )->Focus( npItr );
        if( dragger )
        {
            activeDragger = dragger;
        }
    }

    --npItr;
    return activeDragger;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::Push(
    const osgUtil::LineSegmentIntersector& deviceInput,
    const osg::NodePath& np,
    osg::NodePath::iterator& npItr )
{
    osg::Node* node = *npItr;
    if( this == node )
    {
        //Get the active dragger
        ++npItr;
        Dragger* activeDragger( NULL );
        for( unsigned int i = 0; i < getNumChildren(); ++i )
        {
            Dragger* dragger = GetChild( i )->Push( deviceInput, np, npItr );
            if( dragger )
            {
                activeDragger = dragger;
            }
        }

        --npItr;
        return activeDragger;
    }

    if( m_comboForm )
    {
        Hide();
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        ++npItr;
        Dragger* activeDragger( NULL );
        for( unsigned int i = 0; i < getNumChildren(); ++i )
        {
            Dragger* dragger = GetChild( i )->Release( npItr );
            if( dragger )
            {
                activeDragger = dragger;
            }
        }

        --npItr;
        return activeDragger;
    }

    if( m_comboForm )
    {
        Show();
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::addChild( Dragger* child )
{
    child->SetRootDragger( this );

    return Dragger::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::GetChild( unsigned int i )
{
    return static_cast< Dragger* >( Dragger::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
const TransformationType::Enum& CompoundDragger::GetEnabledModes() const
{
    return m_enabledModes;
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::insertChild( unsigned int index, Dragger* child )
{
    child->SetRootDragger( this );

    return Dragger::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::replaceChild( Dragger* origChild, Dragger* newChild )
{
    newChild->SetRootDragger( this );

    return Dragger::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::setChild( unsigned int i, Dragger* node )
{
    node->SetRootDragger( this );

    return Dragger::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetAxisDirection(
    const AxisDirection::Enum& axisDirection )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetAxisDirection( axisDirection );
    }

    Dragger::SetAxisDirection( axisDirection );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetColor(
    Color::Enum colorTag, osg::Vec4& newColor, bool use )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetColor( colorTag, newColor, use );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetConstraintMap( ConstraintMap& constraintMap )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetConstraintMap( constraintMap );
    }

    Dragger::SetConstraintMap( constraintMap );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetCurrentGLTransformInfo(
    ves::xplorer::scenegraph::GLTransformInfoPtr currentGLTransformInfo )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetCurrentGLTransformInfo( currentGLTransformInfo );
    }

    Dragger::SetCurrentGLTransformInfo( currentGLTransformInfo );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetEnabledModes( TransformationType::Enum value )
{
    //We don't want to do this since this function is used to show/hide geometry
    /*
    if( m_enabledModes == value )
    {
        return;
    }
    */

    m_enabledModes = value;

    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i );
        if( dragger->GetTransformationType() & m_enabledModes )
        {
            dragger->Show();
        }
        else
        {
            dragger->Hide();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetHelpCircle( HelpCircle* const helpCircle )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        Dragger* dragger = GetChild( i );

        CompoundDragger* compoundDragger = dragger->AsCompoundDragger();
        if( compoundDragger )
        {
            compoundDragger->SetHelpCircle( helpCircle );
            RotateCompound* rotateCompound =
                compoundDragger->AsRotateCompound();
            if( rotateCompound )
            {
                rotateCompound->insertChild( 0, helpCircle );
            }

            continue;
        }

        Rotate* rotate = dragger->AsRotate();
        if( rotate )
        {
            rotate->SetHelpCircle( helpCircle );

            continue;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetRootDragger( Dragger* rootDragger )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetRootDragger( rootDragger );
    }

    Dragger::SetRootDragger( rootDragger );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetVectorSpace( const VectorSpace::Enum& vectorSpace )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetVectorSpace( vectorSpace );
    }

    Dragger::SetVectorSpace( vectorSpace );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::UseColor( Color::Enum colorTag )
{
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->UseColor( colorTag );
    }
}
////////////////////////////////////////////////////////////////////////////////
