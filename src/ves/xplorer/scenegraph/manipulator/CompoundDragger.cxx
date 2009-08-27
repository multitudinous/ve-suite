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
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //


using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
CompoundDragger::CompoundDragger(
    const AxesFlag::Enum& axesFlag,
    const TransformationType::Enum& transformationType,
    Manipulator* const parentManipulator )
    :
    Dragger( axesFlag, transformationType, parentManipulator )
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
    osg::Node* node = *(++npItr);
    if( this == node )
    {
        ;
    }

    Dragger* activeDragger( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
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
    //Get the active dragger
    osg::Node* node = *(++npItr);
    if( this == node )
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

        --npItr;
        return activeDragger;
    }

    if( m_comboForm )
    {
        TurnOff();
    }

    --npItr;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *(++npItr);
    if( this == node )
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

        --npItr;
        return activeDragger;
    }

    if( m_comboForm )
    {
        TurnOn();
    }

    --npItr;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::addChild( Dragger* child )
{
    return Dragger::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
Dragger* CompoundDragger::GetChild( unsigned int i )
{
    return static_cast< Dragger* >( Dragger::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::insertChild( unsigned int index, Dragger* child )
{
    return Dragger::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::replaceChild( Dragger* origChild, Dragger* newChild )
{
    return Dragger::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool CompoundDragger::setChild( unsigned int i, Dragger* node )
{
    return Dragger::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::SetColor(
    ColorTag::Enum colorTag, osg::Vec4& newColor, bool use )
{
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->SetColor( colorTag, newColor, use );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CompoundDragger::UseColor( ColorTag::Enum colorTag )
{
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        GetChild( i )->UseColor( colorTag );
    }
}
////////////////////////////////////////////////////////////////////////////////
