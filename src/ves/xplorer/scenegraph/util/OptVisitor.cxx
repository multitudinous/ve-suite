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
#include "OptVisitor.h"
#include <osg/Geode>
#include <osg/Geometry>

#include <osg/io_utils>
#include <iostream>
#include <set>


OptVisitor::OptVisitor()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    changeDLtoVBO_( false ),
    changeDynamicToStatic_( false ),
    changeDAtoDEUI_( false ),
    triangles_( 0 ),
    triFans_( 0 ),
    triStrips_( 0 ),
    newDEUIs_( 0 )
{
}
OptVisitor::~OptVisitor()
{
}

void
OptVisitor::apply( osg::Node& node )
{
    if( changeDynamicToStatic_ )
        node.setDataVariance( osg::Object::STATIC );
    traverse( node );
}

void
OptVisitor::apply( osg::Geode& geode )
{
    for(unsigned int i=0;i<geode.getNumDrawables();++i)
    {
        osg::Drawable* draw = geode.getDrawable(i);
        if( changeDLtoVBO_ )
        {
            draw->setUseDisplayList( false );
            draw->setUseVertexBufferObjects( true );
        }
        if( changeDynamicToStatic_ )
        {
            draw->setDataVariance( osg::Object::STATIC );
        }

        osg::Geometry* geom = dynamic_cast< osg::Geometry* >( draw );
        if( ( geom != NULL ) && changeDAtoDEUI_ )
        {
            osg::ref_ptr< osg::DrawElementsUInt > deui = new osg::DrawElementsUInt( GL_TRIANGLES );

            unsigned int numPS( geom->getNumPrimitiveSets() );
            while( numPS > 0 )
            {
                numPS--;
                osg::PrimitiveSet* ps( geom->getPrimitiveSet( numPS ) );
                if( ps->getType() == osg::PrimitiveSet::DrawArraysPrimitiveType )
                {
                    osg::DrawArrays* da = dynamic_cast< osg::DrawArrays* >( ps );
                    if( ps->getMode() == osg::PrimitiveSet::TRIANGLES )
                    {
                        processTriangles( *da, *deui );
                        geom->removePrimitiveSet( numPS );
                    }
                    else if( ps->getMode() == osg::PrimitiveSet::TRIANGLE_FAN )
                    {
                        processTriFan( *da, *deui );
                        geom->removePrimitiveSet( numPS );
                    }
                    else if( ps->getMode() == osg::PrimitiveSet::TRIANGLE_STRIP )
                    {
                        processTriStrip( *da, *deui );
                        geom->removePrimitiveSet( numPS );
                    }
                }
            }

            // Create the new DEUI.
            if( deui->size() > 0 )
            {
                geom->addPrimitiveSet( deui.get() );
                newDEUIs_++;
            }
        }
    }
}

void
OptVisitor::processTriangles( const osg::DrawArrays& da, osg::VectorGLuint& indices )
{
    ++triangles_;

    GLint first = da.getFirst();
    GLsizei count = da.getCount();

    unsigned int index( first );
    GLsizei processed( 0 );
    while( processed + 3 <= count )
    {
        indices.push_back( index++ );
        indices.push_back( index++ );
        indices.push_back( index++ );
        processed += 3;
    }
}

void
OptVisitor::processTriFan( const osg::DrawArrays& da, osg::VectorGLuint& indices )
{
    ++triFans_;

    GLint first = da.getFirst();
    GLsizei count = da.getCount();

    unsigned int index( first+1 );
    GLsizei processed( 0 );
    while( processed + 3 <= count )
    {
        indices.push_back( first );
        indices.push_back( index++ );
        indices.push_back( index );
        processed++;
    }
}

void
OptVisitor::processTriStrip( const osg::DrawArrays& da, osg::VectorGLuint& indices )
{
    ++triStrips_;

    GLint first = da.getFirst();
    GLsizei count = da.getCount();

    unsigned int index( first );
    GLsizei processed( 0 );
    while( processed + 3 <= count )
    {
        indices.push_back( index );
        indices.push_back( index+1 );
        indices.push_back( index+2 );
        index++;
        processed++;
    }
}

void
OptVisitor::dump( std::ostream& ostr )
{
    ostr << "Converted from DrawArrays to DrawElementsUInt:" << std::endl;
    ostr << "\tTriangles:\t" << triangles_ << std::endl;
    ostr << "\tTriFans:\t" << triFans_ << std::endl;
    ostr << "\tTriStripss:\t" << triStrips_ << std::endl;
    ostr << "Total DrawElementsUInt created: " << newDEUIs_ << std::endl;
}
