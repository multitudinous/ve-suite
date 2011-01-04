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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/rtt/UnitOut.h>

// --- OSG Includes --- //
#include <osg/Geode>

// --- C/C++ Includes --- //

using namespace ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
UnitOut::UnitOut()
    :
    Unit()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitOut::UnitOut( const UnitOut& unitOut, const osg::CopyOp& copyop )
    :
    Unit( unitOut, copyop )
{

}
////////////////////////////////////////////////////////////////////////////////
UnitOut::~UnitOut()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitOut::Initialize()
{
    Unit::Initialize();

    //Setup a geode and the drawable as childs of this unit
    if( !mDrawable.valid() )
    {
        CreateTexturedQuadDrawable();
    }
    mGeode->removeDrawables( 0, mGeode->getNumDrawables() );
    mGeode->addDrawable( mDrawable.get() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitOut::CreateVESQuad(
    osg::Vec3Array* quadVerts, osg::Vec2Array* texCoords )
{
    /*
    osg::Geometry* quadGeom = new osg::Geometry();
    quadGeom->setVertexArray( quadVerts );
    quadGeom->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, quadVerts->size() ) );
    quadGeom->setTexCoordArray( 0, texCoords );
    quadGeom->setStateSet( new osg::StateSet() );
    quadGeom->setUseDisplayList( false );
    quadGeom->setColorBinding( osg::Geometry::BIND_OFF );
    //quadGeom->setDrawCallback( new Unit::DrawCallback( this ) );
    mGeode->setCullingActive( false );
    
    mDrawable = quadGeom;
    mGeode->addDrawable( mDrawable.get() );
    */
}
////////////////////////////////////////////////////////////////////////////////
