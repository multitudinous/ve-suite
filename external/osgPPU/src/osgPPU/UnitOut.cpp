/***************************************************************************
 *   Copyright (c) 2008   Art Tevs                                         *
 *                                                                         *
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 3 of        *
 *   the License, or (at your option) any later version.                   *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU Lesse General Public License for more details.                    *
 *                                                                         *
 *   The full license is in LICENSE file included with this distribution.  *
 ***************************************************************************/

#include <osgPPU/UnitOut.h>
#include <osgPPU/Processor.h>

#include <osg/Drawable>
#include <osg/Geometry>

#include <iostream>

namespace osgPPU
{
    //------------------------------------------------------------------------------
    UnitOut::UnitOut(const UnitOut& unit, const osg::CopyOp& copyop) :
        Unit(unit, copyop)
    {
    
    }
    
    //------------------------------------------------------------------------------
    UnitOut::~UnitOut()
    {
    
    }

    //------------------------------------------------------------------------------
    void UnitOut::init()
    {
        // init default
        Unit::init();

        // create a quad geometry
        /*
        mDrawable = createTexturedQuadDrawable();
        mGeode->removeDrawables(0, mGeode->getNumDrawables());
        mGeode->addDrawable(mDrawable.get());
        */
    }

    void UnitOut::CreateVESQuad( osg::Vec3Array* quadVerts, osg::Vec2Array* texCoords )
    {
        osg::Geometry* quadGeom = new osg::Geometry();
        quadGeom->setVertexArray( quadVerts );
        quadGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, quadVerts->size() ) );
        quadGeom->setTexCoordArray( 0, texCoords );
        quadGeom->setStateSet(new osg::StateSet());
        quadGeom->setUseDisplayList(false);
        quadGeom->setColorBinding(osg::Geometry::BIND_OFF);
        //quadGeom->setDrawCallback(new Unit::DrawCallback(this));
        mGeode->setCullingActive( false );
        
        mDrawable = quadGeom;
        mGeode->addDrawable( mDrawable.get() );        
    }
}; // end namespace
