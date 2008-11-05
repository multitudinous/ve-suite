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
        //osg::Geode* mQuadGeode = new osg::Geode();
        osg::Geometry* quadGeom = new osg::Geometry();

        quadGeom->setVertexArray( quadVerts );
        //size_t numViewports = quadVerts->size() * 0.25;
        //std::cout << numViewports << std::endl;
        //for( size_t i = 0; i < numViewports; ++i )
        {
            quadGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, quadVerts->size() ) );
            osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
            quadTexCoords->resize( 4 );
            (*quadTexCoords)[ 0 ].set( (*texCoords)[0][0], (*texCoords)[0][1] );
            (*quadTexCoords)[ 1 ].set( (*texCoords)[1][0], (*texCoords)[1][1] );
            (*quadTexCoords)[ 2 ].set( (*texCoords)[2][0], (*texCoords)[2][1] );
            (*quadTexCoords)[ 3 ].set( (*texCoords)[3][0], (*texCoords)[3][1] );
            //quadGeom->setTexCoordArray( 0, quadTexCoords.get() );
        }
        quadGeom->setTexCoordArray( 0, texCoords );

        
        mGeode->setCullingActive( false );
        
        mDrawable = quadGeom;
        mGeode->addDrawable( mDrawable.get() );        
    }
}; // end namespace
