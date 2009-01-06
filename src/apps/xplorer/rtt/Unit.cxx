/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <apps/xplorer/rtt/Unit.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Image>
#include <osg/Program>
#include <osg/Texture2D>

//#include <osgDB/WriteFile>
//#include <osgDB/Registry>

// --- C/C++ Includes --- //
#include <cmath>

using namespace ves::xplorer::rtt;

////////////////////////////////////////////////////////////////////////////////
Unit::Unit()
    :
    osg::Group(),
    mActive( true ),
    mShader( NULL ),
    mDrawable( NULL ),
    mViewport( NULL ),
    mGeode( NULL )
{
    //Set default name
    setName( "Unit" );

    //No culling
    setCullingActive( false );
}
////////////////////////////////////////////////////////////////////////////////
Unit::Unit( const Unit& unit, const osg::CopyOp& copyop )
    :
    osg::Group( unit, copyop ),
    mActive( unit.mActive ),
    mInputTextures( unit.mInputTextures ),
    mOutputTextures( unit.mOutputTextures ),
    mShader( unit.mShader ),
    mDrawable( unit.mDrawable ),
    mViewport( unit.mViewport ),
    mGeode( unit.mGeode )
{

}
////////////////////////////////////////////////////////////////////////////////
Unit::~Unit()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* const Unit::GetInputTexture( int inputIndex ) const
{
    return mInputTextures.find( inputIndex )->second.get();
}
////////////////////////////////////////////////////////////////////////////////
