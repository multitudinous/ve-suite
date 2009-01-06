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
#include <apps/xplorer/rtt/UnitInOut.h>

// --- OSG Includes --- //
#include <osg/FrameBufferObject>

// --- C/C++ Includes --- //

using namespace ves::xplorer::rtt;

////////////////////////////////////////////////////////////////////////////////
UnitInOut::UnitInOut()
    :
    Unit(),
    mFBO( new osg::FrameBufferObject() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::UnitInOut( const UnitInOut& unitInOut, const osg::CopyOp& copyop )
    :
    Unit( unitInOut, copyop ),
    mFBO( unitInOut.mFBO )
{

}
////////////////////////////////////////////////////////////////////////////////
UnitInOut::~UnitInOut()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInOut::SetOutputTexture( osg::Texture* outputTexture, int mrt )
{
    if( outputTexture )
    {
        mOutputTextures[ mrt ] = outputTexture;
    }
    else
    {
        mOutputTextures[ mrt ] = osg::ref_ptr< osg::Texture >( NULL );
    }
}
////////////////////////////////////////////////////////////////////////////////
