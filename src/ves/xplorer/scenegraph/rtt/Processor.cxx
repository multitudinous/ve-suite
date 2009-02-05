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
#include <ves/xplorer/scenegraph/rtt/Processor.h>
#include <ves/xplorer/scenegraph/rtt/Unit.h>

// --- OSG Includes --- //
#include <osg/Camera>

// --- C/C++ Includes --- //

using namespace ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
Processor::Processor()
    :
    osg::Group(),
    mDirty( true ),
    mDirtyUnitGraph( true ),
    mUseColorClamp( true ),
    mCamera( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Processor::Processor( const Processor& processor, const osg::CopyOp& copyop )
    :
    osg::Group( processor, copyop ),
    mDirty( processor.mDirty ),
    mDirtyUnitGraph( processor.mDirtyUnitGraph ),
    mUseColorClamp( processor.mUseColorClamp ),
    mCamera( processor.mCamera )
{

}
////////////////////////////////////////////////////////////////////////////////
Processor::~Processor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* Processor::GetCamera()
{
    return mCamera.get();
}
////////////////////////////////////////////////////////////////////////////////
void Processor::SetCamera( osg::Camera* camera )
{
    //Setup camera
    if( mCamera.get() != camera )
    {
        mCamera = camera;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Processor::DirtyUnitSubgraph()
{
    mDirtyUnitGraph = true;
}
////////////////////////////////////////////////////////////////////////////////
bool Processor::IsDirtyUnitSubgraph() const
{
    return mDirtyUnitGraph;
}
////////////////////////////////////////////////////////////////////////////////
void Processor::MarkUnitSubgraphNonDirty()
{
    mDirtyUnitGraph = false;
}
////////////////////////////////////////////////////////////////////////////////
Unit* Processor::FindUnit( const std::string& unitName )
{
    if( !mDirtyUnitGraph )
    {
        //FindUnitVisitor uv( name );
        //uv.run( this );

        //return uv.getResult();
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
bool Processor::RemoveUnit( Unit* unit )
{
    if( mDirtyUnitGraph )
    {
        osg::notify( osg::INFO )
            << "osgPPU::Processor::removeUnit(" << unit->getName()
            << ") - cannot remove unit because the graph is not valid."
            << std::endl;
        
        //return false;
    }

    //RemoveUnitVisitor uv;
    //uv.run( unit );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingSphere Processor::ComputeBound() const
{
    return osg::BoundingSphere();
}
////////////////////////////////////////////////////////////////////////////////
void Processor::UseColorClamp( bool useColorClamp )
{
    mUseColorClamp = useColorClamp;

    mDirty = true;
}
////////////////////////////////////////////////////////////////////////////////
