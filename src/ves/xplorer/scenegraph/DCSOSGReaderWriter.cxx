/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
*************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/scenegraph/DCS.h>
#include <osg/io_utils>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

using namespace osg;
using namespace osgDB;

// forward declare functions to use later.
bool VEDCS_readLocalData( Object& obj, Input& fr );
bool VEDCS_writeLocalData( const Object& obj, Output& fw );

// register the read and write functions with the osgDB::Registry.
RegisterDotOsgWrapperProxy ve_DCSProxy
(
    new ves::xplorer::scenegraph::DCS,
    "DCS",
    "Object Node Transform Group ves::xplorer::scenegraph::DCS",
    &VEDCS_readLocalData,
    &VEDCS_writeLocalData
);

bool VEDCS_readLocalData( Object& obj, Input& fr )
{
    bool iteratorAdvanced = false;

    ves::xplorer::scenegraph::DCS& transform = static_cast<ves::xplorer::scenegraph::DCS&>( obj );

    if( fr.matchSequence( "position %f %f %f" ) )
    {
        osg::Vec3d pos;
        fr[1].getFloat( pos[0] );
        fr[2].getFloat( pos[1] );
        fr[3].getFloat( pos[2] );

        transform.setPosition( pos );

        fr += 4;
        iteratorAdvanced = true;
    }

    if( fr.matchSequence( "attitude %f %f %f %f" ) )
    {
        osg::Quat att;
        fr[1].getFloat( att[0] );
        fr[2].getFloat( att[1] );
        fr[3].getFloat( att[2] );
        fr[4].getFloat( att[3] );

        transform.setAttitude( att );

        fr += 5;
        iteratorAdvanced = true;
    }

    if( fr.matchSequence( "scale %f %f %f" ) )
    {
        osg::Vec3d scale;
        fr[1].getFloat( scale[0] );
        fr[2].getFloat( scale[1] );
        fr[3].getFloat( scale[2] );

        transform.setScale( scale );

        fr += 4;
        iteratorAdvanced = true;
    }

    if( fr.matchSequence( "pivotPoint %f %f %f" ) )
    {
        osg::Vec3d pivot;
        fr[1].getFloat( pivot[0] );
        fr[2].getFloat( pivot[1] );
        fr[3].getFloat( pivot[2] );

        transform.setPivotPoint( pivot );

        fr += 4;
        iteratorAdvanced = true;
    }

    return iteratorAdvanced;
}


bool VEDCS_writeLocalData( const Object& obj, Output& fw )
{
    const ves::xplorer::scenegraph::DCS& transform = static_cast<const ves::xplorer::scenegraph::DCS&>( obj );
    //fw.writeObject( transform );
    fw.indent() << "position " << transform.getPosition() << std::endl;
    fw.indent() << "attitude " << transform.getAttitude() << std::endl;
    fw.indent() << "scale " << transform.getScale() << std::endl;
    fw.indent() << "pivotPoint " << transform.getPivotPoint() << std::endl;
    return true;
}
