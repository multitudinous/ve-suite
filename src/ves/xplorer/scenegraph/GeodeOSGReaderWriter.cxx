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
#include <ves/xplorer/scenegraph/Geode.h>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

using namespace osg;
using namespace osgDB;

// forward declare functions to use later.
bool VEGeode_readLocalData( Object& obj, Input& fr );
bool VEGeode_writeLocalData( const Object& obj, Output& fw );

// register the read and write functions with the osgDB::Registry.
RegisterDotOsgWrapperProxy ve_GeodeProxy
(
    new ves::xplorer::scenegraph::Geode,
    "Geode",
    "Object Node ves::xplorer::scenegraph::Geode",
    &VEGeode_readLocalData,
    &VEGeode_writeLocalData
);

bool VEGeode_readLocalData( Object& obj, Input& fr )
{
    bool iteratorAdvanced = false;

    ves::xplorer::scenegraph::Geode& geode = static_cast< ves::xplorer::scenegraph::Geode&>( obj );

    int num_drawables;
    if (( fr[0].matchWord( "num_drawables" ) || fr[0].matchWord( "num_geosets" ) ) &&
            fr[1].getInt( num_drawables ) )
    {
        // could allocate space for children here...
        fr += 2;
        iteratorAdvanced = true;
    }

    Drawable* drawable = NULL;
    while (( drawable = fr.readDrawable() ) != NULL )
    {
        geode.addDrawable( drawable );
        iteratorAdvanced = true;
    }

    return iteratorAdvanced;
}


bool VEGeode_writeLocalData( const osg::Object& obj, Output& fw )
{
    const Geode& geode = static_cast<const Geode&>( obj );
    //fw.writeObject( geode );
    fw.indent() << "num_drawables " << geode.getNumDrawables() << std::endl;

    for( unsigned int i = 0;i < geode.getNumDrawables();++i )
    {
        fw.writeObject( *geode.getDrawable( i ) );
    }

    return true;
}
