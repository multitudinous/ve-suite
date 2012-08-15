/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/data/CameraModePropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/MakeLive.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
CameraModePropertySet::CameraModePropertySet()
{
    mTableName = "CameraMode";
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
CameraModePropertySet::CameraModePropertySet( const CameraModePropertySet& orig )
    :
    PropertySet( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraModePropertySet::~CameraModePropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraModePropertySet::CreateSkeleton()
{
    //There should only be ONE UNIQUE CameraModePropertySet. To enforce this and
    //ensure that multiple copies never show up in the store, we set the uuid
    //here to the same thing every time.
    SetUUID( "00000000-0101-1010-1111-000011110000" );

    // NameTag is shown by default, but we don't want that for this PS.
    SetPropertyAttribute( "NameTag", "userVisible", false );

    AddProperty( "DisableCameraTools", true, "NOT VISIBLE IN BROWSER" );
    SetPropertyAttribute( "DisableCameraTools", "userVisible", false );

    AddProperty( "CameraManager", false, "Camera Manager" );
    AddProperty( "PictureMode", false, "Picture Mode" );

    AddProperty( "FlythroughSpeed", 10.0, "Flythrough speed (ft/s)" );
    SetPropertyAttribute( "FlythroughSpeed", "minimumValue", 0.1 );

    //TODO: Highlight tool stuff goes here; not sure yet what it is or should look like

    AddProperty( "Display", boost::any(), "Display Settings" );
    SetPropertyAttribute( "Display", "isUIGroupOnly", true );
    AddProperty( "Display_DepthOfField", false, "Depth of Field Effect" );
    AddProperty( "Display_ProjectionEffect", false, "Projection Effect" );
    AddProperty( "Display_Opacity", 1.0, "Opacity" );
    SetPropertyAttribute( "Display_Opacity", "minimumValue", 0.0 );
    SetPropertyAttribute( "Display_Opacity", "maximumValue", 1.0 );

    AddProperty( "CameraWindow", false, "Show Camera Window" );
    AddProperty( "CameraWindow_Resolution", 300, "Resolution" );
    SetPropertyAttribute( "CameraWindow_Resolution", "minimumValue", 0 );
    SetPropertyAttribute( "CameraWindow_Resolution", "maximumValue", 1000 );

    AddProperty( "DepthHelperWindow", false, "Show Depth Helper Window" );
    AddProperty( "DepthHelperWindow_Resolution", 200, "Resolution" );
    SetPropertyAttribute( "DepthHelperWindow_Resolution", "minimumValue", 0 );
    SetPropertyAttribute( "DepthHelperWindow_Resolution", "maximumValue", 1000 );
}
////////////////////////////////////////////////////////////////////////////////
void CameraModePropertySet::EnableLiveProperties( bool live )
{
    if( live )
    {
        MakeLiveBasePtr p;
        p = MakeLiveBasePtr( new MakeLive<bool>( mUUIDString,
                             GetProperty( "DisableCameraTools" ),
                             "DisableCameraTools", false ) );
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<bool>( mUUIDString,
                             GetProperty( "CameraManager" ),
                             "CameraManagerOn", false ) );
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<bool>( mUUIDString,
                             GetProperty( "PictureMode" ),
                             "PictureModeOn", false ) );
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<double>( mUUIDString,
                             GetProperty( "FlythroughSpeed" ),
                             "FlythroughSpeed", false ) );
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<bool>( mUUIDString,
                             GetProperty( "CameraWindow" ),
                             "CameraWindowOn", false ) );
        mLiveObjects.push_back( p );

        p = MakeLiveBasePtr( new MakeLive<int>( mUUIDString,
                             GetProperty( "CameraWindow_Resolution" ),
                             "CameraWindowResolution", false ) );
        mLiveObjects.push_back( p );
    }
    else
    {
        mLiveObjects.clear();
    }
}

////////////////////////////////////////////////////////////////////////////////
