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
#include <ves/conductor/util/CORBAServiceList.h>

#include "DSUOPlugin.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/xpm/square.xpm>
#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( DSUOPlugin, UIPluginBase )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( DSUOPlugin, UIPluginBase )

///////////////////////////////////////////////////////////////////////////////
DSUOPlugin::DSUOPlugin() :
    UIPluginBase(),
    mDynSimMenu( 0 )
{
    mPluginName = wxString( "DSUO", wxConvUTF8 );
    mDescription = wxString( "DynSim Unit Operation Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "DSUOPlugin" );
}
////////////////////////////////////////////////////////////////////////////////
DSUOPlugin::~DSUOPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
wxString DSUOPlugin::GetConductorName()
{
    return wxString( "DynSim_DSUnitOp", wxConvUTF8 );
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* DSUOPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mDynSimMenu )
    {
        return baseMenu;
    }
    
    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, false );

    mDynSimMenu = new wxMenu();
    baseMenu->Insert( 0, DSUOPLUGIN_DYNSIM_MENU,   _( "DynSim" ), mDynSimMenu,
                    _( "Used in conjunction with Sim" ) );
    baseMenu->Enable( DSUOPLUGIN_DYNSIM_MENU, true );
    return baseMenu;
}