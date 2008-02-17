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

#include <ves/conductor/util/CADNodeMenu.h>
#include <ves/open/xml/cad/CADNode.h>
using namespace ves::open::xml::cad;
using namespace ves::conductor::util;
XERCES_CPP_NAMESPACE_USE
/////////////////////////////////////////
///Constructor                         //
/////////////////////////////////////////
CADNodeMenu::CADNodeMenu()
        : wxMenu()
{

    /*Append(GEOM_NODE_CREATE,
          _T("Create Root Node..."),
          _T(""),
           wxITEM_NORMAL);
    InsertSeparator(1);
    */


    wxMenu* addNodeSubMenu = new wxMenu();
    addNodeSubMenu->Append( GEOM_ASSEMBLY_CREATE,
                            _T( "Create New Assembly..." ),
                            _T( "Create a new assembly node" ),
                            wxITEM_NORMAL );
    addNodeSubMenu->InsertSeparator( 1 );
    addNodeSubMenu->Append( GEOM_VEG_FILE_ADD,
                            _T( "Load VEG file..." ),
                            _T( "" ),
                            wxITEM_NORMAL );
    addNodeSubMenu->Append( GEOM_CAD_FILE_ADD,
                            _T( "Load CAD file..." ),
                            _T( "" ),
                            wxITEM_NORMAL );
    Append( GEOM_NODE_ADD,
            _T( "Add Node..." ),
            addNodeSubMenu );

    Enable( GEOM_NODE_ADD, false );

    Append( GEOM_CLONE_ADD,
            _T( "Clone Node..." ),
            _T( "" ),
            wxITEM_NORMAL );
    Enable( GEOM_CLONE_ADD, false );

    /*Append(GEOM_MENU_MOVE_NODE,
           _T("Move To..."),
          _T(""),
           wxITEM_NORMAL);
    Enable(GEOM_MENU_MOVE_NODE, false);*/

    Append( GEOM_DELETE,
            _T( "Delete..." ),
            _T( "" ),
            wxITEM_NORMAL );
    Enable( GEOM_DELETE, false );
    InsertSeparator( 3 );

    wxMenu* toggleNodeSubMenu = new wxMenu();
    toggleNodeSubMenu->AppendRadioItem( GEOM_TOGGLE_ON,
                                        _T( "ON" ),
                                        _T( "Display this node" ) );
    toggleNodeSubMenu->AppendRadioItem( GEOM_TOGGLE_OFF,
                                        _T( "OFF" ),
                                        _T( "Hide this node" ) );
    Append( GEOM_DISPLAY_TOGGLE,
            _T( "Toggle Node" ),
            toggleNodeSubMenu );
    Enable( GEOM_DISPLAY_TOGGLE, true );
    InsertSeparator( 5 );

    Append( GEOM_PROPERTIES,
            _T( "Properties..." ),
            _T( "" ),
            wxITEM_NORMAL );
    Enable( GEOM_PROPERTIES, false );
    Append( GEOM_OPACITY,
            _T( "Transparency..." ),
            _T( "" ),
            wxITEM_NORMAL );
    Enable( GEOM_OPACITY, false );
}
/////////////////////////////
CADNodeMenu::~CADNodeMenu()
{}
//////////////////////////////////////////////////
void CADNodeMenu::EnableDeleteMenu( bool onOff )
{
    if( onOff == true )
    {
        Enable( GEOM_DELETE, true );
    }
    else
    {
        Enable( GEOM_DELETE, false );
    }
}
//////////////////////////////////////////////////
void CADNodeMenu::EnableGlobalMenus( bool onOff )
{
    if( onOff == true )
    {
        Enable( GEOM_DELETE, true );
        Enable( GEOM_PROPERTIES, true );
        Enable( GEOM_OPACITY, true );
        //Enable(GEOM_MENU_MOVE_NODE, true);
        Enable( GEOM_CLONE_ADD, true );
    }
    else
    {
        Enable( GEOM_DELETE, false );
        Enable( GEOM_PROPERTIES, false );
        Enable( GEOM_OPACITY, false );
        //Enable(GEOM_MENU_MOVE_NODE, false);
        Enable( GEOM_CLONE_ADD, false );
    }
}
/////////////////////////////////////////////
void CADNodeMenu::EnableCloneMenu( bool onOff )
{
    if( onOff == true )
    {
        Enable( GEOM_CLONE_ADD, true );
    }
    else
    {
        Enable( GEOM_CLONE_ADD, false );
    }
}
//////////////////////////////////////////////////
void CADNodeMenu::EnableAssemblyMenus( bool onOff )
{
    if( onOff == true )
    {
        Enable( GEOM_NODE_ADD, true );
    }
    else
    {
        Enable( GEOM_NODE_ADD, false );
    }
}
//////////////////////////////////////////////
void CADNodeMenu::EnablePartMenus( bool onOff )
{
    if( onOff == true )
    {
        EnableAssemblyMenus( false );
    }
    else
    {
        EnableAssemblyMenus( true );
    }
}
///////////////////////////////////////////////
void CADNodeMenu::EnableCreateMenu( bool onOff )
{
    if( onOff == true )
    {
        //Enable(GEOM_NODE_CREATE, true);
        EnableGlobalMenus( false );
    }
    else
    {
        //Enable(GEOM_NODE_CREATE, false);
        EnableGlobalMenus( true );
    }
}
////////////////////////////////////////////////
void CADNodeMenu::SetToggleNodeValue( bool onOff )
{
    /*get the toggle menu
    set the value to what was passed in*/
    if( onOff == true )
    {
        Check( GEOM_TOGGLE_ON, true );
    }
    else
    {
        Check( GEOM_TOGGLE_OFF, true );
    }
}
////////////////////////////////////////////////
void CADNodeMenu::DisableInitializePhysics()
{
    Enable( GEOM_INITIALIZE_PHYSICS, false );
}
////////////////////////////////////////////////
