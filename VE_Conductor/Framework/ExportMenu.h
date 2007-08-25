/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef EXPORT_MENU_H
#define EXPORT_MENU_H

/*!\file ExportMenu.h
 */

/*!\class ExportMenu
 * 
 */

// --- wxWidgets Includes --- //
#include <wx/menu.h>

// --- VE-Suite Includes --- //
namespace VE_Conductor
{
    class CORBAServiceList;
}

// --- C/C++ Libraries --- //
#include <map>
#include <string>

class ExportMenu : public wxMenu
{
public:
    ///Constructor
    ///\param parent The parent of the toolbar
    ExportMenu();

    ///Destructor
    virtual ~ExportMenu();

    ///\enum The enums for MainToolBar
    enum 
    {
        EXPORT_SCREEN_SHOT=950,
        EXPORT_DOT_FILE
    };

private:
    ///Adds the tools to the toolbar
    void CreateExportMenu();

    ///Handles event for new
    ///\param event The wxCommand event
    void OnScreenShot( wxCommandEvent& event );

    ///Handles event for open
    ///\param event The wxCommand event
    void OnDOTFile( wxCommandEvent& event );

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    DECLARE_EVENT_TABLE()
};

#endif //EXPORT_MENU_H
