/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef APP_TREE_CTRL_H
#define APP_TREE_CTRL_H

// --- VE-Suite Includes --- //
class AppFrame;
class DBConnection;

// --- wxWidgets Includes --- //
#include <wx/treectrl.h>

// --- C/C++ Includes --- //
#include <string>


/*!\file AppTreeCtrl.h
 * AppTreeCtrl API
 */

/*!\class AppTreeCtrl
 *
 */
class AppTreeCtrl : public wxTreeCtrl
{
public:
    ///Constructor
    AppTreeCtrl( wxWindow* parent );

    ///Destructor
    virtual ~AppTreeCtrl();

    enum
    {
        DATABASE = 0,
        MSACCESS = 1,
        MYSQL = 2,
        TABLE = 3
    };

    ///
    void AddDBConnection( DBConnection* dbConnection );

protected:

private:
    ///Loads and stores the xpm images
    void LoadBitmaps();

    ///
    void CreateGUI();

    ///
    void SelectionChanged( wxTreeEvent& event );
    
    ///
    void RightClick( wxTreeEvent& event );
    
    ///
    void DoubleClick( wxTreeEvent& event );

    ///
    AppFrame* m_appFrame;

    ///
    wxTreeItemId m_rootID;

    ///
    wxTreeItemId m_selectionID;

    DECLARE_EVENT_TABLE();

};

struct DBConnectionData : public wxTreeItemData
{
    DBConnection* m_dbConnection;
};

struct DBTableData : public wxTreeItemData
{
    std::string m_dbTableName;
};

#endif //APP_TREE_CTRL_H
