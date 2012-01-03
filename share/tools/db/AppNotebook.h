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

#ifndef APP_NOTEBOOK_H
#define APP_NOTEBOOK_H

// --- VE-Suite Includes --- //
#include "TypeDefs.h"

class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;
class wxGrid;

// --- C/C++ Includes --- //


/*!\file AppNotebook.h
 *
 */

/*!\class AppNotebook
 *
 */
class AppNotebook : public wxNotebook
{
public:
    ///Constructor
    AppNotebook( wxWindow* parent );

    ///Destructor
    virtual ~AppNotebook();

    ///
    void ClearTableDetails();

    ///
    void ClearTableData();

    ///
    void PopulateTableDetails( const StringVector2D* tableDetails );

    ///
    void PopulateTableData(
        const StringVector1D* tableFieldNames, const StringVector2D* tableData );

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    ///
    AppFrame* m_appFrame;

    wxPanel* m_tableDetailsPanel;
    wxPanel* m_tableDataPanel;
    wxPanel* m_sqlPanel;

    wxGrid* m_tableDetailsGrid;
    wxGrid* m_tableDataGrid;

    DECLARE_EVENT_TABLE()
};

#endif //APP_NOTEBOOK_H
