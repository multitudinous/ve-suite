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

#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
class AppMenuBar;
class AppToolBar;
class AppTreeCtrl;
class AppNotebook;
class DBConnectionDialog;
class VESConnectionDialog;
class CorbaUnitManager;

// --- wxWidgets Includes --- //
#include <wx/frame.h>


/*!\file AppFrame.h
 *
 */

/*!\class AppFrame
 * Main wxFrame for DBApp
 */
class AppFrame : public wxFrame
{
public:
    ///Constructor
    AppFrame( wxWindow* parent, wxWindowID id );

    ///Destructor
    virtual ~AppFrame();

    ///
    ///\return
    AppMenuBar* const GetAppMenuBar() const;

    ///
    ///\return
    AppToolBar* const GetAppToolBar() const;

    ///
    ///\return
    AppTreeCtrl* const GetAppTreeCtrl() const;

    ///
    ///\return
    AppNotebook* const GetAppNotebook() const;

    ///
    ///\return
    DBConnectionDialog* const GetDBConnectionDialog() const;

    ///
    ///\return
    VESConnectionDialog* const GetVESConnectionDialog() const;

    ///
    ///\return
    CorbaUnitManager* const GetCorbaUnitManager() const;

protected:

private:
    ///
    void CreateGUI();

    ///
    AppMenuBar* m_appMenuBar;
    
    ///
    AppToolBar* m_appToolBar;
    
    ///
    AppTreeCtrl* m_appTreeCtrl;
    
    ///
    AppNotebook* m_appNotebook;
    
    ///
    DBConnectionDialog* m_dbConnectionDialog;
    
    ///
    VESConnectionDialog* m_vesConnectionDialog;
    
    ///
    CorbaUnitManager* m_corbaUnitManager;
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
