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

#ifndef CMAIN_DLG_H
#define CMAIN_DLG_H

// --- VE_PowersimUnit Includes --- //
class Body_Unit_i;
class CorbaUnitManager;

// --- ATL Includes --- //
#include <atlbase.h>
#include <atlwin.h>

class CMainDlg : public CDialogImpl< CMainDlg >
{
public:
    enum { IDD = IDD_MAINDLG };

    CMainDlg();

    virtual BOOL PreTranslateMessage( MSG* pMsg );
    virtual BOOL OnIdle();

    BEGIN_MSG_MAP( CMainDlg )
        MESSAGE_HANDLER( WM_INITDIALOG, OnInitDialog )
        MESSAGE_HANDLER( WM_DESTROY, OnDestroy )
        COMMAND_ID_HANDLER( IDD_ABOUTDLG, OnAppAbout )
        COMMAND_ID_HANDLER( IDOK, OnOK )
        COMMAND_ID_HANDLER( IDCANCEL, OnCancel )
        COMMAND_ID_HANDLER( IDC_DIRPICKER, OnDirPicker )
        MESSAGE_HANDLER( WM_SYSCOMMAND, OnSysCommand )
    END_MSG_MAP()

    LRESULT OnInitDialog( UINT, WPARAM, LPARAM, BOOL& );
    LRESULT OnDestroy( UINT, WPARAM, LPARAM, BOOL& );
    LRESULT OnAppAbout( WORD , WORD, HWND, BOOL& );
    LRESULT OnOK( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnCancel( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnDirPicker( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnSysCommand( UINT, WPARAM wParam, LPARAM, BOOL& bHandled );

    void CloseDialog( int nVal );

protected:

private:
    bool m_initialized;

    Body_Unit_i* m_unitObject;
    CorbaUnitManager* m_corbaUnitManager;

};

#endif //CMAIN_DLG_H
