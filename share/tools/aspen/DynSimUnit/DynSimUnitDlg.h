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
// DynSimUnitDlg.h : header file
//

#pragma once
#include "afxwin.h"

#define REFRESH_OPC 1

class AspenUnit_i;
class CorbaUnitManager;
class CDialogThread;

// CDynSimUnitDlg dialog
class CDynSimUnitDlg : public CDialog
{
// Construction
public:
    CDynSimUnitDlg(CWnd* pParent = NULL);    // standard constructor

// Dialog Data
    enum { IDD = IDD_DYNSIMUNIT_DIALOG };

    protected:
    virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
    virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
    BOOL OnIdle( LONG lCount );

// Implementation
protected:
    HICON m_hIcon;

    // Generated message map functions
    virtual BOOL OnInitDialog();
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    afx_msg void OnTimer (UINT TimerVal);
    DECLARE_MESSAGE_MAP()

private:
    AspenUnit_i* unitObject;
    CString moduleName;
    CorbaUnitManager* commManager;
    //CDialogThread* orbThread;
    CString computerName;
    CString unitName;
    CString portNumber;
    afx_msg void OnBnClickedCancel();
    bool initialized;
public:
    afx_msg void OnBnClickedOk();
    afx_msg void OnBnClickedButton2();
};
