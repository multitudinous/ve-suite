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

// --- My Includes --- //
#include "ExcelWrap.h"
#include "HyperLabUnit_i.h"
#include "HyperLabUnit_client.h"

//#include "Excel_2000/CApplication.h"
#include "Excel_2003/CApplication.h"

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

#pragma once

//CHyperLabUnitDlg dialog
class CHyperLabUnitDlg : public CDialog
{
public:
    //Standard Constructor
    CHyperLabUnitDlg( CWnd* pParent = NULL );
    ~CHyperLabUnitDlg();

    //Dialog Data
    enum{ IDD = IDD_HYPERLABUNIT_DIALOG };

    //void SetVESUnit(Body_Unit_i* unitPtr);
    void OnEnterIdle( UINT nWhy, CWnd* pWho );
    virtual LRESULT WindowProc( UINT message, WPARAM wParam, LPARAM lParam );
    BOOL OnIdle( LONG lCount );

    CEdit editComputerName;
    CEdit editPortNumber;
    CEdit editUnitName;
    afx_msg void OnBnClickedOpen();
    afx_msg void OnBnClickedConnect();
    afx_msg void OnEnChangePort();
    afx_msg void OnEnChangeComputer();
    afx_msg void OnEnChangeUnit();

    CApplication app;
    CorbaUnitManager* commManager;
    Body_Unit_i* unitObject;

protected:
    //DDX/DDV support
    virtual void DoDataExchange( CDataExchange* pDX );

    //Implementation
    HICON m_hIcon;

    //Generated message map functions
    BOOL OnInitDialog();

    afx_msg void OnSysCommand( UINT nID, LPARAM lParam );
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    DECLARE_MESSAGE_MAP()

private:
    std::vector< std::string > fileList;
    CString path;
    CString computerName;
    CString portNumber;
    CString unitName;

    ExcelWrap excelWrap;
};
