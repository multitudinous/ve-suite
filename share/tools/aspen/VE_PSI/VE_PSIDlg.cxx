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
#include "stdafx.h"
#include "VE_PSI.h"
#include "VE_PSIDlg.h"

#include "VEPSI_i.h"
#include "CorbaUnitManager.h"

VE_PSIDlg::VE_PSIDlg(CWnd* pParent /*=NULL*/)
    : CDialog(VE_PSIDlg::IDD, pParent)
{
    m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void VE_PSIDlg::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
}

///////////////////////////////////////////////////////////////////////////////
BEGIN_MESSAGE_MAP(VE_PSIDlg, CDialog)
    ON_WM_PAINT()
    ON_WM_QUERYDRAGICON()
    ON_BN_CLICKED(IDOK, &VE_PSIDlg::OnBnClickedOk)
    ON_BN_CLICKED(IDCANCEL, &VE_PSIDlg::OnBnClickedCancel)
    ON_BN_CLICKED(IDC_BUTTON2, &VE_PSIDlg::OnBnClickedButton2)
END_MESSAGE_MAP()

///////////////////////////////////////////////////////////////////////////////
BOOL VE_PSIDlg::OnInitDialog()
{
    CDialog::OnInitDialog();
    SetIcon(m_hIcon, TRUE);
    SetIcon(m_hIcon, FALSE);

    CString path = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Path"), _T("C:\\") );
    CString name = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Name"), _T("localhost") );
    CString port = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Port"), _T("1239") );
    CString unitName = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Unit Name"), _T("VE-PSI") );
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
    Display->SetWindowTextA(name);
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
    Display->SetWindowTextA(port);
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
    Display->SetWindowTextA( path );
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT6));
    Display->SetWindowTextA( unitName );

    initialized = false;
    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
void VE_PSIDlg::OnPaint()
{
    if (IsIconic())
    {
        CPaintDC dc(this);
        SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);
        int cxIcon = GetSystemMetrics(SM_CXICON);
        int cyIcon = GetSystemMetrics(SM_CYICON);
        CRect rect;
        GetClientRect(&rect);
        int x = (rect.Width() - cxIcon + 1) / 2;
        int y = (rect.Height() - cyIcon + 1) / 2;
        dc.DrawIcon(x, y, m_hIcon);
    }
    else
    {
        CDialog::OnPaint();
    }
}

///////////////////////////////////////////////////////////////////////////////
HCURSOR VE_PSIDlg::OnQueryDragIcon()
{
    return static_cast<HCURSOR>(m_hIcon);
}

///////////////////////////////////////////////////////////////////////////////
BOOL VE_PSIDlg::OnIdle( LONG test )
{
    //Sleep( 10 );
    //if(initialized)
    //{
    //    commManager->CheckCORBAWork();
    //}
    return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
LRESULT VE_PSIDlg::WindowProc(UINT message, 
                WPARAM wParam, LPARAM lParam)
{
    DWORD QueueStatus;
    LRESULT resValue = 0;
    bool OnIdleRetVal = true;

    if(message == WM_ENTERIDLE) {
        OnIdleRetVal = OnIdle((UINT)wParam);
        if(!OnIdleRetVal)
            wParam = 0;
    } else
        resValue = CDialog::WindowProc(message, 
        wParam, lParam);

    QueueStatus = GetQueueStatus(QS_ALLINPUT);

    if(HIWORD(QueueStatus) == 0)
        PostMessage(WM_ENTERIDLE, 
            wParam + (OnIdleRetVal ? 1 : 0), 0);

    return resValue;
}

///////////////////////////////////////////////////////////////////////////////
void VE_PSIDlg::OnBnClickedCancel()
{   
    if( initialized )
    {
        CEdit *Display;
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
        CString path;
        Display->GetWindowText( path );
        AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Path"), path );
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
        CString name;
        Display->GetWindowText( name );
        AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Name"), name );
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
        CString port;
        Display->GetWindowText( port );
        AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Port"), port );

        if(commManager != NULL)
        {
            commManager->DestroyORB();
            delete commManager;
            commManager = NULL;
        }
    }
    this->OnOK();
}

///////////////////////////////////////////////////////////////////////////////
void VE_PSIDlg::OnBnClickedOk()
{
    if(!initialized)
    {
        CEdit *Display;
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
        CString name;
        Display->GetWindowText(name);
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
        CString port;
        Display->GetWindowText(port);
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
        CString dir;
        Display->GetWindowText(dir);
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT6));
        CString unitName;
        Display->GetWindowText(unitName);

        commManager = new CorbaUnitManager(this);
        commManager->SetComputerNameUnitNameAndPort( std::string( dir.GetString() ), 
            std::string( name.GetString() ) , std::string( port.GetString() ),
            std::string( unitName.GetString() ) );
        if ( commManager->RunORB() )
        {
            unitObject = commManager->GetUnitObject();
            if ( !unitObject )
            {
                AfxMessageBox( _T("Unable to connect to VE-CE" ));
                commManager->DestroyORB();
                delete commManager;
                commManager = NULL;
                initialized = false;
                return;
            }

            {
                initialized = true;
                GetDlgItem(IDOK)->EnableWindow(FALSE);
                commManager->CheckCORBAWorkThread();
            }
        }
        else
        {
            CEdit * log = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT1));
            log->SetSel(-1, -1);
            log->ReplaceSel("Unit name already exist.\r\nRename your unit!\r\n");
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
void VE_PSIDlg::OnBnClickedButton2()
{
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = _T("Pick a Directory");
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
        {
            CEdit *Display;
            Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
            CString thePath( path );
            Display->SetWindowTextA( thePath + "\\" );
        }

        IMalloc * imalloc = 0;
        if ( SUCCEEDED( SHGetMalloc ( &imalloc )) )
        {
            imalloc->Free ( pidl );
            imalloc->Release ( );
        }
    }
}
