/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef OPCUOVARDIALOG_H
#define OPCUOVARDIALOG_H

#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/VEConfig.h>

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>

#include <vector>

#include "OpcUOPlugin.h"

#undef OpcUOVarDialog_STYLE
#if wxCHECK_VERSION( 2, 9, 0 )
#define OpcUOVarDialog_STYLE wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxCLOSE_BOX
#else
#define OpcUOVarDialog_STYLE wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU | wxTHICK_FRAME | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxCLOSE_BOX
#endif

namespace ves
{
namespace conductor
{
class OpcUOVarDialog : public wxDialog
{
    private:
        DECLARE_EVENT_TABLE();
        
    public:
        OpcUOVarDialog(wxWindow *parent, wxWindowID id = 1,
            const wxString &title = wxT( "OpcUOVarDialog" ),
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = OpcUOVarDialog_STYLE );

        virtual ~OpcUOVarDialog();
        void CancelButtonClick( wxCommandEvent& event );
        void SetButtonClick( wxCommandEvent& event );
        //void SetData( wxString name = wxT(""), wxString description = wxT(""),
        //    wxString value = wxT(""), wxString units = wxT("") );
        void SetData( wxString name = wxT(""), wxString value = wxT("") );
        void UpdateSizes();
        void SetComponentName( wxString name );
        void SetServiceList(
            ves::conductor::util::CORBAServiceList * serviceList );
        wxString mCompName;
        ves::conductor::util::CORBAServiceList * mServiceList;
    
    private:
        wxButton *CancelButton;
        wxButton *SetButton;
        wxButton *MonitorButton;
        wxBoxSizer *WxBoxSizer1;
        wxGrid *WxGrid;
        wxFlexGridSizer *WxFlexGridSizer;
        std::vector< int > rowsChanged;
        int monitorRow;
        //OpcUOPlugin * mParent;
        
        std::string ConvertUnicode( const wxChar* data )
        {
            std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
            return tempStr;
        }
        wxString prefix;
    
    private:
        enum
        {
            ID_MONITORBUTTON = 1006,
            ID_CANCELBUTTON = 1005,
            ID_SETBUTTON = 1004,
            ID_WXGRID = 1002,
            ID_DUMMY_VALUE_
        };
    
    private:
        void OnClose( wxCloseEvent& event );
        void CreateGUIControls( );
        void OnCellChange( wxGridEvent& event );
        void OnSelectCell( wxGridEvent& event );
        void OnMonitorVariable( wxCommandEvent& event );
};
}
}
#endif
