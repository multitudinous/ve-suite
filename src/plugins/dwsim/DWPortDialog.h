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
#ifndef DWPORTDIALOG_H
#define DWPORTDIALOG_H

#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/VEConfig.h>

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>
#include <vector>

namespace ves
{
namespace conductor
{

/*!\file DWPortDialog.h
  Aspen Dynamics Unit Operations Variable Dialog
  */
/*!\class ves::conductor::DWPortDialog
 * This class is for the Aspen Dynamics variables.
 */
class DWPortDialog : public wxDialog
{
public:
    ///Constructor
    DWPortDialog(wxWindow *parent,
        std::string unitName, bool input,
        wxWindowID id = 1, const wxString &title = wxT("DWPortDialog"),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU |
        wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxCLOSE_BOX);

    ///Destructor
    virtual ~DWPortDialog();

    ///???
    void CancelButtonClick(wxCommandEvent& event);

    ///???
    //void SetButtonClick(wxCommandEvent& event);

    ///???
    void SetData( wxString name = wxT(""), wxString description = wxT(""),
        wxString value = wxT(""), wxString units = wxT("") );

    ///???
    void UpdateSizes();

    ///???
    void SetComponentName( wxString name );

    ///???
    void SetServiceList(
        ves::conductor::util::CORBAServiceList * serviceList );

    ///???
    wxString m_compName;

    ///???
    ves::conductor::util::CORBAServiceList * m_serviceList;

private:
    wxButton *CancelButton;
    wxButton *SetButton;
    //wxButton *MonitorButton;
    wxBoxSizer *WxBoxSizer1;
    wxGrid *WxGrid;
    wxFlexGridSizer *WxFlexGridSizer;
    std::vector< int > rowsChanged;
    int m_monitorRow;
    wxEvtHandler * m_parent;
    wxString prefix;
    bool mIputPort;
    ves::open::xml::DataValuePairPtr m_vendorData;
    
    ///???
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >
            ( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    ///???
    void OnClose(wxCloseEvent& event);

    ///???
    void CreateGUIControls();

    ///???
    void OnCellChange( wxGridEvent& event );

    ///???
    void OnSelectCell( wxGridEvent& event );

    ///???
    void SetButtonClick(wxCommandEvent& event);

    DECLARE_EVENT_TABLE();
};
}
}
#endif
