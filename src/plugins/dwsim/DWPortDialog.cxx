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
#include "DWPortDialog.h"
#include "DWPlugin.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/open/xml/DataValuePair.h>
#include <wx/tokenzr.h>

using namespace ves::conductor;
BEGIN_EVENT_TABLE( DWPortDialog, wxDialog )
    EVT_CLOSE( DWPortDialog::OnClose )
    EVT_BUTTON( DW_PORT_ID_CANCELBUTTON, DWPortDialog::CancelButtonClick )
    EVT_BUTTON( DW_PORT_ID_SETBUTTON, DWPortDialog::SetButtonClick )
END_EVENT_TABLE()

DWPortDialog::DWPortDialog(wxWindow *parent, std::string unitName, bool input, /*wxEvtHandler *tempParent,*/ wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
    CreateGUIControls();
    mIputPort = input;
    //m_parent = tempParent;
    m_vendorData = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    m_vendorData->SetData( "vendorUnit", unitName );
}

DWPortDialog::~DWPortDialog()
{
} 

void DWPortDialog::CreateGUIControls()
{
    WxFlexGridSizer = new wxFlexGridSizer(0, 1, 0, 0);
    this->SetSizer(WxFlexGridSizer);
    this->SetAutoLayout(true);

    WxGrid = new wxGrid(this, DW_VAR_ID_WXGRID, wxPoint(5,5), wxSize(320,120), wxVSCROLL | wxHSCROLL);
    WxGrid->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
    WxGrid->SetDefaultColSize(50);
    WxGrid->SetDefaultRowSize(25);
    WxGrid->SetRowLabelSize(50);
    WxGrid->SetColLabelSize(25);
    //WxGrid->CreateGrid(0,3,wxGrid::wxGridSelectCells);
    WxGrid->CreateGrid(0,3,wxGrid::wxGridSelectRows);
    WxFlexGridSizer->Add(WxGrid,0,wxEXPAND | wxALL,5);

    WxBoxSizer1 = new wxBoxSizer(wxHORIZONTAL);
    WxFlexGridSizer->Add(WxBoxSizer1, 0, wxALIGN_CENTER | wxALL, 5);

    SetButton = new wxButton(this, DW_PORT_ID_SETBUTTON, wxT("Set"), wxPoint(5,5), wxSize(75,25), 0, wxDefaultValidator, wxT("SetButton"));
    SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
    WxBoxSizer1->Add(SetButton,0,wxALIGN_CENTER | wxALL,5);

    CancelButton = new wxButton(this, DW_PORT_ID_CANCELBUTTON, wxT("Close"), wxPoint(90,5), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));
    CancelButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
    WxBoxSizer1->Add(CancelButton,0,wxALIGN_CENTER | wxALL,5);

    SetTitle(wxT("DWPortDialog"));
    SetIcon(wxNullIcon);
    
    GetSizer()->Layout();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Center();
    
    WxGrid->SetColLabelValue( 0, _("Description") );
    WxGrid->SetColLabelValue( 1, _("Value") );
    WxGrid->SetColLabelValue( 2, _("Units") );

    //this should be done dynamically
    WxGrid->SetRowLabelSize( 100 );

    WxFlexGridSizer->SetFlexibleDirection(wxBOTH);
    WxFlexGridSizer->AddGrowableCol(0);
    WxFlexGridSizer->AddGrowableRow(0);
}

//for closing
void DWPortDialog::OnClose(wxCloseEvent& /*event*/)
{
    this->Destroy();
}

//CancelButtonClick
void DWPortDialog::CancelButtonClick(wxCommandEvent& event)
{
    this->Destroy();
}

// SetButtonClick
void DWPortDialog::SetButtonClick(wxCommandEvent& event)
{  
    if( mIputPort ) //input port
    {
        int monitorRow = WxGrid->GetSelectedRows()[0];
        ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
        params->SetCommandName( "setInputPort" );
        params->AddDataValuePair( m_vendorData );

        //int numOfChanges = rowsChanged.size();
        //for(int i = 0; i < numOfChanges; i++)
        //{        
            std::vector<std::string> paramList;
            
            wxString varName = WxGrid->GetRowLabelValue( monitorRow );
            wxStringTokenizer tkz( varName, wxT("."));
            //varName
            
            ///component name
            paramList.push_back( ConvertUnicode( tkz.GetNextToken().c_str() ) );

            //variable name
            //reinsert the prefix
            //varName = prefix.Append( varName.c_str() );
            paramList.push_back( ConvertUnicode( tkz.GetNextToken().c_str() ) );

            //value
            //wxString value = WxGrid->GetCellValue( monitorRow, 1 );
            //paramList.push_back( ConvertUnicode( value.c_str() ) );

            //add list to DVP
            ves::open::xml::DataValuePairPtr
                inpParams( new ves::open::xml::DataValuePair() );
            inpParams->SetData("params",paramList);
            params->AddDataValuePair( inpParams );
        //}

        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
            nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
        std::string >( params, "vecommand" ) );

        ves::open::xml::XMLReaderWriter commandWriter;
        std::string status="returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );
        m_serviceList->Query( status );

        wxMessageDialog popup( this, _("Input port variable has been set") );
        popup.ShowModal(); 
    }
    else //output port
    {
        int monitorRow = WxGrid->GetSelectedRows()[0];
        ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
        params->SetCommandName( "setOutputPort" );
        params->AddDataValuePair( m_vendorData );

        //int numOfChanges = rowsChanged.size();
        //for(int i = 0; i < numOfChanges; i++)
        //{        
            std::vector<std::string> paramList;
            
            wxString varName = WxGrid->GetRowLabelValue( monitorRow );
            wxStringTokenizer tkz( varName, wxT("."));
            //varName
            
            ///component name
            paramList.push_back( ConvertUnicode( tkz.GetNextToken().c_str() ) );

            //variable name
            //reinsert the prefix
            //varName = prefix.Append( varName.c_str() );
            paramList.push_back( ConvertUnicode( tkz.GetNextToken().c_str() ) );

            //value
            //wxString value = WxGrid->GetCellValue( monitorRow, 1 );
            //paramList.push_back( ConvertUnicode( value.c_str() ) );

            //add list to DVP
            ves::open::xml::DataValuePairPtr
                inpParams( new ves::open::xml::DataValuePair() );
            inpParams->SetData("params",paramList);
            params->AddDataValuePair( inpParams );
        //}

        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
            nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
        std::string >( params, "vecommand" ) );

        ves::open::xml::XMLReaderWriter commandWriter;
        std::string status="returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );
        m_serviceList->Query( status );

        wxMessageDialog popup( this, _("Output port variable has been set") );
        popup.ShowModal(); 
    }
}

// SetData
void DWPortDialog::SetData( wxString name, wxString description,
                                     wxString value, wxString units )
{
    //add a new row
    WxGrid->AppendRows( 1 );
    int index = WxGrid->GetNumberRows() - 1;

    //remove the name of the block from the variable name
    //int remove = name.Find( wxT( ".") );
    //prefix = name.SubString( 0, remove );
    //name = name.SubString( remove + 1,name.Length() );
    
    //insert all data
    WxGrid->SetRowLabelValue( index, name );
    WxGrid->SetCellValue( index, 0, description );
    WxGrid->SetCellValue( index, 1, value );
    WxGrid->SetCellValue( index, 2, units );
}

//Update the grid size to match data size
void DWPortDialog::UpdateSizes()
{
    WxGrid->AutoSize();
    WxGrid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTRE );
}
////////////////////////////////////////////////////////////////////////////////
void DWPortDialog::SetServiceList(
    ves::conductor::util::CORBAServiceList * serviceList )
{
    m_serviceList = serviceList;
}
///////////////////////////////////////////////////////////////////////////////
void DWPortDialog::SetComponentName( wxString name )
{
    m_compName = name;
}