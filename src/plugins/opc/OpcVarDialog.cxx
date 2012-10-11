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
#include "OpcVarDialog.h"
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::conductor;
using namespace ves::conductor::util;
BEGIN_EVENT_TABLE( OpcVarDialog, wxDialog )
    EVT_CLOSE( OpcVarDialog::OnClose )
    EVT_BUTTON( OPC_VAR_ID_CANCELBUTTON, OpcVarDialog::CancelButtonClick )
    EVT_BUTTON( OPC_VAR_ID_SETBUTTON, OpcVarDialog::SetButtonClick )
    EVT_BUTTON( OPC_VAR_ID_MONITORBUTTON, OpcVarDialog::OnMonitorVariable )
    EVT_GRID_CELL_CHANGE( OpcVarDialog::OnCellChange )
    //EVT_GRID_SELECT_CELL( OpcVarDialog::OnSelectCell )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
OpcVarDialog::OpcVarDialog(wxWindow *parent, wxEvtHandler *tempParent,
                               std::string unitName, wxWindowID id,
                               const wxString &title, const wxPoint &position,
                               const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
    CreateGUIControls();
    m_parent = tempParent;
    m_vendorData = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    m_vendorData->SetData( "vendorUnit", unitName );
}

///////////////////////////////////////////////////////////////////////////////
OpcVarDialog::~OpcVarDialog()
{
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::CreateGUIControls()
{
    WxFlexGridSizer = new wxFlexGridSizer(0, 1, 0, 0);
    this->SetSizer(WxFlexGridSizer);
    this->SetAutoLayout(true);

    WxGrid = new wxGrid(this, OPC_VAR_ID_WXGRID, wxPoint(5,5), wxSize(320,120),
        wxVSCROLL | wxHSCROLL);
    WxGrid->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));
    WxGrid->SetDefaultColSize(50);
    WxGrid->SetDefaultRowSize(25);
    WxGrid->SetRowLabelSize(50);
    WxGrid->SetColLabelSize(25);
    //WxGrid->CreateGrid(0,3,wxGrid::wxGridSelectCells);
    WxGrid->CreateGrid(0,1,wxGrid::wxGridSelectRows);
    WxFlexGridSizer->Add(WxGrid,0,wxEXPAND | wxALL,5);

    WxBoxSizer1 = new wxBoxSizer(wxHORIZONTAL);
    WxFlexGridSizer->Add(WxBoxSizer1, 0, wxALIGN_CENTER | wxALL, 5);

    SetButton = new wxButton(this, OPC_VAR_ID_SETBUTTON, wxT("Set"), wxPoint(5,5),
        wxSize(75,25), 0, wxDefaultValidator, wxT("SetButton"));
    SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));
    WxBoxSizer1->Add(SetButton,0,wxALIGN_CENTER | wxALL,5);

    CancelButton = new wxButton(this, OPC_VAR_ID_CANCELBUTTON, wxT("Close"),
        wxPoint(90,5), wxSize(75,25), 0, wxDefaultValidator,
        wxT("CancelButton"));
    CancelButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));
    WxBoxSizer1->Add(CancelButton,0,wxALIGN_CENTER | wxALL,5);

    MonitorButton = new wxButton(this, OPC_VAR_ID_MONITORBUTTON, wxT("Monitor"),
        wxPoint(115,5), wxSize(75,25), 0, wxDefaultValidator,
        wxT("MonitorButton"));
    MonitorButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));
    WxBoxSizer1->Add(MonitorButton,0,wxALIGN_CENTER | wxALL,5);

    SetTitle(wxT("OpcVarDialog"));
    SetIcon(wxNullIcon);
    
    GetSizer()->Layout();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Center();
    
    WxGrid->SetColLabelValue( 0, _("Value") );
    //WxGrid->SetColLabelValue( 0, _("Description") );
    //WxGrid->SetColLabelValue( 1, _("Value") );
    //WxGrid->SetColLabelValue( 2, _("Units") );

    //this should be done dynamically
    WxGrid->SetRowLabelSize( 100 );

    //wxGridCellAttr * readOnly = new wxGridCellAttr();
    //readOnly->SetReadOnly(false);
    //WxGrid->SetColAttr( 1, readOnly );
    //WxGrid->SetColAttr( 2, readOnly );
    WxFlexGridSizer->SetFlexibleDirection(wxBOTH);
    WxFlexGridSizer->AddGrowableCol(0);
    WxFlexGridSizer->AddGrowableRow(0);
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::OnClose( wxCloseEvent& )
{
    this->Destroy();
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::CancelButtonClick( wxCommandEvent& )
{
    this->Destroy();
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::SetButtonClick( wxCommandEvent& )
{  
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    //input variables;
    params->SetCommandName( "setOPCValues" );
    params->AddDataValuePair( m_vendorData );

    int numOfChanges = rowsChanged.size();
    for(int i = 0; i < numOfChanges; i++)
    {        
        std::vector<std::string> paramList;
        
        //component name
        //paramList.push_back( ConvertUnicode( CompName.c_str() ) );

        //variable name
        wxString varName = WxGrid->GetRowLabelValue( rowsChanged[i] );
        std::string temp = ConvertUnicode( mCompName.c_str() ) + "." +
            ConvertUnicode( varName.c_str() );
        //paramList.push_back( ConvertUnicode( varName.c_str() ) );

        //value
        wxString value = WxGrid->GetCellValue( rowsChanged[i], 0 );
        //paramList.push_back( ConvertUnicode( value.c_str() ) );

        //add list to DVP
        ves::open::xml::DataValuePairPtr
            inpParams( new ves::open::xml::DataValuePair() );
        inpParams->SetDataName( temp );
        inpParams->SetDataString( ConvertUnicode( value.c_str() ) );
        params->AddDataValuePair( inpParams );
    }

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
    std::string >( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    mServiceList->Query( status );

    wxMessageDialog popup( this, _("Data has been sent to DynSim") );
    popup.ShowModal(); 
}

// SetData
/*void OpcVarDialog::SetData( wxString name, wxString description,
                                     wxString value, wxString units )
{
    //add a new row
    WxGrid->AppendRows( 1 );
    int index = WxGrid->GetNumberRows() - 1;

    //remove the name of the block from the variable name
    int remove = name.Find( wxT( ".") );
    prefix = name.SubString( 0, remove );
    name = name.SubString( remove + 1,name.Length() );
    
    //insert all data
    WxGrid->SetRowLabelValue( index, name );
    WxGrid->SetCellValue( index, 0, description );
    WxGrid->SetCellValue( index, 1, value );
    WxGrid->SetCellValue( index, 2, units );
}*/

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::SetData( wxString name, wxString value )
{
    //add a new row
    WxGrid->AppendRows( 1 );
    int index = WxGrid->GetNumberRows() - 1;
    
    //insert all data
    WxGrid->SetRowLabelValue( index, name );
    WxGrid->SetCellValue( index, 0, value );
    //WxGrid->SetReadOnly(index, 0, false);
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::UpdateSizes()
{
    WxGrid->AutoSize();
    WxGrid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTRE );
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::OnCellChange(wxGridEvent& event)
{
    rowsChanged.push_back( event.GetRow() );
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::OnSelectCell(wxGridEvent& event)
{
    monitorRow =  event.GetRow();
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::SetComponentName( wxString name )
{
    mCompName = name;
}

///////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::SetServiceList(
    ves::conductor::util::CORBAServiceList * serviceList )
{
    mServiceList = serviceList;
}
////////////////////////////////////////////////////////////////////////////////
void OpcVarDialog::OnMonitorVariable( wxCommandEvent& event )
{
    if( WxGrid->GetSelectedRows().IsEmpty() )
    {
        return;
    }
    
    monitorRow = WxGrid->GetSelectedRows()[0];

    ves::open::xml::CommandPtr monitor( new ves::open::xml::Command() );
    monitor->SetCommandName("addVariable");
    monitor->AddDataValuePair( m_vendorData );
    
    wxString varName = WxGrid->GetRowLabelValue( monitorRow );
    std::string temp = /*ConvertUnicode( mCompName.c_str() ) + "." +*/
        ConvertUnicode( varName.c_str() );

    ves::open::xml::DataValuePairPtr
        variables( new ves::open::xml::DataValuePair() );
    variables->SetData( "variable", temp.c_str() );
    monitor->AddDataValuePair( variables );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr,
        std::string >( monitor, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = mServiceList->Query( status );
    ::wxPostEvent( m_parent, event );
    //DynamicsDataBuffer::instance()->Enable();
    //parent->StartTimer( 1000 );
}

//NEED TO KEEP TRACK OF WHICH VARIABLES ARE ALREADY ADDED
//SO YOU CAN REMOVE THAT VALUE FROM BEING MONITORED AND ADD THE
//NEW VARIABLE YOU WANT TO MONITOR