#include "AspenDynamicsDialog.h"

BEGIN_EVENT_TABLE(AspenDynamicsDialog,wxDialog)
	EVT_CLOSE(AspenDynamicsDialog::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,AspenDynamicsDialog::CancelButtonClick)
	EVT_BUTTON(ID_SETBUTTON,AspenDynamicsDialog::SetButtonClick)
    EVT_GRID_CELL_CHANGE(AspenDynamicsDialog::WxGridCellChange)
END_EVENT_TABLE()

AspenDynamicsDialog::AspenDynamicsDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

AspenDynamicsDialog::~AspenDynamicsDialog()
{
} 

void AspenDynamicsDialog::CreateGUIControls()
{
	WxFlexGridSizer = new wxFlexGridSizer(0, 1, 0, 0);
	this->SetSizer(WxFlexGridSizer);
	this->SetAutoLayout(true);

	WxGrid = new wxGrid(this, ID_WXGRID, wxPoint(5,5), wxSize(320,120), wxVSCROLL | wxHSCROLL);
	WxGrid->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
	WxGrid->SetDefaultColSize(50);
	WxGrid->SetDefaultRowSize(25);
	WxGrid->SetRowLabelSize(50);
	WxGrid->SetColLabelSize(25);
	WxGrid->CreateGrid(0,3,wxGrid::wxGridSelectCells);
	WxFlexGridSizer->Add(WxGrid,0,wxEXPAND | wxALL,5);

	WxBoxSizer1 = new wxBoxSizer(wxHORIZONTAL);
	WxFlexGridSizer->Add(WxBoxSizer1, 0, wxALIGN_CENTER | wxALL, 5);

	SetButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(5,5), wxSize(75,25), 0, wxDefaultValidator, wxT("SetButton"));
	SetButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
	WxBoxSizer1->Add(SetButton,0,wxALIGN_CENTER | wxALL,5);

	CancelButton = new wxButton(this, ID_CANCELBUTTON, wxT("Close"), wxPoint(90,5), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));
	CancelButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
	WxBoxSizer1->Add(CancelButton,0,wxALIGN_CENTER | wxALL,5);

	SetTitle(wxT("AspenDynamicsDialog"));
	SetIcon(wxNullIcon);
	
	GetSizer()->Layout();
	GetSizer()->Fit(this);
	GetSizer()->SetSizeHints(this);
	Center();
	
	WxGrid->SetColLabelValue( 0, wxString("Description") );
	WxGrid->SetColLabelValue( 1, wxString("Value") );
	WxGrid->SetColLabelValue( 2, wxString("Units") );

    //this should be done dynamically
    WxGrid->SetRowLabelSize( 500 );

	wxGridCellAttr * readOnly = new wxGridCellAttr();
	readOnly->SetReadOnly(true);
	WxGrid->SetColAttr( 0, readOnly );
	WxGrid->SetColAttr( 2, readOnly );
	WxFlexGridSizer->SetFlexibleDirection(wxBOTH);
	WxFlexGridSizer->AddGrowableCol(0);
	WxFlexGridSizer->AddGrowableRow(0);
}

//for closing
void AspenDynamicsDialog::OnClose(wxCloseEvent& /*event*/)
{
	this->Destroy();
}

//CancelButtonClick
void AspenDynamicsDialog::CancelButtonClick(wxCommandEvent& event)
{
    this->Destroy();
}

// SetButtonClick
void AspenDynamicsDialog::SetButtonClick(wxCommandEvent& event)
{  
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    //input variables;
    params->SetCommandName( "setParam" );

    int numOfChanges = rowsChanged.size();
    for(int i = 0; i < numOfChanges; i++)
    {        
        std::vector<std::string> paramList;
        
        //component name
        paramList.push_back( CompName.c_str() );

        //variable name
        wxString varName = WxGrid->GetRowLabelValue( rowsChanged[i] );
        paramList.push_back( varName.c_str() );

        //value
        wxString value = WxGrid->GetCellValue( rowsChanged[i], 1 );
        paramList.push_back( value.c_str() );

        //add list to DVP
        ves::open::xml::DataValuePairPtr
            inpParams( new ves::open::xml::DataValuePair() );
        inpParams->SetData("params",paramList);
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
    ServiceList->Query( status );

    wxMessageDialog popup( this, "Data has been sent to Aspen Dynamics");
    popup.ShowModal(); 
}

// SetData
void AspenDynamicsDialog::SetData( wxString name, wxString description,
                                     wxString value, wxString units )
{
	WxGrid->AppendRows( 1 );
    int index = WxGrid->GetNumberRows() - 1;
	WxGrid->SetRowLabelValue( index, name );
	WxGrid->SetCellValue( index, 0, description );
	WxGrid->SetCellValue( index, 1, value );
	WxGrid->SetCellValue( index, 2, units );
}

//Update the grid size to match data size
void AspenDynamicsDialog::UpdateSizes()
{
	WxGrid->AutoSize();
    WxGrid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTRE );
}

//WxGridCellChange
void AspenDynamicsDialog::WxGridCellChange(wxGridEvent& event)
{
	rowsChanged.push_back( event.GetRow() );
}

//WxGridCellChange
void AspenDynamicsDialog::SetComponentName( wxString name )
{
	CompName = name;
}

void AspenDynamicsDialog::SetServiceList(
    ves::conductor::util::CORBAServiceList * serviceList )
{
    this->ServiceList = serviceList;
}