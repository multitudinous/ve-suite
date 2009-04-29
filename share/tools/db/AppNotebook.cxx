
// --- VE-Suite Includes --- //
#include "AppNotebook.h"
#include "AppFrame.h"
#include "DBAppEnums.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/panel.h>
#include <wx/grid.h>

BEGIN_EVENT_TABLE( AppNotebook, wxNotebook )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppNotebook::AppNotebook( wxWindow* parent )
    :
    wxNotebook(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxNB_FIXEDWIDTH | wxNB_TOP ),
    m_appFrame( static_cast< AppFrame* >( parent ) ),
    m_tableDetailsPanel( NULL ),
    m_dataPanel( NULL ),
    m_sqlPanel( NULL ),
    m_tableDetailsGrid( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppNotebook::~AppNotebook()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );

    //Create the panels
    m_tableDetailsPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
    m_tableDetailsPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_tableDetailsPanel, wxT( "Table Details" ), true );

    m_dataPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
    m_dataPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_dataPanel, wxT( "Data" ), false );

    m_sqlPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
    m_sqlPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_sqlPanel, wxT( "SQL" ), false );

    //Create the grid
	wxGridSizer* tableDetailsGridSizer;
	tableDetailsGridSizer = new wxGridSizer( 1, 1, 0, 0 );
	
	m_tableDetailsGrid =
        new wxGrid(
            m_tableDetailsPanel, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxALWAYS_SHOW_SB | wxHSCROLL | wxVSCROLL );

    //Grid
	m_tableDetailsGrid->CreateGrid( 1, 6 );
	m_tableDetailsGrid->EnableEditing( false );
	m_tableDetailsGrid->EnableGridLines( true );
	m_tableDetailsGrid->EnableDragGridSize( false );
	m_tableDetailsGrid->SetMargins( 0, 0 );
	
	//Columns
    m_tableDetailsGrid->AutoSizeColumns();
	m_tableDetailsGrid->EnableDragColMove( false );
	m_tableDetailsGrid->EnableDragColSize( false );
	m_tableDetailsGrid->SetColLabelSize( 30 );
	m_tableDetailsGrid->SetColLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );
	
	//Rows
	m_tableDetailsGrid->AutoSizeRows();
	m_tableDetailsGrid->EnableDragRowSize( false );
	m_tableDetailsGrid->SetRowLabelSize( 80 );
	m_tableDetailsGrid->SetRowLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );
	
	//Label Appearance
    m_tableDetailsGrid->SetColLabelValue( 0, wxT( "Field" ) );
    m_tableDetailsGrid->SetColLabelValue( 1, wxT( "Type" ) );
    m_tableDetailsGrid->SetColLabelValue( 2, wxT( "Null" ) );
    m_tableDetailsGrid->SetColLabelValue( 3, wxT( "Key" ) );
    m_tableDetailsGrid->SetColLabelValue( 4, wxT( "Default" ) );
    m_tableDetailsGrid->SetColLabelValue( 5, wxT( "Extra" ) );

    //Cell Defaults
	m_tableDetailsGrid->SetDefaultCellAlignment( wxALIGN_LEFT, wxALIGN_TOP );
	tableDetailsGridSizer->Add( m_tableDetailsGrid, 0, wxEXPAND, 5 );
	
	m_tableDetailsPanel->SetSizer( tableDetailsGridSizer );
	m_tableDetailsPanel->Layout();
	tableDetailsGridSizer->Fit( m_tableDetailsPanel );
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::PopulateTableDetails( const StringArray2D* tableDetails )
{
    size_t numRows = (*tableDetails).size();
    size_t numCols = (*tableDetails)[ 0 ].size();

    int numTableRows = m_tableDetailsGrid->GetNumberRows();
    if( numTableRows < numRows )
    {
        m_tableDetailsGrid->AppendRows( numRows - numTableRows );
    }
    else if( numTableRows > numRows )
    {
        m_tableDetailsGrid->DeleteRows( numRows, numTableRows - numRows );
    }

    for( size_t i = 0; i < numRows; ++i )
    {
        for( size_t j = 0; j < numCols; ++j )
        {
            m_tableDetailsGrid->SetCellValue(
                i, j, wxT( (*tableDetails)[ i ][ j ].c_str() ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
