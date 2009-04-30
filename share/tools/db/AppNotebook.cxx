
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
    m_tableDataPanel( NULL ),
    m_sqlPanel( NULL ),
    m_tableDetailsGrid( NULL ),
    m_tableDataGrid( NULL )
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

    m_tableDataPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
    m_tableDataPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_tableDataPanel, wxT( "Table Data" ), false );

    m_sqlPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxTAB_TRAVERSAL );
    m_sqlPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_sqlPanel, wxT( "SQL" ), false );

    //Create the table details grid
	wxGridSizer* tableDetailsGridSizer;
	tableDetailsGridSizer = new wxGridSizer( 1, 1, 0, 0 );
	
	m_tableDetailsGrid =
        new wxGrid(
            m_tableDetailsPanel, wxID_ANY,
            wxDefaultPosition, wxDefaultSize );

    //Grid
	m_tableDetailsGrid->CreateGrid( 0, 6 );
	m_tableDetailsGrid->EnableEditing( false );
	m_tableDetailsGrid->EnableGridLines( true );
	m_tableDetailsGrid->EnableDragGridSize( false );
	m_tableDetailsGrid->SetMargins( 0, 0 );
	
	//Label Appearance
    m_tableDetailsGrid->SetColLabelValue( 0, wxT( "Field" ) );
    m_tableDetailsGrid->SetColLabelValue( 1, wxT( "Type" ) );
    m_tableDetailsGrid->SetColLabelValue( 2, wxT( "Null" ) );
    m_tableDetailsGrid->SetColLabelValue( 3, wxT( "Key" ) );
    m_tableDetailsGrid->SetColLabelValue( 4, wxT( "Default" ) );
    m_tableDetailsGrid->SetColLabelValue( 5, wxT( "Extra" ) );

    //Columns
    m_tableDetailsGrid->AutoSizeColumns();
	m_tableDetailsGrid->EnableDragColMove( false );
	m_tableDetailsGrid->EnableDragColSize( false );
	m_tableDetailsGrid->SetColLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );
	
	//Rows
	m_tableDetailsGrid->AutoSizeRows();
	m_tableDetailsGrid->EnableDragRowSize( false );
	m_tableDetailsGrid->SetRowLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );

    //Cell Defaults
	m_tableDetailsGrid->SetDefaultCellAlignment( wxALIGN_LEFT, wxALIGN_TOP );

	tableDetailsGridSizer->Add( m_tableDetailsGrid, 0, wxEXPAND, 5 );
    m_tableDetailsGrid->Hide();
	
	m_tableDetailsPanel->SetSizer( tableDetailsGridSizer );
	m_tableDetailsPanel->Layout();
	tableDetailsGridSizer->Fit( m_tableDetailsPanel );

    //Create the table data grid
	wxGridSizer* tableDataGridSizer;
	tableDataGridSizer = new wxGridSizer( 1, 1, 0, 0 );
	
	m_tableDataGrid =
        new wxGrid(
            m_tableDataPanel, wxID_ANY,
            wxDefaultPosition, wxDefaultSize );

    //Grid
	m_tableDataGrid->CreateGrid( 0, 0 );
	m_tableDataGrid->EnableEditing( false );
	m_tableDataGrid->EnableGridLines( true );
	m_tableDataGrid->EnableDragGridSize( false );
	m_tableDataGrid->SetMargins( 0, 0 );

    //Columns
    m_tableDataGrid->AutoSizeColumns();
	m_tableDataGrid->EnableDragColMove( false );
	m_tableDataGrid->EnableDragColSize( false );
	m_tableDataGrid->SetColLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );
	
	//Rows
	m_tableDataGrid->AutoSizeRows();
	m_tableDataGrid->EnableDragRowSize( false );
	m_tableDataGrid->SetRowLabelAlignment( wxALIGN_CENTRE, wxALIGN_CENTRE );

    //Cell Defaults
	m_tableDataGrid->SetDefaultCellAlignment( wxALIGN_LEFT, wxALIGN_TOP );

	tableDataGridSizer->Add( m_tableDataGrid, 0, wxEXPAND, 5 );
    m_tableDataGrid->Hide();
	
	m_tableDataPanel->SetSizer( tableDataGridSizer );
	m_tableDataPanel->Layout();
	tableDataGridSizer->Fit( m_tableDataPanel );
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::ClearTableDetails()
{
    m_tableDetailsGrid->DeleteRows( 0, m_tableDetailsGrid->GetNumberRows() );
    m_tableDetailsGrid->Hide();
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::ClearTableData()
{
    m_tableDataGrid->DeleteCols( 0, m_tableDataGrid->GetNumberCols() );
    m_tableDataGrid->DeleteRows( 0, m_tableDataGrid->GetNumberRows() );
    m_tableDataGrid->Hide();
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::PopulateTableDetails( const StringVector2D* tableDetails )
{
    size_t numRows = (*tableDetails).size();
    size_t numCols = (*tableDetails)[ 0 ].length;

    //Resize the grid to fit the table details information
    int numTableRows = m_tableDetailsGrid->GetNumberRows();
    if( numTableRows < numRows )
    {
        m_tableDetailsGrid->AppendRows( numRows - numTableRows );
    }
    else if( numTableRows > numRows )
    {
        m_tableDetailsGrid->DeleteRows( numRows, numTableRows - numRows );
    }

    //Fill the cells with the new table details information
    for( size_t i = 0; i < numRows; ++i )
    {
        for( size_t j = 0; j < numCols; ++j )
        {
            //m_tableDetailsGrid->SetCellValue(
                //i, j, wxT( (*tableDetails)[ i ][ j ].c_str() ) );
        }
    }

    //Now fit the cells to their contents
    m_tableDetailsGrid->AutoSizeColumns();
    m_tableDetailsGrid->AutoSizeRows();
    m_tableDetailsGrid->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::PopulateTableData(
    const StringVector1D* tableFieldNames, const StringVector2D* tableData )
{
    size_t numRows = (*tableData).size();
    size_t numCols = (*tableData)[ 0 ].length;

    //Fill the column labels with the table field names
    for( size_t i = 0; i < numCols; ++i )
    {
        //m_tableDataGrid->SetColLabelValue(
            //i, wxT( (*tableFieldNames)[ i ].c_str() ) );
    }

    //Resize the grid to fit the table data information
    int numTableCols = m_tableDataGrid->GetNumberCols();
    if( numTableCols < numCols )
    {
        m_tableDataGrid->AppendCols( numCols - numTableCols );
    }
    else if( numTableCols > numCols )
    {
        m_tableDataGrid->DeleteCols( numCols, numTableCols - numCols );
    }

    int numTableRows = m_tableDataGrid->GetNumberRows();
    if( numTableRows < numRows )
    {
        m_tableDataGrid->AppendRows( numRows - numTableRows );
    }
    else if( numTableRows > numRows )
    {
        m_tableDataGrid->DeleteRows( numRows, numTableRows - numRows );
    }

    //Fill the cells with the new table data information
    for( size_t i = 0; i < numRows; ++i )
    {
        for( size_t j = 0; j < numCols; ++j )
        {
            //m_tableDataGrid->SetCellValue(
                //i, j, wxT( (*tableData)[ i ][ j ].c_str() ) );
        }
    }

    //Now fit the cells to their contents
    m_tableDataGrid->AutoSizeColumns();
    m_tableDataGrid->AutoSizeRows();
    m_tableDataGrid->Show();
}
////////////////////////////////////////////////////////////////////////////////
