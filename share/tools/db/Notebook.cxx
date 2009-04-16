
// --- VE-Suite Includes --- //
#include "Notebook.h"
#include "DBAppEnums.h"

// --- wxWidgets Includes --- //
#include <wx/panel.h>

BEGIN_EVENT_TABLE( Notebook, wxNotebook )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
Notebook::Notebook( wxWindow* parent )
    :
    wxNotebook(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxNB_FIXEDWIDTH | wxNB_TOP )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
Notebook::~Notebook()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Notebook::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );

    m_tableDetailsPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxRAISED_BORDER | wxTAB_TRAVERSAL );
    m_tableDetailsPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_tableDetailsPanel, wxT( "TableDetails" ), true );

    m_dataPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxRAISED_BORDER | wxTAB_TRAVERSAL );
    m_dataPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_dataPanel, wxT( "Data" ), false );

    m_sqlPanel =
        new wxPanel(
            this, wxID_ANY,
            wxDefaultPosition, wxDefaultSize,
            wxRAISED_BORDER | wxTAB_TRAVERSAL );
    m_sqlPanel->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    AddPage( m_sqlPanel, wxT( "SQL" ), false );
}
////////////////////////////////////////////////////////////////////////////////
