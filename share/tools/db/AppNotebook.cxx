
// --- VE-Suite Includes --- //
#include "AppNotebook.h"
#include "DBAppEnums.h"

// --- wxWidgets Includes --- //
#include <wx/panel.h>

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
        wxNB_FIXEDWIDTH | wxNB_RIGHT )
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
}
////////////////////////////////////////////////////////////////////////////////
