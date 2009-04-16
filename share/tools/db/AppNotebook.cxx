
// --- VE-Suite Includes --- //
#include "AppNotebook.h"
#include "DBAppEnums.h"

#include "xpm/ToolBar/AddConnection.xpm"

// --- wxWidgets Includes --- //
#include <wx/panel.h>
#include <wx/imaglist.h>

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
        wxNB_FIXEDWIDTH | wxNB_TOP )
{
    LoadImages();
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppNotebook::~AppNotebook()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::LoadImages()
{
    wxImageList* imageList = new wxImageList( 16, 16 );
    imageList->Add( wxBitmap( AddConnection_xpm ) );
    imageList->Add( wxBitmap( AddConnection_xpm ) );
    imageList->Add( wxBitmap( AddConnection_xpm ) );

    AssignImageList( imageList );
}
////////////////////////////////////////////////////////////////////////////////
void AppNotebook::CreateGUI()
{
    //If not called, notebook bg color doesn't work right when switching themes
    SetBackgroundColour( GetThemeBackgroundColour() );
    //SetBackgroundColour( wxColour( 255, 255, 255 ) );

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

    //Add images to the panels
    SetPageImage( 0, 0 );
    SetPageImage( 1, 1 );
    SetPageImage( 2, 2 );
}
////////////////////////////////////////////////////////////////////////////////
