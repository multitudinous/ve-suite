
// --- VE-Suite Includes --- //
#include "AppFrame.h"
#include "MenuBar.h"
#include "ToolBar.h"
#include "ConnectionDialog.h"

// --- wxWidgets Includes --- //


// --- C/C++ Libraries --- //


BEGIN_EVENT_TABLE( AppFrame, wxFrame )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame()
    :
    wxFrame(),
    m_connectionDialog( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame(
    wxWindow* parent,
    wxWindowID id,
    const wxString& title,
    const wxPoint& pos,
    const wxSize& size,
    long style )
    :
    wxFrame( parent, id, title, pos, size, style ),
    m_connectionDialog( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CreateGUI()
{

}
////////////////////////////////////////////////////////////////////////////////
