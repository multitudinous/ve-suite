
// --- VE-Suite Includes --- //
#include "AppFrame.h"


// --- wxWidgets Includes --- //


// --- C/C++ Libraries --- //


BEGIN_EVENT_TABLE( AppFrame, wxFrame )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame()
    :
    wxFrame()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame( wxWindow * parent, wxWindowID id, const wxString& title )
    :
    wxFrame( parent, id, title, wxDefaultPosition, wxDefaultSize )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
