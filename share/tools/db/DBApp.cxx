
// --- VE-Suite Includes --- //
#include "DBApp.h"
#include "AppFrame.h"

// --- wxWidgets Includes --- //
#include <wx/confbase.h>


IMPLEMENT_APP( DBApp );

////////////////////////////////////////////////////////////////////////////////
bool DBApp::OnInit()
{
    SetAppName( wxT( "VE-DB" ) );

    m_appFrame = new AppFrame( NULL, wxNewId() );
    m_appFrame->Show( true );
    SetTopWindow( m_appFrame );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
int DBApp::OnExit()
{
    delete wxConfigBase::Set( ( wxConfigBase* ) NULL );

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
