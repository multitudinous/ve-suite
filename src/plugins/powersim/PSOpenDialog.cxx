
// --- VE-Suite Includes --- //
#include "PSOpenDialog.h"
//#include "../ConductorPluginEnums.h"

// --- wxWidgets Includes --- //
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/combobox.h>

// --- C/C++ Includes --- //
#include <vector>

BEGIN_EVENT_TABLE( PSOpenDialog, wxDialog )
EVT_CLOSE( PSOpenDialog::OnClose )
EVT_BUTTON( wxID_CANCEL, PSOpenDialog::CancelButtonClick )
EVT_BUTTON( wxID_OK, PSOpenDialog::OKButtonClick )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
PSOpenDialog::PSOpenDialog( wxWindow* parent )
    :
    wxDialog(
        parent,
        wxID_ANY,
        wxT( "SIP File" ),
        wxDefaultPosition,
        wxDefaultSize,
        wxCAPTION | wxCLOSE_BOX |
        wxMINIMIZE_BOX | wxDIALOG_NO_PARENT | /*wxSTAY_ON_TOP |*/ wxSYSTEM_MENU )
{
    CreateGUIControls();
}
////////////////////////////////////////////////////////////////////////////////
PSOpenDialog::~PSOpenDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::CreateGUIControls()
{
    //SetTitle(wxT("sim File"));
    SetIcon( wxNullIcon );
    SetSize( 8, 8, 370, 138 );
    Center();

    Label = new wxStaticText( this, wxID_ANY, wxT( "Sim Project" ), wxPoint( 8,16 ), wxDefaultSize, 0, wxT( "Label" ) );
    Label->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );

    CancelButton = new wxButton( this, wxID_CANCEL );//, wxT( "Cancel" ), wxPoint(275,55), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));
    CancelButton->SetFont( wxFont( 12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT( "Tahoma" ) ) );

    OKButton = new wxButton( this, wxID_OK );//, wxT( "Ok" ), wxPoint(195,55), wxSize(75,25), 0, wxDefaultValidator, wxT("OKButton"));
    OKButton->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::OnClose( wxCloseEvent& event )
{
    Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::OKButtonClick( wxCommandEvent& event )
{
    if( ComboBox->GetCurrentSelection() != wxNOT_FOUND )
    {
        EndModal( wxID_OK );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::CancelButtonClick( wxCommandEvent& event )
{
    EndModal( wxID_CANCEL );
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::SetPopulateFilenames( )
{
    wxDir pluginsDir( wxGetCwd() );
    wxString filename;

    //***
    wxString ext = wxString( "*.***", wxConvUTF8 );
    std::vector< wxString > simList;
    bool cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 4 );
        simList.push_back( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    //xml
    ext = wxString( "*.xml", wxConvUTF8 );
    std::vector< wxString > xmlList;
    cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 4 );
        xmlList.push_back( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    //find file that have both *** and xml files
    for( int i = 0; i < simList.size(); i++ )
    {
        for( int j = 0; j < xmlList.size(); j++ )
        {
            if( simList[i].CmpNoCase( xmlList[j] ) == 0 )
            {
                arrayStringFor_ComboBox.Add( simList[i] );
            }
        }
    }

    // construct combo box
    ComboBox = new wxComboBox(this, -1, wxT(""), wxPoint(149,13), wxSize(208,27), arrayStringFor_ComboBox, wxCB_READONLY, wxDefaultValidator, wxT("ComboBox"));
    ComboBox->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
}
////////////////////////////////////////////////////////////////////////////////
wxString PSOpenDialog::GetFilename( )
{
    return arrayStringFor_ComboBox[ComboBox->GetCurrentSelection()];
}
////////////////////////////////////////////////////////////////////////////////
