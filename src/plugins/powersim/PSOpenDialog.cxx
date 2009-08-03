
// --- VE-Suite Includes --- //
#include "PSOpenDialog.h"

// --- wxWidgets Includes --- //
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/sizer.h>
#include <wx/dir.h>

// --- C/C++ Includes --- //
#include <vector>

BEGIN_EVENT_TABLE( PSOpenDialog, wxDialog )
EVT_CLOSE( PSOpenDialog::OnClose )
EVT_BUTTON( wxID_CANCEL, PSOpenDialog::OnCancel )
EVT_BUTTON( wxID_OK, PSOpenDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
PSOpenDialog::PSOpenDialog( wxWindow* parent )
    :
    wxDialog(
        parent,
        wxID_ANY,
        wxT( "SIP File" ),
        wxDefaultPosition,
        wxSize( 400, 125 ),
        wxCAPTION | wxCLOSE_BOX | /*wxMINIMIZE_BOX |*/
        wxDIALOG_NO_PARENT | wxSYSTEM_MENU ),
    m_comboBox( NULL )
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
    SetIcon( wxNullIcon );
    SetSizeHints( wxDefaultSize, wxDefaultSize );

    wxBoxSizer* dialogSizer;
    dialogSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* mainSizer;
    mainSizer = new wxBoxSizer( wxHORIZONTAL );

    wxStaticText* label;
    label = new wxStaticText( this, wxID_ANY, wxT( "Powersim Project" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    label->Wrap( -1 );
    label->SetFont( wxFont( wxNORMAL_FONT->GetPointSize(), 70, 90, 92, false, wxEmptyString ) );
    mainSizer->Add( label, 1, wxALIGN_CENTER | wxALL, 5 );

    m_comboBox = new wxComboBox( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, NULL, wxCB_READONLY );//, wxDefaultValidator, wxT( "ComboBox" ) ); 
    mainSizer->Add( m_comboBox, 2, wxALIGN_CENTER | wxALL, 5 );

    dialogSizer->Add( mainSizer, 1, wxEXPAND, 5 );

    wxStdDialogButtonSizer* sdbSizer;
    wxButton* sdbSizerOK;
    wxButton* sdbSizerCancel;
    sdbSizer = new wxStdDialogButtonSizer();
    sdbSizerOK = new wxButton( this, wxID_OK );
    sdbSizer->AddButton( sdbSizerOK );
    sdbSizerCancel = new wxButton( this, wxID_CANCEL );
    sdbSizer->AddButton( sdbSizerCancel );
    sdbSizer->Realize();
    dialogSizer->Add( sdbSizer, 1, wxEXPAND, 5 );

    SetSizer( dialogSizer );
    Layout();
    Center();
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::OnClose( wxCloseEvent& event )
{
    Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::OnOK( wxCommandEvent& event )
{
    if( m_comboBox->GetCurrentSelection() != wxNOT_FOUND )
    {
        EndModal( wxID_OK );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::OnCancel( wxCommandEvent& event )
{
    EndModal( wxID_CANCEL );
}
////////////////////////////////////////////////////////////////////////////////
void PSOpenDialog::SetPopulateFilenames()
{
    m_comboBox->Clear();

    wxDir pluginsDir( wxGetCwd() );
    wxString filename;

    //sip
    wxString ext = wxString( "*.sip", wxConvUTF8 );
    std::vector< wxString > sipList;
    bool cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 4 );
        sipList.push_back( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    //Find file that have both sip and "" files
    wxArrayString arrayString;
    for( int i = 0; i < sipList.size(); ++i )
    {
        //for( int j = 0; j < xmlList.size(); ++j )
        //{
            //if( sipList[ i ].CmpNoCase( xmlList[ j ] ) == 0 )
            //{
                arrayString.Add( sipList[ i ] );
            //}
        //}
    }

    m_comboBox->Append( arrayString );
}
////////////////////////////////////////////////////////////////////////////////
wxString PSOpenDialog::GetFilename()
{
    //return m_comboBox->GetStringSelection();
    return m_comboBox->GetLabelText();
}
////////////////////////////////////////////////////////////////////////////////
