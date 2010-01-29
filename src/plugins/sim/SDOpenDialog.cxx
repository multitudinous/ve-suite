#include "SDOpenDialog.h"
#include "../ConductorPluginEnums.h"
#include <vector>

BEGIN_EVENT_TABLE(SDOpenDialog,wxDialog)
	EVT_CLOSE(SDOpenDialog::OnClose)
	EVT_BUTTON(SDOPENDIALOG_CANCELBUTTON, SDOpenDialog::CancelButtonClick)
	EVT_BUTTON(SDOPENDIALOG_OKBUTTON, SDOpenDialog::OKButtonClick)
END_EVENT_TABLE()

SDOpenDialog::SDOpenDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

SDOpenDialog::~SDOpenDialog()
{
} 

void SDOpenDialog::CreateGUIControls()
{
	SetTitle(wxT("sim File"));
	SetIcon(wxNullIcon);
	SetSize(8,8,370,138);
	Center();

	Label = new wxStaticText(this, -1, wxT("Sim Project"), wxPoint(8,16), wxDefaultSize, 0, wxT("Label"));
	Label->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));

	CancelButton = new wxButton(this, SDOPENDIALOG_CANCELBUTTON, wxT("Cancel"), wxPoint(275,55), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));
	CancelButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));

	OKButton = new wxButton(this, SDOPENDIALOG_OKBUTTON, wxT("Ok"), wxPoint(195,55), wxSize(75,25), 0, wxDefaultValidator, wxT("OKButton"));
	OKButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
}

void SDOpenDialog::OnClose( wxCloseEvent& )
{
	Destroy();
}

void SDOpenDialog::OKButtonClick( wxCommandEvent& event )
{
    if( ComboBox->GetCurrentSelection() != wxNOT_FOUND )
    {
        EndModal( wxID_OK );
    }
}

void SDOpenDialog::CancelButtonClick( wxCommandEvent& event )
{
    EndModal( wxID_CANCEL );
}

void SDOpenDialog::SetPopulateFilenames( )
{
    wxDir pluginsDir( ::wxGetCwd() );
    wxString filename;

    //***
    wxString ext = wxString( "*.s4m", wxConvUTF8 );
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

wxString SDOpenDialog::GetFilename( )
{
    return arrayStringFor_ComboBox[ComboBox->GetCurrentSelection()];
}