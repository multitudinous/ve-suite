/////////////////////////////////////////////////////////////////////////////
// Name:        streamlines.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:45:04 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "streamlines.h"
#endif

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

////@begin includes
////@end includes

#include "streamlines.h"

////@begin XPM images
////@end XPM images

/*!
 * Application instance implementation
 */

////@begin implement app
//IMPLEMENT_APP( StreamlinesApp )
////@end implement app

/*!
 * StreamlinesApp type definition
 */

//IMPLEMENT_CLASS( StreamlinesApp, wxApp )

/*!
 * StreamlinesApp event table definition
 */
/*
BEGIN_EVENT_TABLE( StreamlinesApp, wxApp )

////@begin StreamlinesApp event table entries
////@end StreamlinesApp event table entries

END_EVENT_TABLE()
*/
/*!
 * Constructor for StreamlinesApp
 */
/*
StreamlinesApp::StreamlinesApp()
{
////@begin StreamlinesApp member initialisation
////@end StreamlinesApp member initialisation
}
*/
/*!
 * Initialisation for StreamlinesApp
 */
/*
bool StreamlinesApp::OnInit()
{    
////@begin StreamlinesApp initialisation
    // Remove the comment markers above and below this block
    // to make permanent changes to the code.

#if wxUSE_XPM
    wxImage::AddHandler(new wxXPMHandler);
#endif
#if wxUSE_LIBPNG
    wxImage::AddHandler(new wxPNGHandler);
#endif
#if wxUSE_LIBJPEG
    wxImage::AddHandler(new wxJPEGHandler);
#endif
#if wxUSE_GIF
    wxImage::AddHandler(new wxGIFHandler);
#endif
////@end StreamlinesApp initialisation

    return true;
}
*/
/*!
 * Cleanup for StreamlinesApp
 */
/*
int StreamlinesApp::OnExit()
{    
////@begin StreamlinesApp cleanup
    return wxApp::OnExit();
////@end StreamlinesApp cleanup
}
*/

/*!
 * Streamlines type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( Streamlines, wxDialog )

/*!
 * Streamlines event table definition
 */

BEGIN_EVENT_TABLE( Streamlines, wxDialog )
////@begin Streamlines event table entries 
   EVT_RADIOBOX      (CURSOR_RBOX,                 Streamlines::_onDirection)
   EVT_RADIOBOX      (DIRECTION_RBOX,              Streamlines::_onDirection)
   EVT_RADIOBOX      (INTEGRATION_DIR_RBOX,        Streamlines::_onDirection)
   EVT_COMMAND_SCROLL(NUMBER_PTS_SLIDER,           Streamlines::_onnPointsSlider)
   EVT_COMMAND_SCROLL(PLANE_SIZE_SLIDER,           Streamlines::_onnPointsSlider)
   EVT_BUTTON        (ADVANCED_STREAMLINE_BUTTON,  Streamlines::_onAdvanced)
////@end Streamlines event table entries
END_EVENT_TABLE()

/*!
 * Streamlines constructors
 */

Streamlines::Streamlines(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Streamlines"), 
		wxDefaultPosition, wxDefaultSize, 
      (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
   this->SetSize( dialogPosition );

   xplorerPtr = VjObs::_duplicate( veEngine );
   domManager = domManagerIn;

   CreateControls();
}
/*
Streamlines::Streamlines( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * Streamlines creator
 */

bool Streamlines::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin Streamlines member initialisation
////@end Streamlines member initialisation

////@begin Streamlines creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end Streamlines creation
    return true;
}

/*!
 * Control creation for Streamlines
 */

void Streamlines::CreateControls()
{    
////@begin Streamlines content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 11:01:46 CDT (unregistered)

    Streamlines* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Streamline Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer4, 0, wxGROW|wxALL, 5);

    wxString itemRadioBox5Strings[] = {
        _("none"),
        _("point"),
        _("line"),
        _("plane")
    };
    wxRadioBox* itemRadioBox5 = new wxRadioBox( itemDialog1, ID_RADIOBOX, _("Cursor Selection"), wxDefaultPosition, wxDefaultSize, 4, itemRadioBox5Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(itemRadioBox5, 0, wxALIGN_TOP|wxALL, 5);

    wxString itemRadioBox6Strings[] = {
        _("x"),
        _("y"),
        _("z")
    };
    wxRadioBox* itemRadioBox6 = new wxRadioBox( itemDialog1, ID_RADIOBOX1, _("Direction"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox6Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(itemRadioBox6, 0, wxALIGN_TOP|wxALL, 5);

    wxString itemRadioBox7Strings[] = {
        _("backward"),
        _("forward"),
        _("both directions")
    };
    wxRadioBox* itemRadioBox7 = new wxRadioBox( itemDialog1, ID_RADIOBOX2, _("Integration Direction"), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox7Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(itemRadioBox7, 0, wxALIGN_TOP|wxALL, 5);

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _("Size(%)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText8, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider9 = new wxSlider( itemDialog1, ID_SLIDER, 50, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider9, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText10 = new wxStaticText( itemDialog1, wxID_STATIC, _("Number of Point (Per Plane Direction)"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText10, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider11 = new wxSlider( itemDialog1, ID_SLIDER1, 2, 2, 20, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider11, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxBoxSizer* itemBoxSizer12 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer12, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxButton* itemButton13 = new wxButton( itemDialog1, wxID_OK, _("Compute Streamline"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton13, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton14 = new wxButton( itemDialog1, ADVANCED_STREAMLINE_BUTTON, _("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer12->Add(itemButton14, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

////@end Streamlines content construction
}

/*!
 * Should we show tooltips?
 */

bool Streamlines::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap Streamlines::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Streamlines bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end Streamlines bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon Streamlines::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin Streamlines icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end Streamlines icon retrieval
}//////////////////////////////////////////////////////////
void Streamlines::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
{
   adStreamline = new AdvancedStreamlines(xplorerPtr, domManager);
   adStreamline->ShowModal();
std::cout<<"ADVANCEDSTREAMLINES WORKING"<<std::endl;
}
//////////////////////////////////////////////////////////
void Streamlines::_onCursorSelect(wxCommandEvent& WXUNUSED(event))
{

}
//////////////////////////////////////////////////////////
void Streamlines::_onDirection(wxCommandEvent& WXUNUSED(event))
{

}
//////////////////////////////////////////////////////////
void Streamlines::_onIntegrateDir(wxCommandEvent& WXUNUSED(event))
{

}
//////////////////////////////////////////////////////////
void Streamlines::_onnPointsSlider(wxScrollEvent& WXUNUSED(event))
{

}
