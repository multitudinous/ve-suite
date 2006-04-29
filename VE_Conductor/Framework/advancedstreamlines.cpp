/////////////////////////////////////////////////////////////////////////////
// Name:        advancedstreamlines.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:59:27 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "advancedstreamlines.h"
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

#include "advancedstreamlines.h"

////@begin XPM images
////@end XPM images

/*!
 * Application instance implementation
 */

////@begin implement app
//IMPLEMENT_APP( AdvancedStreamlinesApp )
////@end implement app

/*!
 * AdvancedStreamlinesApp type definition
 */

//IMPLEMENT_CLASS( AdvancedStreamlinesApp, wxApp )

/*!
 * AdvancedStreamlinesApp event table definition
 */
/*
BEGIN_EVENT_TABLE( AdvancedStreamlinesApp, wxApp )

////@begin AdvancedStreamlinesApp event table entries
////@end AdvancedStreamlinesApp event table entries

END_EVENT_TABLE()
*/
/*!
 * Constructor for AdvancedStreamlinesApp
 */
/*
AdvancedStreamlinesApp::AdvancedStreamlinesApp()
{
////@begin AdvancedStreamlinesApp member initialisation
////@end AdvancedStreamlinesApp member initialisation
}
*/
/*!
 * Initialisation for AdvancedStreamlinesApp
 */
/*
bool AdvancedStreamlinesApp::OnInit()
{    
////@begin AdvancedStreamlinesApp initialisation
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
////@end AdvancedStreamlinesApp initialisation

    return true;
}
*/
/*!
 * Cleanup for AdvancedStreamlinesApp
 */
/*
int AdvancedStreamlinesApp::OnExit()
{    
////@begin AdvancedStreamlinesApp cleanup
    return wxApp::OnExit();
////@end AdvancedStreamlinesApp cleanup
}
*/

/*!
 * AdvancedStreamlines type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( AdvancedStreamlines, wxDialog )

/*!
 * AdvancedStreamlines event table definition
 */

BEGIN_EVENT_TABLE( AdvancedStreamlines, wxDialog )
////@begin AdvancedStreamlines event table entries
   EVT_BUTTON        ( PARTICLE_TRACKING_BUTTON, AdvancedStreamlines::_onParticleTrack)
   EVT_CHECKBOX      ( USE_SEED_POINT_CHK,       AdvancedStreamlines::_onCheck)
   EVT_CHECKBOX      ( ARROWS_CHK,               AdvancedStreamlines::OnArrowCheck )
   EVT_COMMAND_SCROLL( SPHERE_SIZE_SLIDER,       AdvancedStreamlines::onScaleSlider)
#ifdef WIN32
   EVT_COMMAND_SCROLL_ENDSCROLL(PROPOGATION_SLIDER,         AdvancedStreamlines::_onPropSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(INTEGRATION_STEP_SLIDER,    AdvancedStreamlines::_oniStepSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(STEP_SIZE_SLIDER,           AdvancedStreamlines::_onStepSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(LINE_DIAMETER_SLIDER,       AdvancedStreamlines::_onDiameterSlider)
#else
   EVT_COMMAND_SCROLL(PROPOGATION_SLIDER,         AdvancedStreamlines::_onPropSlider)
   EVT_COMMAND_SCROLL(INTEGRATION_STEP_SLIDER,    AdvancedStreamlines::_oniStepSlider)
   EVT_COMMAND_SCROLL(STEP_SIZE_SLIDER,           AdvancedStreamlines::_onStepSlider)
   EVT_COMMAND_SCROLL(LINE_DIAMETER_SLIDER,       AdvancedStreamlines::_onDiameterSlider)
#endif
////@end AdvancedStreamlines event table entries
END_EVENT_TABLE()

/*!
 * AdvancedStreamlines constructors
 */

AdvancedStreamlines::AdvancedStreamlines(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Advanced Streamlines"), 
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
AdvancedStreamlines::AdvancedStreamlines( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * AdvancedStreamlines creator
 */

bool AdvancedStreamlines::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin AdvancedStreamlines member initialisation
////@end AdvancedStreamlines member initialisation

////@begin AdvancedStreamlines creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end AdvancedStreamlines creation
    return true;
}

/*!
 * Control creation for AdvancedStreamlines
 */

void AdvancedStreamlines::CreateControls()
{    
////@begin AdvancedStreamlines content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 11:44:01 CDT (unregistered)

    AdvancedStreamlines* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Advanced Streamline Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText4 = new wxStaticText( itemDialog1, wxID_STATIC, _("Propagation (Total) Time"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText4, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider5 = new wxSlider( itemDialog1, ID_SLIDER, 100, 1, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider5, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer6 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer6, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText7 = new wxStaticText( itemDialog1, wxID_STATIC, _("Shorter"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer6->Add(itemStaticText7, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _("Longer"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer6->Add(itemStaticText8, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _("Integration Step"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText9, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider10 = new wxSlider( itemDialog1, ID_SLIDER1, 1000, 1, 5000, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider10, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer11, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _("Smaller"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT|wxSTATIC_BORDER );
    itemBoxSizer11->Add(itemStaticText12, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _("Larger"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer11->Add(itemStaticText13, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _("Step"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText14, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider15 = new wxSlider( itemDialog1, ID_SLIDER2, 1, 1, 5000, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider15, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer16 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer16, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText17 = new wxStaticText( itemDialog1, wxID_STATIC, _("Finer"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer16->Add(itemStaticText17, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _("Coarser"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer16->Add(itemStaticText18, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText19 = new wxStaticText( itemDialog1, wxID_STATIC, _("Sphere/Arrow/Particle Size(%)"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemStaticBoxSizer3->Add(itemStaticText19, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider20 = new wxSlider( itemDialog1, ID_SLIDER3, 50, 1, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider20, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText21 = new wxStaticText( itemDialog1, wxID_STATIC, _("Line Diameter"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText21, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider22 = new wxSlider( itemDialog1, ID_SLIDER4, 0, -100, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider22, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer23 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer23, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxStaticText* itemStaticText24 = new wxStaticText( itemDialog1, wxID_STATIC, _("Decrease Size"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer23->Add(itemStaticText24, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText25 = new wxStaticText( itemDialog1, wxID_STATIC, _("Increase Size"), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer23->Add(itemStaticText25, 1, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxBoxSizer* itemBoxSizer26 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer26, 0, wxGROW|wxALL, 5);

    wxCheckBox* itemCheckBox27 = new wxCheckBox( itemDialog1, ID_CHECKBOX, _("Use Last Seed Point"), wxDefaultPosition, wxDefaultSize, 0 );
    itemCheckBox27->SetValue(false);
    itemBoxSizer26->Add(itemCheckBox27, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxCheckBox* itemCheckBox28 = new wxCheckBox( itemDialog1, ID_CHECKBOX1, _("Stream Arrows"), wxDefaultPosition, wxDefaultSize, 0 );
    itemCheckBox28->SetValue(false);
    itemBoxSizer26->Add(itemCheckBox28, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton29 = new wxButton( itemDialog1, ID_BUTTON, _("Particle Tracking"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemButton29, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

////@end AdvancedStreamlines content construction
}

/*!
 * Should we show tooltips?
 */

bool AdvancedStreamlines::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap AdvancedStreamlines::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin AdvancedStreamlines bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end AdvancedStreamlines bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon AdvancedStreamlines::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin AdvancedStreamlines icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end AdvancedStreamlines icon retrieval
}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_onCheck( wxCommandEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::OnArrowCheck( wxCommandEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_oniStepSlider( wxScrollEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_onPropSlider( wxScrollEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_onStepSlider( wxScrollEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_onDiameterSlider( wxScrollEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::onScaleSlider( wxScrollEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////
void AdvancedStreamlines::_onParticleTrack( wxCommandEvent& WXUNUSED(event) )
{

}
