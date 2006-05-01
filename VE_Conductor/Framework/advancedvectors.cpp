/////////////////////////////////////////////////////////////////////////////
// Name:        advancedvectors.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 22:00:30 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////



////@begin includes
////@end includes

#include "VE_Conductor/Framework/advancedvectors.h"

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/checkbox.h>
#include <wx/icon.h>
////@begin XPM images
////@end XPM images

/*!
 * Application instance implementation
 */

////@begin implement app
//IMPLEMENT_APP( AdvancedVectorsApp )
////@end implement app

/*!
 * AdvancedVectorsApp type definition
 */

//IMPLEMENT_CLASS( AdvancedVectorsApp, wxApp )

/*!
 * AdvancedVectorsApp event table definition
 */
/*
BEGIN_EVENT_TABLE( AdvancedVectorsApp, wxApp )

////@begin AdvancedVectorsApp event table entries
////@end AdvancedVectorsApp event table entries

END_EVENT_TABLE()
*/
/*!
 * Constructor for AdvancedVectorsApp
 */
/*
AdvancedVectorsApp::AdvancedVectorsApp()
{
////@begin AdvancedVectorsApp member initialisation
////@end AdvancedVectorsApp member initialisation
}
*/
/*!
 * Initialisation for AdvancedVectorsApp
 */
/*
bool AdvancedVectorsApp::OnInit()
{    
////@begin AdvancedVectorsApp initialisation
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
////@end AdvancedVectorsApp initialisation

    return true;
}
*/
/*!
 * Cleanup for AdvancedVectorsApp
 */
/*
int AdvancedVectorsApp::OnExit()
{    
////@begin AdvancedVectorsApp cleanup
    return wxApp::OnExit();
////@end AdvancedVectorsApp cleanup
}
*/

/*!
 * AdvancedVectors type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( AdvancedVectors, wxDialog )

/*!
 * AdvancedVectors event table definition
 */

BEGIN_EVENT_TABLE( AdvancedVectors, wxDialog )

////@begin AdvancedVectors event table entries
////@end AdvancedVectors event table entries

END_EVENT_TABLE()

/*!
 * AdvancedVectors constructors
 */

AdvancedVectors::AdvancedVectors(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Advanced Vectors"), 
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
AdvancedVectors::AdvancedVectors( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * AdvancedVectors creator
 */

bool AdvancedVectors::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin AdvancedVectors member initialisation
////@end AdvancedVectors member initialisation

////@begin AdvancedVectors creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end AdvancedVectors creation
    return true;
}

/*!
 * Control creation for AdvancedVectors
 */

void AdvancedVectors::CreateControls()
{    
////@begin AdvancedVectors content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 10:39:13 CDT (unregistered)

    AdvancedVectors* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Advanced Vector Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer4Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Vector Threshold"));
    wxStaticBoxSizer* itemStaticBoxSizer4 = new wxStaticBoxSizer(itemStaticBoxSizer4Static, wxVERTICAL);
    itemStaticBoxSizer3->Add(itemStaticBoxSizer4, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText5 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Max%"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemStaticBoxSizer4->Add(itemStaticText5, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider6 = new wxSlider( itemDialog1, ID_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer4->Add(itemSlider6, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText7 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Min%"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer4->Add(itemStaticText7, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider8 = new wxSlider( itemDialog1, ID_SLIDER1, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer4->Add(itemSlider8, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Scale"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText9, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider10 = new wxSlider( itemDialog1, ID_SLIDER2, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider10, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer11, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Decrease Size"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer11->Add(itemStaticText12, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Increase Size\n"), wxDefaultPosition, wxSize(210, -1), wxALIGN_RIGHT );
    itemBoxSizer11->Add(itemStaticText13, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Vector Ratio"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText14, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider15 = new wxSlider( itemDialog1, ID_SLIDER3, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider15, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer16 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer16, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText17 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Dense"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer16->Add(itemStaticText17, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Sparse"), wxDefaultPosition, wxSize(260, -1), wxALIGN_RIGHT );
    itemBoxSizer16->Add(itemStaticText18, 0, wxALIGN_TOP|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxCheckBox* itemCheckBox19 = new wxCheckBox( itemDialog1, ID_CHECKBOX, _T("Scalar by Vector Magnitude"), wxDefaultPosition, wxDefaultSize, 0 );
    itemCheckBox19->SetValue(false);
    itemStaticBoxSizer3->Add(itemCheckBox19, 0, wxALIGN_LEFT|wxALL, 5);

////@end AdvancedVectors content construction
}

/*!
 * Should we show tooltips?
 */

bool AdvancedVectors::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap AdvancedVectors::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin AdvancedVectors bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end AdvancedVectors bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon AdvancedVectors::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin AdvancedVectors icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end AdvancedVectors icon retrieval
}
