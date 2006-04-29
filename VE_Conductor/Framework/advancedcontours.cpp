/////////////////////////////////////////////////////////////////////////////
// Name:        advancedcontours.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 20:30:15 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "advancedcontours.h"
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

#include "advancedcontours.h"

////@begin XPM images
////@end XPM images

/*!
 * Application instance implementation
 */

////@begin implement app
//IMPLEMENT_APP( AdvancedContoursApp )
////@end implement app

/*!
 * AdvancedContoursApp type definition
 */

//IMPLEMENT_CLASS( AdvancedContoursApp, wxApp )

/*!
 * AdvancedContoursApp event table definition
 */
/*
BEGIN_EVENT_TABLE( AdvancedContoursApp, wxApp )

////@begin AdvancedContoursApp event table entries
////@end AdvancedContoursApp event table entries

END_EVENT_TABLE()
*/
/*!
 * Constructor for AdvancedContoursApp
 */
/*
AdvancedContoursApp::AdvancedContoursApp()
{
////@begin AdvancedContoursApp member initialisation
////@end AdvancedContoursApp member initialisation
}
*/
/*!
 * Initialisation for AdvancedContoursApp
 */
/*
bool AdvancedContoursApp::OnInit()
{    
////@begin AdvancedContoursApp initialisation
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
////@end AdvancedContoursApp initialisation

    return true;
}
*/
/*!
 * Cleanup for AdvancedContoursApp
 */
/*
int AdvancedContoursApp::OnExit()
{    
////@begin AdvancedContoursApp cleanup
    return wxApp::OnExit();
////@end AdvancedContoursApp cleanup
}
*/

/*!
 * AdvancedContours type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( AdvancedContours, wxDialog )

/*!
 * AdvancedContours event table definition
 */

BEGIN_EVENT_TABLE( AdvancedContours, wxDialog )
////@begin AdvancedContours event table entries

////@end AdvancedContours event table entries
END_EVENT_TABLE()

/*!
 * AdvancedContours constructors
 */

AdvancedContours::AdvancedContours(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Advanced Contours"), 
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
AdvancedContours::AdvancedContours( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * AdvancedContours creator
 */

bool AdvancedContours::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin AdvancedContours member initialisation
////@end AdvancedContours member initialisation

////@begin AdvancedContours creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end AdvancedContours creation
    return true;
}

/*!
 * Control creation for AdvancedContours
 */

void AdvancedContours::CreateControls()
{    
////@begin AdvancedContours content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 10:36:58 CDT (unregistered)

    AdvancedContours* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Advanced Contour Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText4 = new wxStaticText( itemDialog1, wxID_STATIC, _("Contour Opacity"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText4, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider5 = new wxSlider( itemDialog1, ID_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider5, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer6 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer6, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText7 = new wxStaticText( itemDialog1, wxID_STATIC, _("Transparent"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer6->Add(itemStaticText7, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _("Opaque"), wxDefaultPosition, wxSize(215, -1), wxALIGN_RIGHT );
    itemBoxSizer6->Add(itemStaticText8, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _("Warped Contour Scale"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText9, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider10 = new wxSlider( itemDialog1, ID_SLIDER1, 50, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider10, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer11, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _("Lower"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer11->Add(itemStaticText12, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _("Higher"), wxDefaultPosition, wxSize(250, -1), wxALIGN_RIGHT );
    itemBoxSizer11->Add(itemStaticText13, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _("Contour LOD"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText14, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider15 = new wxSlider( itemDialog1, ID_SLIDER2, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider15, 0, wxGROW|wxLEFT|wxRIGHT, 5);

    wxBoxSizer* itemBoxSizer16 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer16, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticText* itemStaticText17 = new wxStaticText( itemDialog1, wxID_STATIC, _("Higher Detail"), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer16->Add(itemStaticText17, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

    wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _("Lower Detail"), wxDefaultPosition, wxSize(210, -1), wxALIGN_RIGHT );
    itemBoxSizer16->Add(itemStaticText18, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM|wxADJUST_MINSIZE, 5);

////@end AdvancedContours content construction
}

/*!
 * Should we show tooltips?
 */

bool AdvancedContours::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap AdvancedContours::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin AdvancedContours bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end AdvancedContours bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon AdvancedContours::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin AdvancedContours icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end AdvancedContours icon retrieval
}

