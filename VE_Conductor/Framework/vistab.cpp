/////////////////////////////////////////////////////////////////////////////
// Name:        vistab.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     17/04/2006 16:26:41
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks (unregistered), 17/04/2006 16:26:41

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "vistab.h"
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

#include "vistab.h"

////@begin XPM images
////@end XPM images

/*!
 * Vistab type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( Vistab, wxDialog )

/*!
 * Vistab event table definition
 */

BEGIN_EVENT_TABLE( Vistab, wxDialog )
////@begin Vistab event table entries
   EVT_TOOL     (CONTOUR_BUTTON,    Vistab::_onContour)
   EVT_TOOL     (VECTOR_BUTTON,     Vistab::_onVector)
   EVT_TOOL     (STREAMLINE_BUTTON, Vistab::_onStreamline)
////@end Vistab event table entries
END_EVENT_TABLE()

/*!
 * Vistab constructors
 */

Vistab::Vistab(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Visualization Tab"), 
      wxPoint(850,50), wxSize(400,400), 
      (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{

   xplorerPtr = VjObs::_duplicate( veEngine );
   domManager = domManagerIn;

   CreateControls();
}
/*
Vistab::Vistab( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * Vistab creator
 */

bool Vistab::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin Vistab member initialisation
////@end Vistab member initialisation

////@begin Vistab creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end Vistab creation
    return true;
}

/*!
 * Control creation for Vistab
 */

void Vistab::CreateControls()
{    
////@begin Vistab content construction
    // Generated by DialogBlocks, Wed 26 Apr 2006 18:52:50 CDT (unregistered)

    Vistab* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxToolBar* itemToolBar3 = new wxToolBar( itemDialog1, ID_TOOLBAR, wxDefaultPosition, wxSize(320, -1), wxTB_HORIZONTAL );
    itemToolBar3->SetToolSeparation(10);
    itemToolBar3->SetToolPacking(10);
    itemToolBar3->SetToolBitmapSize(wxSize(45, 30));
    wxBitmap itemtool4Bitmap(itemDialog1->GetBitmapResource(wxT("../../../../../../../home/users/jaredabo/GUIs/GUI/contour.png")));
    wxBitmap itemtool4BitmapDisabled;
    itemToolBar3->AddTool(CONTOUR_BUTTON, _T(""), itemtool4Bitmap, itemtool4BitmapDisabled, wxITEM_RADIO, _("Scalar Contours"), wxEmptyString);
    wxBitmap itemtool5Bitmap(itemDialog1->GetBitmapResource(wxT("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png")));
    wxBitmap itemtool5BitmapDisabled;
    itemToolBar3->AddTool(VECTOR_BUTTON, _T(""), itemtool5Bitmap, itemtool5BitmapDisabled, wxITEM_RADIO, _("Vectors"), wxEmptyString);
    wxBitmap itemtool6Bitmap(itemDialog1->GetBitmapResource(wxT("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png")));
    wxBitmap itemtool6BitmapDisabled;
    itemToolBar3->AddTool(STREAMLINE_BUTTON, _T(""), itemtool6Bitmap, itemtool6BitmapDisabled, wxITEM_RADIO, _("Streamlines"), wxEmptyString);
    wxBitmap itemtool7Bitmap(wxNullBitmap);
    wxBitmap itemtool7BitmapDisabled;
    itemToolBar3->AddTool(ID_TOOL3, _T(""), itemtool7Bitmap, itemtool7BitmapDisabled, wxITEM_RADIO, _("Isosurfaces"), wxEmptyString);
    wxBitmap itemtool8Bitmap(wxNullBitmap);
    wxBitmap itemtool8BitmapDisabled;
    itemToolBar3->AddTool(ID_TOOL4, _T(""), itemtool8Bitmap, itemtool8BitmapDisabled, wxITEM_RADIO, _("Polydata"), wxEmptyString);
    wxBitmap itemtool9Bitmap(wxNullBitmap);
    wxBitmap itemtool9BitmapDisabled;
    itemToolBar3->AddTool(ID_TOOL5, _T(""), itemtool9Bitmap, itemtool9BitmapDisabled, wxITEM_RADIO, _("Texture Based"), wxEmptyString);
    itemToolBar3->Realize();
    itemBoxSizer2->Add(itemToolBar3, 0, wxGROW|wxALL, 5);

    wxString itemComboBox10Strings[] = {
        _("DataSet1"),
        _("DataSet2"),
        _("DataSet3")
    };
    wxComboBox* itemComboBox10 = new wxComboBox( itemDialog1, ID_COMBOBOX, _T(""), wxDefaultPosition, wxSize(150, -1), 3, itemComboBox10Strings, wxCB_DROPDOWN );
    if (ShowToolTips())
        itemComboBox10->SetToolTip(_("Data Sets"));
    itemBoxSizer2->Add(itemComboBox10, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer11, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer12Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Scalars"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer12, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxString itemListBox13Strings[] = {
        _("DataSet1"),
        _("DataSet2"),
        _("DataSet3")
    };
    wxListBox* itemListBox13 = new wxListBox( itemDialog1, ID_LISTBOX, wxDefaultPosition, wxSize(125, -1), 3, itemListBox13Strings, wxLB_SINGLE );
    itemStaticBoxSizer12->Add(itemListBox13, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer14Static = new wxStaticBox(itemDialog1, wxID_ANY, _("Vectors"));
    wxStaticBoxSizer* itemStaticBoxSizer14 = new wxStaticBoxSizer(itemStaticBoxSizer14Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer14, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxString itemListBox15Strings[] = {
        _("DataSet1"),
        _("DataSet2"),
        _("DataSet3")
    };
    wxListBox* itemListBox15 = new wxListBox( itemDialog1, ID_LISTBOX1, wxDefaultPosition, wxSize(125, -1), 3, itemListBox15Strings, wxLB_SINGLE );
    itemStaticBoxSizer14->Add(itemListBox15, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxStaticText* itemStaticText16 = new wxStaticText( itemDialog1, wxID_STATIC, _("Max%"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    itemBoxSizer2->Add(itemStaticText16, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider17 = new wxSlider( itemDialog1, ID_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemBoxSizer2->Add(itemSlider17, 0, wxGROW|wxLEFT|wxRIGHT|wxTOP, 5);

    wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _("Min%"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
    itemBoxSizer2->Add(itemStaticText18, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider19 = new wxSlider( itemDialog1, ID_SLIDER1, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemBoxSizer2->Add(itemSlider19, 0, wxGROW|wxALL, 5);

    wxButton* itemButton20 = new wxButton( itemDialog1, ID_BUTTON, _("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer2->Add(itemButton20, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
///@end Vistab content construction
}

/*!
 * Should we show tooltips?
 */

bool Vistab::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap Vistab::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Vistab bitmap retrieval
    wxUnusedVar(name);
    if (name == _T("../../../../../../../home/users/jaredabo/GUIs/GUI/contour.png"))
    {
        wxBitmap bitmap(_T("../../../../../../../home/users/jaredabo/GUIs/GUI/contour.png"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }
    else if (name == _T("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png"))
    {
        wxBitmap bitmap(_T("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }
    else if (name == _T("../Deere/Deere/Pics/geom_with_baffle.PNG"))
    {
        wxBitmap bitmap(_T("../Deere/Deere/Pics/geom_with_baffle.PNG"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }
    return wxNullBitmap;
////@end Vistab bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon Vistab::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin Vistab icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end Vistab icon retrieval
}
////////////////////////////////////////////////////////////
void Vistab::SetCommInstance( VjObs_ptr veEngine )
{
   xplorerPtr = VjObs::_duplicate( veEngine );
}
////////////////////////////////////////////////////////////
void Vistab::_onContour( wxCommandEvent& WXUNUSED(event) )
{
   contour = new Contours(xplorerPtr, domManager);
   contour->ShowModal();
std::cout<<"CONTOURS WORKING"<<std::endl;   
}
////////////////////////////////////////////////////////////
void Vistab::_onVector( wxCommandEvent& WXUNUSED(event) )
{
   vector = new Vectors(xplorerPtr, domManager);
   vector->ShowModal();
std::cout<<"VECTORS WORKING"<<std::endl;
}
////////////////////////////////////////////////////////////
void Vistab::_onStreamline( wxCommandEvent& WXUNUSED(event) )
{
   streamline = new Streamlines(xplorerPtr, domManager);
   streamline->ShowModal();
std::cout<<"STREAMLINES WORKING"<<std::endl;
}
/*!
 * Vistab type definition
 */
/*
IMPLEMENT_DYNAMIC_CLASS( Vistab, wxDialog )
*/
/*!
 * Vistab event table definition
 */
/*
BEGIN_EVENT_TABLE( Vistab, wxDialog )

////@begin Vistab event table entries
////@end Vistab event table entries

END_EVENT_TABLE()
*/
/*!
 * Vistab constructors
 */
/*
Vistab::Vistab( )
{
}

Vistab::Vistab( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
}
*/
/*!
 * Vistab creator
 */
/*
bool Vistab::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin Vistab member initialisation
////@end Vistab member initialisation

////@begin Vistab creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end Vistab creation
    return true;
}
*/
/*!
 * Control creation for Vistab
 */
/*
void Vistab::CreateControls()
{    
////@begin Vistab content construction
    // Generated by DialogBlocks, 18/04/2006 14:28:15 (unregistered)

    Vistab* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

////@end Vistab content construction
}
*/
/*!
 * Should we show tooltips?
 */
/*
bool Vistab::ShowToolTips()
{
    return true;
}
*/
/*!
 * Get bitmap resources
 */
/*
wxBitmap Vistab::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Vistab bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end Vistab bitmap retrieval
}
*/
/*!
 * Get icon resources
 */
/*
wxIcon Vistab::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin Vistab icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end Vistab icon retrieval
}
*/
