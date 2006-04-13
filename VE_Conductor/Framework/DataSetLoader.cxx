/////////////////////////////////////////////////////////////////////////////
// Name:        DataSetLoader.cxx
// Purpose:     
// Author:      
// Modified by: 
// Created:     11/04/2006 20:55:38
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma implementation "DataSetLoader.h"
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

#include "DataSetLoader.h"

////@begin XPM images

////@end XPM images

/*!
 * Application instance implementation
 */

////@begin implement app
IMPLEMENT_APP( DataSetLoader )
////@end implement app

/*!
 * DataSetLoader type definition
 */

IMPLEMENT_CLASS( DataSetLoader, wxApp )

/*!
 * DataSetLoader event table definition
 */

BEGIN_EVENT_TABLE( DataSetLoader, wxApp )

////@begin DataSetLoader event table entries
////@end DataSetLoader event table entries

END_EVENT_TABLE()

/*!
 * Constructor for DataSetLoader
 */

DataSetLoader::DataSetLoader()
{
////@begin DataSetLoader member initialisation
////@end DataSetLoader member initialisation
}

/*!
 * Initialisation for DataSetLoader
 */

bool DataSetLoader::OnInit()
{    
////@begin DataSetLoader initialisation
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
////@end DataSetLoader initialisation

    return true;
}

/*!
 * Cleanup for DataSetLoader
 */
int DataSetLoader::OnExit()
{    
////@begin DataSetLoader cleanup
    return wxApp::OnExit();
////@end DataSetLoader cleanup
}

