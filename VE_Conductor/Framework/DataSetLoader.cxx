/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
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

