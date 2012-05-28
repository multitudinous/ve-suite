/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for WMS server properties.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MINERVA_WMS_DIALOG_H_
#define _MINERVA_WMS_DIALOG_H_

#include <wx/dialog.h>

#include <string>
#include <ves/VEConfig.h>

class wxTextCtrl;
class wxStaticText;
class wxRadioButton;
class wxCheckBox;
class wxStdDialogButtonSizer;
class wxButton;

class VE_GUIPLUGINS_EXPORTS MinervaWmsDialog : public wxDialog
{
public:
    typedef wxDialog BaseClass;

    MinervaWmsDialog( wxWindow* parent, wxWindowID id );
    virtual ~MinervaWmsDialog();

    std::string server() ;
    std::string layers() ;
    std::string styles() ;
    std::string format() const;

private:

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    wxTextCtrl* _serverTextCtrl;
    wxTextCtrl* _layersTextCtrl;
    wxTextCtrl* _stylesTextCtrl;
    wxRadioButton* _jpegRadioBtn;
    wxRadioButton* _pngRadioBtn;
    wxRadioButton* _tiffRadioBtn;
    wxCheckBox* _transparentCheckBox;
    wxStdDialogButtonSizer* _sdbSizer;
    wxButton* _sdbSizerOK;
    wxButton* _sdbSizerCancel;
};

#endif // _MINERVA_WMS_DIALOG_H_
