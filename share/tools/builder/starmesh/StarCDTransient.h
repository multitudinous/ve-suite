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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _STARCDTRANSLATOR_H_
#define _STARCDTRANSLATOR_H_

#include <wx/frame.h>

class wxButton;
class wxSizer;
class wxTextCtrl;
class wxRadioBox;
class wxStaticText;
class wxStaticBox;

enum StarCDTranslatorIDs
{
    FILE_NAME,
    FILE_NAME_BUTTON,
    UNITS_RADIOBOX,
    TIME_STEPS,
    BEGINNING_TIME,
    POST_FREQUENCY,
    INPUT_DIRECTORY,
    INPUT_DIRECTORY_BUTTON,
    OUTPUT_DIRECTORY,
    OUTPUT_DIRECTORY_BUTTON,
    TRANSLATE_BUTTON
};

class StarCDTranslator: public wxFrame
{    
public:
    StarCDTranslator( wxWindow* parent,
     		      wxWindowID id,
		      const wxString& title, 
		      const wxPoint& pos = wxDefaultPosition, 
		      const wxSize& size = wxDefaultSize, 
		      long style = wxDEFAULT_FRAME_STYLE );

    virtual ~StarCDTranslator();

    ///Creates the controls and sizers
    void BuildGUI();
    
private:
    wxTextCtrl* m_fileName;
    wxTextCtrl* m_inputDir;
    wxTextCtrl* m_outputDir;
    wxTextCtrl* m_timeSteps;
    wxTextCtrl* m_startTime;
    wxTextCtrl* m_frequency;
    
    wxButton* m_fileNameButton;
    wxButton* m_inputDirButton;
    wxButton* m_outputDirButton;  
    wxButton* m_translateButton;
    wxButton* m_closeButton;
    
    wxRadioBox* m_units;    
    
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    DECLARE_EVENT_TABLE()
};

#endif //_STARCDTRANSLATOR_H_
