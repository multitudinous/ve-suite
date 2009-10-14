/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef WARRANTY_TOOL_UI_DIALOG_H
#define WARRANTY_TOOL_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/conductor/util/DualSlider.h>

#include "MachineInfoDlg.h"

namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
class wxSpinCtrlDbl;
}
}
}

// --- wxWidgets Includes --- //
class wxRadioBox;
class wxSlider;
class wxSpinCtrl;
class wxComboBox;
class wxTextCtrl;
class wxFixWidthImportCtrl;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace warrantytool
{
class WarrantyToolUIDialog : public MachineInfoDlg
{
public:
    WarrantyToolUIDialog();
    WarrantyToolUIDialog(
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    virtual ~WarrantyToolUIDialog();

    enum
    {
        GLOW_RESET,
        GLOW_CLEAR,
        GLOW_ADD,
        OPEN_WARRANTY_FILE,
        PART_SELECTION
    };

protected:

private:
    void StripCharacters( std::string& data, const std::string& character );

    void GetTextInput( wxCommandEvent& event );

    void BuildGUI();

    void SendCommandsToXplorer();

    void OpenWarrantyFile( wxCommandEvent& event );

    ves::conductor::util::CORBAServiceList* mServiceList;
    
    wxTextCtrl* mPartNumberEntry;
    wxFixWidthImportCtrl* mTabDialog;
    
    std::vector< std::string > mPartNumberList;
    ///PArt numbers loaded from the csv files
    std::vector< std::string > mLoadedPartNumbers;
    ///Description of part numbers loaded from csv files
    std::vector< std::string > mPartNumberDescriptions;
    wxComboBox* mPartListCMB;
private:
    wxArrayString m_partNumberStrings;
    wxArrayString m_columnStrings;
    
protected:
	// Handlers for MachineInfoDlg events.
	void OnDataLoad( wxFileDirPickerEvent& event );
	void OnVariableAndLogicalChoice( wxCommandEvent& event );
	void OnCreateInputText( wxCommandEvent& event );
	void OnTextQueryEnter( wxCommandEvent& event );
	void OnPartSelection( wxCommandEvent& event );
	void OnPartNumberEntry( wxCommandEvent& event );
    void OnTextChkListToggle( wxCommandEvent& event );
	void OnQueryApply( wxCommandEvent& event );
	void OnDialogCancel( wxCommandEvent& event );
	void OnQueryOK( wxCommandEvent& event );
    void OnToggleUnselected( wxCommandEvent& event );
    void OnClearData( wxCommandEvent& event );

    const std::string GetTextFromChoice( wxChoice* variable,
                                        wxChoice* logicOperator,
                                        wxTextCtrl* textInput );
    
    const std::string GetTextFromLogicOperator( wxChoice* logicOperator );
    
    void SubmitQueryCommand();
    
    void UpdateQueryDisplay();
    
    void ParseDataBase( const std::string& csvFilename );

    void ParseDataFile( const std::string& csvFilename );

    //DECLARE_EVENT_TABLE()
};

} //end warrantytool

#endif //WARRANTY_TOOL_UI_DIALOG_H
