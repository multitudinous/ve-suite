/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
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
 * File:          $RCSfile: mFixTranslator.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// File: mFixTranslator.h
// Author: Jeremy Jarrell
//		   jarrell@csee.wvu.edu
//         West Virginia Virtual Environments Lab
// Date: Spring 2004
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.
//

#include <wx/wx.h>
#include <wx/image.h>

// :dependencies
#include <wx/statline.h>

// writing dependencies
#include "UnstructuredGridWriter.h"
#include "mfixDataHeaders.h"
#include "converter.h"

#ifndef MFIXTRANSLATOR_H
#define MFIXTRANSLATOR_H


class mFixTranslator: public wxDialog {
public:
    // mFixTranslator::ids
    mFixTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:
    // mFixTranslator::methods
    void set_properties();
    void do_layout();

protected:
	// Structures for header files
	resHead resH;
	spHead spH;
	mfixData mfD;

    // mFixTranslator::attributes
    wxButton* FileOpenBttn;
    wxButton* DestBttn;
    wxButton* AboutBttn;
    wxStaticLine* static_line_4;

	wxStaticText* static_text_1;
    wxGauge* SPGauge_1;
	wxStaticText* static_text_1_TS;

	wxStaticText* static_text_2;
    wxGauge* SPGauge_2;
	wxStaticText* static_text_2_TS;
	
	wxStaticText* static_text_3;
    wxGauge* SPGauge_3;
	wxStaticText* static_text_3_TS;

	wxStaticText* static_text_4;
    wxGauge* SPGauge_4;
	wxStaticText* static_text_4_TS;
	
	wxStaticText* static_text_5;
    wxGauge* SPGauge_5;
	wxStaticText* static_text_5_TS;
	
	wxStaticText* static_text_6;
    wxGauge* SPGauge_6;
	wxStaticText* static_text_6_TS;
	
	wxStaticText* static_text_7;
    wxGauge* SPGauge_7;
	wxStaticText* static_text_7_TS;
	
	wxStaticText* static_text_8;
    wxGauge* SPGauge_8;
	wxStaticText* static_text_8_TS;
	
	wxStaticText* static_text_9;
    wxGauge* SPGauge_9;
	wxStaticText* static_text_9_TS;
	
	wxStaticText* static_text_a;
    wxGauge* SPGauge_a;
	wxStaticText* static_text_a_TS;
	
// pan
	wxStaticText* static_text_b;
    wxGauge* SPGauge_b;
	wxStaticText* static_text_b_TS;
	

    wxStaticLine* static_line_1;
	wxStaticText* statusTitle;
    wxTextCtrl* text_ctrl_statusReport;
	wxCheckBox* enableLoggingCheckbox;

	// Output file(s) checkbox group
	wxCheckBox* legacyVTKCheckbox;
	wxRadioBox* legacyVTKRadioBox;
	wxCheckBox* serialXMLCheckbox;
	wxCheckBox* parallelXMLCheckbox;
	wxCheckBox* cgnsCheckbox;
	wxStaticLine* static_line_6;
	wxButton* selectAllButton;
	wxButton* unselectAllButton;
	wxStaticLine* static_line_7;	
	wxStaticLine* static_line_8;
			
	// Options group
	wxStaticLine* static_line_11;
	wxCheckBox* exportGeometry;
	wxCheckBox* exportBoundingBox;
	wxStaticLine* static_line_options;
	wxButton* selectAllOptions;
	wxButton* unselectAllOptions;

	// Restart file information wxTextCtrl group
	wxStaticText* ST_RunName;
	wxTextCtrl* TC_RunName;
	wxStaticText* ST_RunDate;
	wxTextCtrl* TC_RunDate;
	wxStaticText* ST_RunTime;
	wxTextCtrl* TC_RunTime;
	wxStaticText* ST_VersionNum;
	wxTextCtrl* TC_VersionNum;
	wxStaticText* ST_CoordinateSystem;
	wxTextCtrl* TC_CoordinateSystem;
	wxStaticText* ST_GasSpecies;
	wxTextCtrl* TC_GasSpecies;
	wxStaticText* ST_SolidPhases;
	wxTextCtrl* TC_SolidPhases;
	wxStaticText* ST_NumScalars;
	wxTextCtrl* TC_NumScalars;
	wxStaticText* ST_NumReactionRates;
	wxTextCtrl* TC_NumReactionRates;
	wxStaticText* ST_IMax;
	wxTextCtrl* TC_IMax;	
	wxStaticText* ST_JMax;
	wxTextCtrl* TC_JMax;
	wxStaticText* ST_KMax;
	wxTextCtrl* TC_KMax;

	// Output variable(s) checkbox group
	wxCheckBox* VoidFractionVariable;				// Contained in sp1
	wxCheckBox* GasPressureVariable;				// Contained in sp2
	wxCheckBox* P_StarVariable;						// Contained in sp2
	wxCheckBox* GasVelocityVariable;				// Contained in sp3
	wxCheckBox* SolidPhaseVelocityVariable;			// Contained in sp4
	wxCheckBox* SolidPhaseDensityVariable;			// Contained in sp5
	wxCheckBox* TemperaturesVariable;				// Contained in sp6
	wxCheckBox* GasSpeciesVariable;					// Contained in sp7
	wxCheckBox* SolidPhaseSpeciesVariable;			// Contained in sp7
	wxCheckBox* Theta_MVariable;					// Contained in sp8
	wxCheckBox* ScalarsVariable;					// Contained in sp9
	wxCheckBox* NumReactionRatesVariable;			// Contained in spa
	wxCheckBox* KTURBG_Variable;			// Contained in spb // pan
	wxCheckBox* ETURBG_Variable;			// Contained in spb // pan
	wxStaticLine* static_line_9;
	wxButton* SelectAllOutputVariables;
	wxButton* UnselectAllOutputVariables;

	bool sp1Active;
	bool sp2Active;
	bool sp3Active; 
	bool sp4Active;
	bool sp5Active;
	bool sp6Active;
	bool sp7Active;
	bool sp8Active;
	bool sp9Active;
	bool spaActive;
	bool spbActive;  // pan

	wxTextCtrl* BeginTSBox;
	wxStaticText* BeginTSLabel;
	wxTextCtrl* EndTSBox;
	wxStaticText* EndTSLabel;
	wxStaticLine* static_line_10;
	wxCheckBox* EnableSpecificTSRange;
	int BeginTSVal;
	int EndTSVal;

    wxStaticLine* static_line_3;
	wxStaticText* TotalProgress;
    wxGauge* ProgressGauge;
	wxStaticText* outputFormatText;
    wxStaticLine* static_line_2;
	wxStaticLine* static_line_5;
    wxButton* GoBttn;
    wxButton* CancelBttn;

	// sp* File Flags
	bool sp1Exists;
	bool sp2Exists;
	bool sp3Exists;
	bool sp4Exists;
	bool sp5Exists;
	bool sp6Exists;
	bool sp7Exists;
	bool sp8Exists;
	bool sp9Exists;
	bool spaExists;
	bool spbExists;   // pan

	int sp1TimeSteps;
	int sp2TimeSteps;
	int sp3TimeSteps;
	int sp4TimeSteps;
	int sp5TimeSteps;
	int sp6TimeSteps;
	int sp7TimeSteps;
	int sp8TimeSteps;
	int sp9TimeSteps;
	int spaTimeSteps;
	int spbTimeSteps;  // pan
	int diffTS;
	int actualTS;
	int maxTS;

	// Output Flags
	bool legacyVTKOutput;
	bool serialXMLOutput;
	bool parallelXMLOutput;
	bool CGNSOutput;

	int formatTally;
	int formatCurrent;
	int totalTimeStepTally;

	wxFileDialog* OpenFileDialog;
	wxFileDialog* SaveFileDialog;

	char* inFilename;
	char Dir[255];

	wxString OutputDirectory;

	int GridWriterFormatFlag;
	
	// Added EVENT_TABLE
	DECLARE_EVENT_TABLE()

	enum EVENTS
	{
		BUTTON_FILE_OPEN,		
		BUTTON_DEST,		
		BUTTON_ABOUT,		
		BUTTON_GO,				
		BUTTON_CANCEL,			
		CHECKBOX_VTK,
		RADIOBOX_VTK,
		CHECKBOX_SERIAL_XML,	
		CHECKBOX_PARALLEL_XML,	
		CHECKBOX_CGNS,			
		BUTTON_SELECTALL,		
		BUTTON_UNSELECTALL,	
		BUTTON_SELECTALL_OPTIONS,
		BUTTON_UNSELECTALL_OPTIONS,
		CHECKBOX_VOIDFRACTION,
		CHECKBOX_GASPRESSURE,
		CHECKBOX_P_STAR,
		CHECKBOX_GASVELOCITY,
		CHECKBOX_SOLIDPHASEVELOCITY,
		CHECKBOX_SOLIDPHASEDENSITY,
		CHECKBOX_TEMPERATURE,
		CHECKBOX_GASSPECIES,
		CHECKBOX_SOLIDPHASESPECIES,
		CHECKBOX_THETA_M,
		CHECKBOX_SCALARS,
		CHECKBOX_NUMREACTIONRATES,
		CHECKBOX_K_TURB_G,			// pan ... two new variables in
		CHECKBOX_E_TURB_G,			// pan     the SPB file
		BUTTON_SELECTALLOUTPUTVARS,
		BUTTON_UNSELECTALLOUTPUTVARS,
		TEXTCTRL_BEGINNINGTS,
		TEXTCTRL_ENDINGTS,
		CHECKBOX_ENABLESPECIFICTSRANGE,
		GAUGE_SP1,				
		GAUGE_SP2,			
		GAUGE_SP3,			
		GAUGE_SP4,				
		GAUGE_SP5,				
		GAUGE_SP6,				
		GAUGE_SP7,			
		GAUGE_SP8,				
		GAUGE_SP9,				
		GAUGE_SP0,				
		GAUGE_SPa,				
		GAUGE_SPb,				// pan				
		RADIOBOX_SERIAL,		
		RADIOBOX_PARALLEL,		
		RADIOBOX_CGNS			
	};	

	// Added Event handler functions
	void FormatDate(int iDay, int iMonth, int iYear);
	void FormatTime(int iSecond, int iMinute, int iHour);
	void FlushGUI(void);
	void SetOutputVariableFlags(void);
	int DetermineMaxTimeSteps(void);
	void OnFileOpenButton(wxCommandEvent& event);
	void OnDestButton(wxCommandEvent& event);
	void OnAboutButton(wxCommandEvent& event);
	void OnGoButton(wxCommandEvent& event);
	void OnCancelButton(wxCommandEvent& event);
	void OnCheckboxLegacyVTK(wxCommandEvent& event);
	void OnRadioBoxLegacyVTK(wxCommandEvent& event);
	void OnCheckboxSerialXML(wxCommandEvent& event);
	void OnCheckboxParallelXML(wxCommandEvent& event);
	void OnCheckboxCGNS(wxCommandEvent& event);
	void OnButtonSelectAll(wxCommandEvent& event);
	void OnButtonUnselectAll(wxCommandEvent& event);
	//void OnButtonSelectAllOptions(wxCommandEvent& event);
	//void OnButtonUnselectAllOptions(wxCommandEvent& event);
	void OnSelectAllOutputVariablesButton(wxCommandEvent& event);
	void OnUnselectAllOutputVariablesButton(wxCommandEvent& event);
	void OnTextCtrlBeginningTS(wxCommandEvent& event);
	void OnTextCtrlEndingTS(wxCommandEvent& event);
	void OnCheckboxEnableSpecificTSRange(wxCommandEvent& event);
	void UpdateMaxTS(wxCommandEvent& event);
};


#endif // CODE_H
