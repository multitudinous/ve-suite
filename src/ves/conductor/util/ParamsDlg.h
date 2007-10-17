/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef PARAMSDLG_H
#define PARAMSDLG_H

#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/VEConfig.h>
#include <iostream>
#include <fstream>

#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>

#undef ParamsDlg_STYLE
#define ParamsDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxSTAY_ON_TOP | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS ParamsDlg : public wxDialog
{
	public:
		ParamsDlg(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("ParamsDialog"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = ParamsDlg_STYLE);
		virtual ~ParamsDlg();

		enum
		{
			ID_SETBUTTON = 1235,
			ID_WXMEMO3 = 1234,
			ID_PROMPTMEMO = 1233,
			ID_OPTIONSMEMO = 1232,
			ID_CHILDRENLABEL = 1231,
			ID_HASLABEL = 1230,
			ID_HASCHILDRENEDIT = 1229,
			ID_OABOX = 1228,
			ID_PORTTYPEEDIT = 1216,
			ID_PORTTYPELABEL = 1215,
			ID_MULTIPORTLABEL = 1214,
			ID_MULTIPORTEDIT = 1213,
			ID_GENDEREDIT = 1212,
			ID_GENDERLABEL = 1211,
			ID_INOROUTLABEL = 1210,
			ID_INOROUTEDIT = 1209,
			ID_FCPBOX = 1208,
			ID_TYPELABEL = 1116,
			ID_WXEDIT16 = 1112,
			ID_CSBOX = 1111,
			ID_DVALUELABEL = 1107,
			ID_LISTLABEL = 1106,
			ID_MEASURELABEL = 1105,
			ID_QUANTITYLABEL = 1104,
			ID_PROMPT = 1103,
			ID_DEFAULTVALUEEDIT = 1101,
			ID_DEFAULTLABEL = 1100,
			ID_LOWERLIMITEDIT = 1099,
			ID_LOWERLIMITLABEL = 1098,
			ID_UPPERLIMITLABEL = 1097,
			ID_UPPERLIMITEDIT = 1096,
			ID_ENTERABLEEDIT = 1095,
			ID_ENTERABLELABEL = 1094,
			ID_OUTPUTLABEL = 1093,
			ID_OUTPUTEDIT = 1092,
			ID_AVNBOX = 1046,
			ID_RECORDLABEL = 1045,
			ID_RECORDTYPEEDIT = 1044,
			ID_MARBOX = 1042,
			ID_OPTIONSLABEL = 1041,
			ID_OPTIONLABEL = 1033,
			ID_OPTIONLISTEDIT = 1032,
			ID_BASISLABEL = 1027,
			ID_BASISEDIT = 1026,
			ID_UNITLABEL = 1024,
			ID_UNITEDIT = 1023,
			ID_PHYSICALLABEL = 1018,
			ID_QUANTITY = 1017,
			ID_VALUEEDIT = 1012,
			ID_VALUELABEL = 1011,
			ID_ = 1009,
			ID_DIMENSIONLABEL = 1008,
			ID_DIMENSIONEDIT = 1007,
			ID_PARAMCHOICE = 1006,
			ID_NODEPATHLABEL = 1004,
			ID_NODEPATH = 1003,
			ID_PARAMETERLABEL = 1001,
			ID_DUMMY_VALUE_
		};

		void WxButton1Click(wxCommandEvent& event);
		void ParamChoiceSelected(wxCommandEvent& event );
		void SetButtonClick(wxCommandEvent& event);
		void AppendList(const char *);
		void SetCompName(const char *);
		void SetServiceList(ves::conductor::util::CORBAServiceList *);
		void SetDialogType(const char *);
		void SetIsBlock(bool);
	
	private:
		wxString CompName;
		wxString DialogType;
		ves::conductor::util::CORBAServiceList * serviceList;

		wxButton *SetButton;
		wxTextCtrl *WxMemo3;
		wxTextCtrl *PromptMemo;
		wxTextCtrl *OptionsMemo;
		wxStaticText *ChildrenLabel;
		wxStaticText *HasLabel;
		wxTextCtrl *HasChildrenEdit;
		wxStaticBox *OABox;
		wxTextCtrl *PortTypeEdit;
		wxStaticText *PortTypeLabel;
		wxStaticText *MultiportLabel;
		wxTextCtrl *MultiportEdit;
		wxTextCtrl *GenderEdit;
		wxStaticText *GenderLabel;
		wxStaticText *InOrOutLabel;
		wxTextCtrl *InOrOutEdit;
		wxStaticBox *FCPBox;
		wxStaticText *TypeLabel;
		wxTextCtrl *WxEdit16;
		wxStaticBox *CSBox;
		wxStaticText *DValueLabel;
		wxStaticText *ListLabel;
		wxStaticText *MeasureLabel;
		wxStaticText *QuantityLabel;
		wxStaticText *PromptLabel;
		wxTextCtrl *DefaultValueEdit;
		wxStaticText *DefaultLabel;
		wxTextCtrl *LowerLimitEdit;
		wxStaticText *LowerLimitLabel;
		wxStaticText *UpperLimitLabel;
		wxTextCtrl *UpperLimitEdit;
		wxTextCtrl *EnterableEdit;
		wxStaticText *EnterableLabel;
		wxStaticText *OutputLabel;
		wxTextCtrl *OutputEdit;
		wxStaticBox *AVNBox;
		wxStaticText *RecordLabel;
		wxTextCtrl *RecordTypeEdit;
		wxStaticBox *MARBox;
		wxStaticText *OptionsLabel;
		wxStaticText *OptionLabel;
		wxTextCtrl *OptionListEdit;
		wxStaticText *BasisLabel;
		wxTextCtrl *BasisEdit;
		wxStaticText *UnitLabel;
		wxTextCtrl *UnitEdit;
		wxStaticText *PhysicalLabel;
		wxTextCtrl *QuantityEdit;
		wxTextCtrl *ValueEdit;
		wxStaticText *ValueLabel;
		wxStaticBox *VARBox;
		wxStaticText *DimensionLabel;
		wxTextCtrl *DimensionEdit;
		wxChoice *ParamChoice;
		wxStaticText *NodePathLabel;
		wxTextCtrl *NodePath;
		wxStaticText *ParameterLabel;

		bool IsBlock;
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
      std::string ConvertUnicode( const wxChar* data )
      {
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
         return tempStr;
      }

		DECLARE_EVENT_TABLE();
};
}
}
}
#endif
